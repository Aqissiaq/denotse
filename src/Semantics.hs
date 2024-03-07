-- | Implementing \mathbb{F} from the paper
{-# LANGUAGE FlexibleInstances #-}

module Semantics where

import Syntax
import qualified Data.Map.Strict as Map
import Control.Monad ((>=>), join)
import Data.List (intersect, union, (\\))

type State = Map.Map Var Val

-- needed for unions, but pieces are uniquely identified by their partition
-- so we equate all state transformers
instance (Eq (State -> Maybe State)) where
  _ == _ = True

{- Concrete Execution -}
unop :: Unop -> Val -> Maybe Val
unop Not (TBool b) = Just $ TBool (not b)
unop Neg (TInt n) = Just $ TInt (-n)
unop _ _ = Nothing

binop :: Binop -> Val -> Val -> Maybe Val
binop  And (TBool b1) (TBool b2) = Just $ TBool (b1 && b2)
binop  Less (TInt n) (TInt m) = Just $ TBool (n < m)
binop  Equal (TInt n) (TInt m) = Just $ TBool (n == m)
binop  Plus (TInt n) (TInt m) = Just $ TInt (n + m)
binop _ _ _ = Nothing

eval :: State -> Expr -> Maybe Val
eval s (EVar x) = s Map.!? x
eval _ (ETrm t) = Just t
eval s (EUnop op e) = eval s e >>= unop op
eval s (EBinop op e1 e2) = do
  e1' <- eval s e1
  e2'<- eval s e2
  binop op e1' e2'

-- the paper's "little f"
exec_concrete :: Stmt -> State -> Maybe State
exec_concrete Skip = pure
exec_concrete (Asgn x e) = \v -> do
  t <- eval v e
  pure (Map.insert x t v)
exec_concrete (Seq s1 s2) =
  exec_concrete s1 >=> exec_concrete s2
exec_concrete (If e s1 s2) = \v -> do
  guard <- eval v e
  if (guard == TBool True)
    then exec_concrete s1 v
    else exec_concrete s2 v
-- just allowing infinite looping for now
exec_concrete (While e s) = \v ->do
  guard <- eval v e
  if (guard == TBool True)
    then exec_concrete (Seq s (While e s)) v
    else Just v

{- "Symbolic Execution": \mathbb{F} -}
type StateSet = [State]

-- violently finitized
allStates :: StateSet
allStates = [Map.fromList [("x", vx), ("y", vy)] | vx <- vals, vy <- vals]
    where vals = [TInt n | n <- [-10,-9..10]] ++ [TBool True, TBool False]

denot_b :: Expr -> StateSet
denot_b b = filter (\s -> eval s b == Just (TBool True)) allStates

preimage :: (State -> Maybe State) -> StateSet -> StateSet
preimage f states = [s | s <- allStates, f s `elem` (map Just states)]

complement :: StateSet -> StateSet
complement s = allStates \\ s

nFold :: Integer -> (a -> a) -> (a -> a)
nFold 0 _ = id
nFold n f = f . nFold (n-1) f

big_F_aux :: Stmt -> Integer -> [(State -> Maybe State, StateSet)]
big_F_aux Skip _ =
  [(pure, allStates)]
big_F_aux (Asgn x e) _ =
  [(\v -> eval v e >>= \t -> pure (Map.insert x t v), allStates)]
big_F_aux (Seq s1 s2) depth  =
  [(f1 >=> f2, b1 `intersect` preimage f1 b2) | (f1, b1) <- (big_F_aux s1 depth ), (f2, b2) <- (big_F_aux s2 depth)]
big_F_aux (If b s1 s2) depth =
  [(f1, b1 `intersect` denot_b b) | (f1, b1) <- big_F_aux s1 depth]
  `union` [(f2, b2 `intersect` complement (denot_b b)) | (f2, b2) <- big_F_aux s2 depth]
big_F_aux (While b0 p0) depth =
  nFold depth (loop b0 p0) [(pure, complement (denot_b b0))]
  where
    loop :: Expr -> Stmt -> [(State -> Maybe State, StateSet)] -> [(State -> Maybe State, StateSet)]
    loop b p pieces = [(f1 >=> fp, denot_b b `intersect` bp `intersect` preimage fp b1)
                            | (f1, b1) <- pieces, (fp, bp) <- big_F_aux p depth] `union` pieces

-- \mathcal{F}
big_F :: Stmt -> [(State -> Maybe State, StateSet)]
big_F p = concatMap (big_F_aux p) [0..]

exec_symb :: Stmt -> State -> [Maybe State]
exec_symb p v = [f v | (f, b) <- big_F p, v `elem` b]

-- not necessary (yet), just nice
-- incidentally, this is the first definition in section 6.1 of the "for free" paper
type Subst = Map.Map Var Expr

lift_subst :: Subst -> Var -> Expr
lift_subst s x = Map.findWithDefault (pure x) x s

apply :: Subst -> Expr -> Expr
apply s = join . fmap (lift_subst s)
