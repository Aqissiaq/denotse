-- | Implementing \mathbb{F} from the paper

module Semantics where

import Syntax
import qualified Data.Map.Strict as Map
import Control.Monad ((>=>))

type Subst = Map.Map Var Expr

apply :: Subst -> Expr -> Expr
apply s = (>>= \x -> Map.findWithDefault (pure x) x s)

{-
Aside / sanity check: concrete execution
-}
type State = Map.Map Var Val

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
exec_concrete Skip v = Just v
exec_concrete (Asgn x e) v = do
  t <- eval v e
  pure (Map.insert x t v)
exec_concrete (Seq s1 s2) v = ((exec_concrete s1) >=> (exec_concrete s2)) v
exec_concrete (If e s1 s2) v = do
  guard <- eval v e
  if (guard == TBool True)
    then exec_concrete s1 v
    else exec_concrete s2 v
-- just allowing infinite looping
exec_concrete (While e s) v = do
  guard <- eval v e
  if (guard == TBool True)
    then exec_concrete (Seq s (While e s)) v
    else Just v
