{-# LANGUAGE DeriveFunctor #-}
-- | Syntax for the While language
module Syntax where

-- a tiny finite type
-- data Var =
--   X | Y | Z | W
--         deriving (Eq, Show)
type Var = String

data Val = TInt Integer | TBool Bool
        deriving (Eq, Show)

data Unop = Not | Neg
        deriving (Eq, Show)

data Binop = And | Less | Equal | Plus
        deriving (Eq, Show)

data ExprF v = EVar v
  | ETrm Val
  | EUnop Unop (ExprF v)
  | EBinop Binop (ExprF v) (ExprF v)
 deriving (Eq, Show, Functor)

-- doing this a bit weird because defining applicative first is cumbersome
bind_expr :: ExprF a -> (a -> ExprF b) -> ExprF b
bind_expr (EVar x) f = f x
bind_expr (ETrm t) _ = (ETrm t)
bind_expr (EUnop op e) f = EUnop op (bind_expr e f)
bind_expr (EBinop op e1 e2) f = EBinop op (bind_expr e1 f) (bind_expr e2 f)

instance Applicative ExprF where
  pure = EVar
  fm <*> xm = bind_expr fm (\f -> bind_expr xm (pure . f))

instance Monad ExprF where
  (>>=) = bind_expr

type Expr = ExprF Var

data Stmt = Skip
        | Asgn Var Expr
        | Seq Stmt Stmt
        | If Expr Stmt Stmt
        | While Expr Stmt
        deriving (Eq, Show)
