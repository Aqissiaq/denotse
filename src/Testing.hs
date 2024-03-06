-- | Some utility stuff to test with

module Testing where
import Syntax
import Semantics
import qualified Data.Map.Strict as Map

test_abs, test_loop :: Stmt
test_abs = let x = (EVar "x") in
  If (EBinop Less x (ETrm (TInt 0))) (Asgn "x" (EUnop Neg x)) Skip

test_loop = let x = (EVar "x")
                y = (EVar "y") in
              While (EBinop Less x y) (Asgn "x" (EBinop Plus x (ETrm (TInt 2))))

empty, test :: Subst
empty = Map.empty
test =  Map.singleton "x" (ETrm (TInt 42))

test_state :: State
test_state = Map.fromList [("x", TInt (-42)), ("y", TInt 1)]
