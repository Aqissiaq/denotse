module Main where

import Testing
import Semantics

main :: IO ()
main = do
  putStrLn $ show test_abs
  putStrLn $ show (exec_concrete test_abs test_state)
  putStrLn $ show (exec_symb test_abs test_state)
  putStrLn $ show test_loop
  putStrLn $ show (exec_concrete test_loop test_state)
  putStrLn $ show (exec_symb test_loop test_state)
  -- the infinite (and unsound) list of increasing loop depth
  -- putStrLn $ show (take 3 $ [b | (_, b) <- big_F test_loop])
