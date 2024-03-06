module Main where

import Testing
import Semantics

main :: IO ()
main = do
  putStrLn $ show test_abs
  putStrLn $ show (exec_concrete test_abs test_state)
  putStrLn $ show test_loop
  putStrLn $ show (exec_concrete test_loop test_state)
