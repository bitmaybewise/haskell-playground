module Main where

import MyLib (spawningProcess)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  spawningProcess $ head args
