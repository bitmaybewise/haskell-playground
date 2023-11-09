module Main where

import Data.Char (isPrint)

countNonPrintableCharacters :: String -> Int
countNonPrintableCharacters =
  length . filter (not . isPrint)

main :: IO ()
main = do
  print $ countNonPrintableCharacters "\v\t\aHello\r\n"
