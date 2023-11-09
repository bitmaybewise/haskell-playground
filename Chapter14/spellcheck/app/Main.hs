module Main where

import Control.Monad (forM_)
import Data.Text (pack, split)
import SpellCheck (spellcheck)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  args <- getArgs
  rawDict <- readFile (head args)
  let dictionary = split (== '\n') (pack rawDict)
  forM_ (tail args) $ \path -> do
    content <- readFile path
    let fileWords = split (== ' ') $ pack content
        match = spellcheck dictionary 3 fileWords
    print match
