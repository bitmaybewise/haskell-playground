module Main where

import Control.Monad
import Marvel.ApiClient qualified as Marvel
import System.Environment

main :: IO ()
main = do
  -- cabal run play -- $MARVEL_PUBLIC_KEY $MARVEL_PRIVATE_KEY
  putStrLn "Welcome to the playground"
  args <- getArgs
  when (length args < 0) $ do
    putStrLn "Missing $MARVEL_PUBLIC_KEY and $MARVEL_PRIVATE_KEY!"
  let apikey = head args
      privatekey = args !! 1
  Marvel.series apikey privatekey >>= print
