module Main where

import System.Environment (getEnv)
import TMDB (runTMBD)

main :: IO ()
main = do
  tmdbToken <- getEnv "TMDB_API_READ_ACCESS_TOKEN"
  runTMBD tmdbToken
