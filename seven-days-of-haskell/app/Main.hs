module Main where

import Lib (run)
import System.Environment (getEnv)

main :: IO ()
main = do
  tmdbToken <- getEnv "TMDB_API_READ_ACCESS_TOKEN"
  marvelApikey <- getEnv "MARVEL_PUBLIC_KEY"
  marvelPrivatekey <- getEnv "MARVEL_PRIVATE_KEY"
  run tmdbToken marvelApikey marvelPrivatekey
