{-# LANGUAGE RecordWildCards #-}

module SpellCheck.Types where

import Data.Text (Text)
import Text.Printf (printf)

data SuggestedMatch = SuggestedMatch
  { matchWord :: Text,
    matchSearchedWord :: Text,
    matchDistance :: Int
  }
  deriving (Show, Eq)

showSuggestedMatch :: SuggestedMatch -> String
showSuggestedMatch SuggestedMatch {..} =
  printf "%s -> %s: %d" matchWord matchSearchedWord matchDistance
