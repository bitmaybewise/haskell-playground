module SpellCheck where

import Data.Text qualified as T
import SpellCheck.Naive (editDistance)
import SpellCheck.Types (SuggestedMatch (SuggestedMatch))

spellcheckWord :: [T.Text] -> Int -> T.Text -> [SuggestedMatch]
spellcheckWord dictionary threshold word =
  getSuggestions dictionary []
  where
    getSuggestions [] suggestions = suggestions
    getSuggestions (dictWord : dict) suggestions
      | distance == 0 = []
      | distance > threshold = getSuggestions dict suggestions
      | otherwise = getSuggestions dict (suggestion : suggestions)
      where
        distance = editDistance dictWord word
        suggestion = SuggestedMatch dictWord word distance

spellcheck :: [T.Text] -> Int -> [T.Text] -> [SuggestedMatch]
spellcheck dictionary threshold =
  concatMap (spellcheckWord dictionary threshold)
