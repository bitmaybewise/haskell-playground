{-# LANGUAGE OverloadedStrings #-}

module SpellCheck where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Text qualified as T
import SpellCheck.LowLevelUnboxed
import SpellCheck.STMemo (MemoCache, cacheSuffixDistances, newCache)
import SpellCheck.Types (SuggestedMatch (SuggestedMatch))

spellcheckWord :: MemoCache s -> [T.Text] -> Int -> T.Text -> ST s [SuggestedMatch]
spellcheckWord cache dictionary threshold word =
  foldM getSuggestions [] dictionary
  where
    getSuggestions suggestions dictWord = do
      cacheSuffixDistances cache dictWord ["s", "es", "'s", "ed", "ing"]
      let distance = editDistance dictWord word
          suggestion = SuggestedMatch dictWord word distance
      if distance > 0 && distance <= threshold
        then pure (suggestion : suggestions)
        else pure suggestions

spellcheck :: [T.Text] -> Int -> [T.Text] -> [SuggestedMatch]
spellcheck dictionary threshold words = runST $ do
  cache <- newCache
  concat <$> traverse (spellcheckWord cache dictionary threshold) words
