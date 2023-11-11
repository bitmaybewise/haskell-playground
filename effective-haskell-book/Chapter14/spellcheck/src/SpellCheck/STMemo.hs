module SpellCheck.STMemo where

import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Text (Text)
import Data.Text qualified as T
import SpellCheck.ListMemo (editDistance)

newtype MemoCache s = MemoCache (STRef s (Map (Text, Text) Int))

readCache :: MemoCache s -> Text -> Text -> ST s (Maybe Int)
readCache (MemoCache ref) stringA stringB =
  Map.lookup (stringA, stringB) <$> readSTRef ref

updateCache :: MemoCache s -> Text -> Text -> Int -> ST s ()
updateCache (MemoCache ref) stringA stringB distance =
  modifySTRef ref $ Map.insert (stringA, stringB) distance

newCache :: ST s (MemoCache s)
newCache = MemoCache <$> newSTRef Map.empty

editDistance :: MemoCache s -> Text -> Text -> ST s Int
editDistance cache = memoizedEditDistance
  where
    memoizedEditDistance stringA stringB = do
      result <- readCache cache stringA stringB
      case result of
        Just distance ->
          pure distance
        Nothing -> do
          newDistance <- findDistance stringA stringB
          updateCache cache stringA stringB newDistance
          pure newDistance

    findDistance stringA stringB
      | T.null stringA = pure $ T.length stringB
      | T.null stringB = pure $ T.length stringA
      | T.head stringA == T.head stringB =
          memoizedEditDistance restOfA restOfB
      | otherwise = do
          deleteCost <- memoizedEditDistance restOfA stringB
          insertCost <- memoizedEditDistance stringA restOfB
          swapCost <- memoizedEditDistance restOfA restOfB
          pure $ 1 + minimum [swapCost, deleteCost, insertCost]
      where
        restOfA = T.tail stringA
        restOfB = T.tail stringB

cacheSuffixDistances :: MemoCache s -> Text -> [Text] -> ST s ()
cacheSuffixDistances cache dictWord suffixes =
  traverse_ cacheSuffix suffixes
  where
    cacheSuffix suffix =
      updateCache cache dictWord (dictWord <> suffix) (T.length suffix)
