{-# LANGUAGE BangPatterns #-}

module SpellCheck.LowLevelUnboxed where

import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.Text qualified as T
import Data.Text.Unsafe qualified as TU
import Data.Vector.Unboxed.Mutable qualified as MVec
import Prelude hiding (length, read, words)

{-# INLINE editDistance #-}
editDistance :: T.Text -> T.Text -> Int
editDistance stringA stringB = runST $ do
  let aLen = T.length stringA
      bLen = T.length stringB
      {-# INLINE lookupIndex #-}
      lookupIndex x y = (y * (aLen + 1)) + x
  cache <- MVec.new $ (aLen + 1) * (bLen + 1)
  for_ [0 .. aLen] $ \idx -> MVec.write cache (lookupIndex idx 0) idx
  for_ [0 .. bLen] $ \idx -> MVec.write cache (lookupIndex 0 idx) idx
  let columnCost !idxA !textIdxA
        | idxA > aLen = pure ()
        | otherwise = do
            let (TU.Iter !a' !textIdxA') = TU.iter stringA textIdxA
                {-# INLINE rowCost #-}
                rowCost !idxB !textIdxB
                  | idxB > bLen = pure ()
                  | otherwise = do
                      let (TU.Iter !b' !textIdxB') = TU.iter stringB textIdxB
                          cost = if a' == b' then 0 else 1
                      insertCost <-
                        (1 +)
                          <$> MVec.read cache (lookupIndex (idxA - 1) idxB)
                      deleteCost <-
                        (1 +)
                          <$> MVec.read cache (lookupIndex idxA (idxB - 1))
                      swapCost <-
                        (cost +)
                          <$> MVec.read cache (lookupIndex (idxA - 1) (idxB - 1))
                      let {-# INLINE newCost #-}
                          newCost = min swapCost $ min insertCost deleteCost
                      MVec.write cache (lookupIndex idxA idxB) newCost
                      rowCost (idxB + 1) (textIdxB + textIdxB')
            rowCost 1 0
            columnCost (idxA + 1) (textIdxA + textIdxA')
  columnCost 1 0
  MVec.read cache (lookupIndex aLen bLen)
