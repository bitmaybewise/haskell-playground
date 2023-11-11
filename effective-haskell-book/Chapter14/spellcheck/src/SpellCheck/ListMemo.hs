module SpellCheck.ListMemo where

import Data.Text qualified as T

editDistance :: T.Text -> T.Text -> Int
editDistance stringA stringB =
  getEditDistance 0 0
  where
    aLen = T.length stringA
    bLen = T.length stringB
    distances =
      map (\idxA -> map (getEditDistance idxA) [0 .. bLen]) [0 .. aLen]
    lookupEditDistance idxA idxB =
      distances !! idxA !! idxB
    getEditDistance idxA idxB
      | idxA == aLen = bLen - idxB
      | idxB == bLen = aLen - idxA
      | stringA `T.index` idxA == stringB `T.index` idxB =
          lookupEditDistance (idxA + 1) (idxB + 1)
      | otherwise =
          let deleteCost = lookupEditDistance (idxA + 1) idxB
              insertCost = lookupEditDistance idxA (idxB + 1)
              swapCost = lookupEditDistance (idxA + 1) (idxB + 1)
           in 1 + minimum [deleteCost, insertCost, swapCost]
