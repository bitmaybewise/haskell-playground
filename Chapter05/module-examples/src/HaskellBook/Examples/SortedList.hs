module HaskellBook.Examples.SortedList
  ( SortedList (getSortedList),
    makeSortedList,
    minimum,
  )
where

import Data.List (sort)
import Prelude hiding (minimum)

data SortedList = SortedList {getSortedList :: [Int]}

minimum :: SortedList -> Int
minimum (SortedList numbers) = head numbers

makeSortedList :: [Int] -> Maybe SortedList
makeSortedList [] = Nothing
makeSortedList numbers = Just $ SortedList (sort numbers)
