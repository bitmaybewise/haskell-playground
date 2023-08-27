module HaskellBook.Examples
  ( module HaskellBook.Examples.UserInfo,
    module CreatingModules,
    SortedList.SortedList (..),
    SortedList.makeSortedList,
    SortedList.minimum,
  )
where

import HaskellBook.Examples.Introduction.CreatingModules as CreatingModules hiding (testMessage)
import qualified HaskellBook.Examples.SortedList as SortedList
import HaskellBook.Examples.UserInfo
