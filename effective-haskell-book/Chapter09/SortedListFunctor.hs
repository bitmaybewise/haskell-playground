{-# LANGUAGE DerivingStrategies #-}

module SortedListFunctor (SortedList, insertSorted) where

data SortedList a = Empty | Cons a (SortedList a)
  deriving stock (Eq, Show)

insertSorted :: (Ord t) => t -> SortedList t -> SortedList t
insertSorted a Empty = Cons a Empty
insertSorted a (Cons b bs)
  | a >= b = Cons b (insertSorted a bs)
  | otherwise = Cons a (Cons b bs)

instance Functor SortedList where
  fmap _ Empty = Empty
  fmap f (Cons a sortedList) = Cons (f a) (fmap f sortedList)

isIdentityCompliant :: Bool
isIdentityCompliant =
  let a = insertSorted 3 Empty
      b = insertSorted 2 a
      c = insertSorted 1 b
   in fmap id c == c

isCompositionCompliant :: Bool
isCompositionCompliant =
  let a = insertSorted 3 Empty
      b = insertSorted 2 a
      c = insertSorted 1 b
      plus1 = (+ 1)
   in fmap (show . plus1) c == (fmap show . fmap plus1 $ c)
