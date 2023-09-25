module Exercises where

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _    = []
zipWith' f _ []    = []
zipWith' f [a] [b] = f a b : []
zipWith' f as bs   = f (head as) (head bs) : zipWith' f (tail as) (tail bs)
