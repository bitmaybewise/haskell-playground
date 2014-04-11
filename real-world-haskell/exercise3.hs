import Data.List as List

data List a = Cons a (List a)
            | Nil
              deriving (Show)

data Tree a = Empty | Node a (Tree a) (Tree a)
             deriving (Show)

data TreeM a = NodeM a (Maybe (TreeM a)) (Maybe (TreeM a))
              deriving (Show)

fromList :: (List a) -> [a]
fromList Nil = []
fromList (Cons a as) = a:(fromList as)

leaf :: a -> Maybe (TreeM a)
leaf a = Just (NodeM a Nothing Nothing)

length' :: [a] -> Int
length' [] = 0
length' xs = 1 + length' (tail xs)

meanList :: [Int] -> Double
meanList [] = 0
meanList xs = fromIntegral (sumList xs) / fromIntegral (length' xs)
              where sumList [] = 0
                    sumList (x:xs) = x + sumList xs

palindrome :: [a] -> [a]
palindrome []  = []
palindrome [a] = [a]
palindrome xs  = xs ++ reverse' xs
                where reverse' []  = []
                      reverse' [a] = [a]
                      reverse' (x:xs) = reverse' xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == reverse xs

sortLists :: [[a]] -> [[a]]
sortLists [] = []
sortLists xs = List.sortBy (\x y -> if List.length x < List.length y
                                    then LT
                                    else GT) xs
intersperse :: a -> [[a]] -> [a]
intersperse _ []     = []
intersperce _ (x:[]) = x
intersperce s (x:xs) = x ++ [s] ++ (intersperce s xs)

heightTree :: Tree a -> Int
heightTree Empty = 0
heightTree (Node _ node1 node2) = 1 + heightTree node1 + heightTree node2
