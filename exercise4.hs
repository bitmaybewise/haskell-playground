higher [] = minBound::Int
higher (a:xs) | a > higher xs = a
              | otherwise = higher xs

lower [] = maxBound::Int
lower (a:xs) | a < lower xs = a
             | otherwise = lower xs

higherAndLower [] = (0,0)
higherAndLower xs = (higher xs, lower xs)

dic = [ 
  (0,"zero"), (1,"one"), (2,"two"),   (3,"three"), (4,"four"), 
  (5,"five"), (6,"six"), (7,"seven"), (8,"eight"), (9,"nine") 
]

getNumber (a,b) = a
getText   (a,b) = b

findText number (a:xs) | getNumber a == number = getText a
                       | otherwise = findText number xs

translateNumbers []     = []
translateNumbers (a:xs) = findText a dic : translateNumbers xs 

people = [ ("goku",25), ("bulma",30), ("gohan",5), ("kame", 70) ]

older (a,b) (x,y) | b > y     = (a,b)
                  | otherwise = (x,y)

olderFromList [a]      = a
olderFromList (a:b:xs) = olderFromList (older a b : xs)

newer (a,b) (x,y) | b < y     = (a,b)
                  | otherwise = (x,y)

newerFromList [a]      = a
newerFromList (a:b:xs) = newerFromList (newer a b : xs)

olderAndNewer = (olderFromList people, newerFromList people)

delPos :: [Int] -> Int -> [Int]
delPos (a:xs) n | n == 0 = (a:xs)
                | n == 1 = xs
                | otherwise = (a:(delPos xs (n - 1)))
