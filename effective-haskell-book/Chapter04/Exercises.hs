module Exercises where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
    deriving (Show)

showStringTree :: BinaryTree String -> String
showStringTree Leaf = "<>"
showStringTree (Branch binTreeL value binTreeR) = 
    showStringTree binTreeL ++ value ++ showStringTree binTreeR

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree bt n =
    case bt of
        Branch Leaf val right -> Branch (Branch Leaf val Leaf) n right
        Branch left val Leaf -> Branch left n (Branch Leaf val Leaf)
        _ -> Branch bt n Leaf

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False
doesIntExist (Branch left val right) n =
    val == n || doesIntExist left n || doesIntExist right n
