module FoldExamples where
import Prelude hiding (foldl, foldr)

foldl func carryValue lst =
    if null lst
    then carryValue
    else foldl func (func carryValue (head lst)) (tail lst)

foldr func carryValue lst =
    if null lst
    then carryValue
    else func (head lst) $ foldr func carryValue (tail lst)
