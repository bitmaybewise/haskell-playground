module Nullable where

import Prelude hiding (null)

class (Eq a) => Nullable a where
  null :: a

  isNull :: a -> Bool
  isNull a = a == null

instance (Eq a) => Nullable (Maybe a) where
  null = Nothing

  isNull Nothing = True
  isNull _ = False

instance (Nullable a, Nullable b) => Nullable (a, b) where
  null = (null, null)

  isNull (a, b) = isNull a && isNull b

instance (Eq a) => Nullable [a] where
  null = []

  isNull [] = True
  isNull _ = False
