module EitherBifunctor where

import Data.Bifunctor

newtype EitherBifunctor l r = EitherBifunctor (Either l r) deriving (Show)

instance Functor (EitherBifunctor l) where
  fmap f (EitherBifunctor either) =
    EitherBifunctor $ fmap f either

instance Bifunctor EitherBifunctor where
  bimap f g (EitherBifunctor either) =
    case either of
      Left a -> EitherBifunctor (Left $ f a)
      Right b -> EitherBifunctor (Right $ g b)
