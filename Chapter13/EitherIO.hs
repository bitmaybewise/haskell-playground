module EitherIO where

newtype EitherIO a = EitherIO
  {runIO :: IO (Either String a)}

instance Functor EitherIO where
  fmap f exceptionalIO =
    EitherIO $ (fmap . fmap) f (runIO exceptionalIO)

instance Applicative EitherIO where
  pure a = EitherIO $ (pure . pure) a
  f <*> a = EitherIO $ do
    f' <- runIO f
    a' <- runIO a
    pure $ f' <*> a'

instance Monad EitherIO where
  return = pure
  (>>=) :: EitherIO a -> (a -> EitherIO b) -> EitherIO b
  a >>= f = EitherIO $ do
    val <- runIO a
    case val of
      Left err -> pure (Left err)
      Right val' -> runIO $ f val'
