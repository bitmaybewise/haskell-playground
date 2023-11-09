module ExceptT where

import Data.Kind (Type)

newtype ExceptT (e :: Type) (m :: Type -> Type) (a :: Type) = ExceptT {runExceptT :: m (Either e a)}

instance (Functor m) => Functor (ExceptT e m) where
  fmap f a = ExceptT $ (fmap . fmap) f (runExceptT a)

instance (Monad m) => Applicative (ExceptT e m) where
  pure a = ExceptT $ pure (pure a)
  f <*> a = ExceptT $ do
    f' <- runExceptT f
    a' <- runExceptT a
    pure $ f' <*> a'

instance (Monad m) => Monad (ExceptT e m) where
  return = pure
  a >>= f = ExceptT $ do
    val <- runExceptT a
    case val of
      Left err -> pure $ Left err
      Right val' -> runExceptT $ f val'

throwError :: (Monad m) => e -> ExceptT e m a
throwError exception = ExceptT (pure $ Left exception)

catchError :: (Monad m) => (e -> ExceptT e m a) -> ExceptT e m a -> ExceptT e m a
catchError handler action = ExceptT $ do
  result <- runExceptT action
  case result of
    Left err -> runExceptT (handler err)
    Right val -> pure (Right val)

succeed :: (Monad m) => m a -> ExceptT e m a
succeed a = ExceptT (Right <$> a)
