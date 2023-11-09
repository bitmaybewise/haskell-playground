{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module MonadStateDemo where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State
import Control.Monad.State qualified as StateT

newtype GuessingGame a = GuessingGame {runGame :: ExceptT String (StateT String IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadState String)

evalGame :: String -> GuessingGame a -> IO (Either String a)
evalGame input =
  flip StateT.evalStateT input . runExceptT . runGame

guessTheState :: Int -> GuessingGame Bool
guessTheState guess = do
  answer <- length <$> get @String
  pure $ guess == answer
