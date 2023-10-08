{-# LANGUAGE OverloadedStrings #-}

module FailingStatefulParser where

import Control.Monad (when)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text

data FullName = FullName
  { first :: Text,
    middle :: Text,
    last :: Text
  }
  deriving (Show)

newtype Parser a = Parser {runParser :: State Text (Either String a)}

evalParser :: Parser a -> Text -> Either String a
evalParser = State.evalState . runParser

parse :: Parser a -> Text -> (Either String a, Text)
parse = State.runState . runParser

instance Functor Parser where
  fmap f parser = Parser $ (fmap . fmap) f (runParser parser)

instance Applicative Parser where
  pure a = Parser $ (pure . pure) a
  f <*> a = Parser $ do
    f' <- runParser f
    a' <- runParser a
    pure $ f' <*> a'

instance Monad Parser where
  return = pure
  a >>= f = Parser $ do
    val <- runParser a
    case val of
      Right val' -> runParser (f val')
      Left err -> pure (Left err)

parseError :: String -> Parser a
parseError errMsg = Parser $ pure (Left errMsg)

parseGet :: Parser Text
parseGet = Parser (Right <$> State.get)

parsePut :: Text -> Parser ()
parsePut newState = Parser $ Right <$> State.put newState

takeUntil :: (Char -> Bool) -> Parser Text
takeUntil predicate = do
  oldState <- parseGet
  let (nextVal, rest) = Text.break predicate oldState
  parsePut rest
  pure nextVal

optionally :: Parser () -> Parser ()
optionally originalParser = Parser $ do
  oldState <- State.get
  result <- runParser originalParser
  case result of
    Left _err -> State.put oldState
    _success -> pure ()
  pure $ Right ()

dropChar :: Parser ()
dropChar = do
  parseState <- parseGet
  case Text.uncons parseState of
    Nothing -> parseError "unexpected end of input"
    Just (_, rest) -> parsePut rest

word :: Parser Text
word = do
  nextWord <- takeUntil isSpace
  when (Text.null nextWord) $
    parseError "unexpected end of input"
  optionally dropChar
  pure nextWord

parseFullName :: Parser FullName
parseFullName = FullName <$> word <*> word <*> word

main :: IO ()
main = do
  putStrLn $ "Result: " <> show (fst result)
  putStrLn $ "Final state: " <> show (snd result)
  where
    updateState :: Parser FullName
    updateState = do
      name <- parseGet
      parsePut $ name <> "Fulano"
      name <- parseGet
      parsePut $ name <> " de"
      name <- parseGet
      parsePut $ name <> " Tal"
      parseFullName

    result :: (Either String FullName, Text)
    result = parse updateState ""
