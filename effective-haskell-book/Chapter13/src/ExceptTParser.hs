{-# LANGUAGE OverloadedStrings #-}

module ExceptTParser where

import Control.Monad (when)
import Control.Monad.State (MonadState (put), State, evalState, get)
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import ExceptT

type Parser a = ExceptT String (State Text) a

runParser :: Parser a -> Text -> Either String a
runParser = evalState . runExceptT

parseNextCharacter :: Parser Char
parseNextCharacter = do
  input <- succeed get
  when (Text.null input) $
    throwError "parseNextCharacter: unexpected end of input"
  succeed . put . Text.tail $ input
  pure $ Text.head input

word :: Parser Text
word = do
  nextWord <- takeUntil isSpace
  when (Text.null nextWord) $
    throwError "unexpected end of input"
  ignoreException dropChar
  pure nextWord
  where
    ignoreException = catchError (const $ pure ())

takeUntil :: (Char -> Bool) -> Parser Text
takeUntil predicate = do
  state <- succeed get
  let (nextVal, rest) = Text.break predicate state
  succeed . put $ rest
  pure nextVal

dropChar :: Parser ()
dropChar = do
  state <- succeed get
  case Text.uncons state of
    Nothing -> throwError "unexpected end of input"
    Just (_, rest) -> succeed . put $ rest
  pure ()

main :: IO ()
main = do
  putStrLn $ "Result: " <> result
  where
    updateState :: Parser Text
    updateState = do
      name <- succeed get
      succeed . put $ name <> "Fulano"
      name <- succeed get
      succeed . put $ name <> " de"
      name <- succeed get
      succeed . put $ name <> " Tal"
      succeed get

    result =
      let result' = runParser updateState ""
       in case result' of
            Left err -> show err
            Right val -> show val
