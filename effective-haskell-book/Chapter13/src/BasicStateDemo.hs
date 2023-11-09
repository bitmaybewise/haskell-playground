module BasicStateDemo where

import Control.Monad.State
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text

type Parser a = State Text (Either Text a)

data FullName = FullName
  { first :: Text,
    middle :: Text,
    last :: Text
  }
  deriving (Show)

takeUntil :: (Char -> Bool) -> Parser Text
takeUntil predicate = do
  oldState <- get
  let (nextVal, rest) = Text.break predicate oldState
  put rest
  pure (pure nextVal)

dropChar :: Parser ()
dropChar = do
  parseState <- get
  let newState = Text.tail parseState
  put newState
  pure (Right ())

word :: Parser Text
word = do
  nextWord <- takeUntil isSpace
  _ <- dropChar
  pure nextWord

parseFullName :: Parser FullName
parseFullName = do
  firstName <- word
  middleName <- word
  lastName <- word
  pure $ do
    firstName' <- firstName
    middleName' <- middleName
    lastName' <- lastName
    pure $ FullName firstName' middleName' lastName'

appendLineWithIndent :: String -> String -> State Int String
appendLineWithIndent message previousMessage = do
  indentLevel <- get
  let nextIndentLevel = indentLevel + 2
      indent = replicate nextIndentLevel ' '
      output = previousMessage <> indent <> message <> "\n"
  put nextIndentLevel
  pure output

appendLineDemo :: IO ()
appendLineDemo =
  putStrLn $ evalState message 0
  where
    message =
      appendLineWithIndent "hello" ""
        >>= appendLineWithIndent "world"
        >>= appendLineWithIndent "love,"
        >>= appendLineWithIndent "Fulano"
