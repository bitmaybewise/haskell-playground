{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassyArchiver where

import Control.Applicative
import Control.Monad (void, when)
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isSeparator)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Enc

data Archive = Archive
  { archiveName :: Text,
    archiveFiles :: [ArchivedFile]
  }
  deriving stock (Show)

data ArchivedFile = ArchivedFile
  { archivedFileName :: Text,
    archivedFileContents :: ByteString
  }
  deriving stock (Show)

newtype Archiver a = Archiver
  {unArchiver :: StateT Text (ExceptT Text IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadState Text,
      MonadError Text,
      MonadIO
    )

runArchiver :: Text -> Archiver a -> IO (Either Text a)
runArchiver inputText archiver =
  runExceptT $ evalStateT (unArchiver archiver) inputText

parseChar :: Archiver Char
parseChar = do
  parseText <- Archiver get
  case Text.uncons parseText of
    Nothing ->
      throwError "end of input"
    Just (c, rest) -> do
      put rest
      pure c

dropSpaces :: Archiver ()
dropSpaces = void $ many (expectChar ' ')

isNewLine :: Char -> Bool
isNewLine = (== '\n')

expect :: (Eq a) => Archiver a -> a -> Archiver ()
expect getActual expected = do
  actual <- getActual
  when (expected /= actual) $ do
    throwError "expectation violated"

expectChar :: Char -> Archiver ()
expectChar = expect parseChar

expectText :: Text -> Archiver ()
expectText expected = do
  stripped <- gets (Text.stripPrefix expected)
  case stripped of
    Nothing -> throwError "missing expected string"
    Just rest -> put rest

takeUntil :: (Char -> Bool) -> Archiver Text
takeUntil predicate = do
  (result, rest) <- Text.break predicate <$> get
  put rest
  pure result

word :: Archiver Text
word = do
  nextWord <- takeUntil (\s -> isSeparator s || isNewLine s)
  void . optional $ expectChar '\n' <|> dropSpaces
  when (Text.null nextWord) $
    throwError "end of input"
  pure nextWord

quotedString :: Archiver Text
quotedString = do
  expectChar '"'
  quotedText <- takeUntil (== '"')
  expectChar '"'
  pure quotedText

restOfLine :: Archiver Text
restOfLine = remainderOfLine <|> remainderOfText
  where
    remainderOfLine = do
      txt <- takeUntil isNewLine
      expectChar '\n'
      pure txt
    remainderOfText = get

dropEmptyLines :: Archiver ()
dropEmptyLines =
  void $ many $ dropSpaces >> expectChar '\n'

parseIndentedLine :: Int -> Archiver Text
parseIndentedLine indentLevel = do
  expectText $ Text.replicate indentLevel " "
  restOfLine

runSubparser :: Archiver a -> Text -> Archiver a
runSubparser action subparserState = do
  oldText <- get
  put subparserState
  result <- action
  put oldText
  pure result

parseBlock :: Archiver a -> Archiver a
parseBlock blockParser =
  dropEmptyLines >> getBlock >>= runSubparser blockParser
  where
    getBlock = do
      firstLineSpacing <- takeUntil (not . isSeparator)
      let indentation = Text.length firstLineSpacing
      firstLine <- restOfLine
      restOfBlock <- many (dropEmptyLines >> parseIndentedLine indentation)
      pure $ Text.unlines (firstLine : restOfBlock)

parseImportStatement :: Archiver ArchivedFile
parseImportStatement = do
  expectText "import"
  dropSpaces
  path <- quotedString
  dropSpaces
  expectChar '\n'
  contents <- liftIO $ ByteString.readFile (Text.unpack path)
  pure $ ArchivedFile path contents

parseNewFileStatement :: Archiver ArchivedFile
parseNewFileStatement = do
  expectText "new-file"
  dropSpaces
  path <- quotedString
  dropSpaces
  expectText ":\n"
  body <- Enc.encodeUtf8 <$> parseBlock get
  pure $ ArchivedFile path body

parseArchiveStatements :: Archiver [ArchivedFile]
parseArchiveStatements =
  many $ dropEmptyLines >> (parseImportStatement <|> parseNewFileStatement)

parseArchive :: Archiver Archive
parseArchive = do
  expectText "archive"
  dropSpaces
  archiveName <- quotedString
  expectText ":\n"
  files <- parseBlock parseArchiveStatements
  pure $ Archive archiveName files
