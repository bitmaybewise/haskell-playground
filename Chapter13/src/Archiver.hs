{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Archiver where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text

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
  deriving newtype (Functor, Applicative, Monad, Alternative)

runArchiver :: Text -> Archiver a -> IO (Either Text a)
runArchiver inputText archiver =
  runExceptT $ evalStateT (unArchiver archiver) inputText

parseChar :: Archiver Char
parseChar = do
  parseText <- Archiver get
  case Text.uncons parseText of
    Nothing ->
      Archiver . lift . throwError $ "end of input"
    Just (c, rest) -> do
      Archiver (put rest)
      pure c

readArchiverContents :: FilePath -> Archiver ByteString
readArchiverContents =
  Archiver . lift . lift . ByteString.readFile
