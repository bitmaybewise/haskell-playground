module Main where

import Control.Exception (IOException, handle)
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isSuffixOf)
import Data.Set qualified as Set (empty, insert, member)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)

data FileType
  = FileTypeDirectory
  | FileTypeRegularFile
  | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
  isDirectory <- doesDirectoryExist fname
  isFile <- doesFileExist fname
  pure $ case (isDirectory, isFile) of
    (True, False) -> FileTypeDirectory
    (False, True) -> FileTypeRegularFile
    _otherwise -> FileTypeOther

dropSuffix :: String -> String -> String
dropSuffix suffix s
  | suffix `isSuffixOf` s = take (length s - length suffix) s
  | otherwise = s

traverseDirectory :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory rootPath action = do
  seenRef <- newIORef Set.empty
  resultRef <- newIORef []
  let haveSeenDirectory canonicalPath =
        Set.member canonicalPath <$> readIORef seenRef

      addDirectoryToSeen canonicalPath =
        modifyIORef seenRef $ Set.insert canonicalPath

      traverseSubdirectory subdirPath = do
        contents <- listDirectory subdirPath
        for_ contents $ \file' ->
          handle @IOException (\_ -> pure ()) $ do
            let file = subdirPath <> "/" <> file'
            canonicalPath <- canonicalizePath file
            classification <- classifyFile canonicalPath
            case classification of
              FileTypeOther -> pure ()
              FileTypeRegularFile ->
                modifyIORef resultRef (\results -> action file : results)
              FileTypeDirectory -> do
                alreadyProcessed <- haveSeenDirectory file
                unless alreadyProcessed $ do
                  addDirectoryToSeen file
                  traverseSubdirectory file

  traverseSubdirectory (dropSuffix "/" rootPath)
  readIORef resultRef

main = traverseDirectory "/tmp" id >>= print
