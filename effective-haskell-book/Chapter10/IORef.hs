{-# LANGUAGE TypeApplications #-}

module Main where

import Data.IORef (newIORef, readIORef, writeIORef)

readWriteRef :: IO Int
readWriteRef = do
  myRef <- newIORef @Int 0
  writeIORef myRef 7
  readIORef myRef

main :: IO ()
main = readWriteRef >>= print
