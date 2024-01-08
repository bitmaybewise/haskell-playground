module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad

main :: IO ()
main = do
  chan <- newChan
  writeList2Chan chan [1 .. 100]
  forM_ [0 .. 2] $ \wid -> waitFor (runWorker wid chan)

runWorker :: Int -> Chan Int -> MVar () -> Int -> IO ()
runWorker wid chan await counter = void (forkIO runWorker')
  where
    runWorker' = do
      putStrLn $ "worker id = " ++ show wid ++ ", counter = " ++ show counter
      chanN <- readChan' `onException` handleEmptyChan
      putStrLn $ "channel nr = " ++ show chanN
      if counter < 10
        then runWorker wid chan await (counter + 1)
        else takeMVar await

    readChan' = readChan chan

    handleEmptyChan :: IO Int
    handleEmptyChan = pure (-1)

-- waitFor is a function to simulate a thread wait using MVar capabilities
waitFor :: (MVar () -> Int -> IO ()) -> IO ()
waitFor fn = do
  await <- newMVar ()
  void $ fn await 0
  putMVar await ()
