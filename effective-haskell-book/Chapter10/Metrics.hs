{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)
import Data.Foldable (Foldable (foldl'), for_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import GHC.Base (lazy)
import Text.Printf (printf)

data AppMetrics = AppMetrics
  { successCount :: Int,
    failureCount :: Int,
    callDuration :: Map.Map String Int
  }
  deriving (Eq, Show)

newtype Metrics = Metrics {appMetricsStore :: IORef AppMetrics}

newMetrics :: IO Metrics
newMetrics =
  let emptyAppMetrics =
        AppMetrics
          { successCount = 0,
            failureCount = 0,
            callDuration = Map.empty
          }
   in Metrics <$> newIORef emptyAppMetrics

tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics metricsRef) = modifyIORef metricsRef $ \m ->
  m {successCount = 1 + successCount m}

tickFailure :: Metrics -> IO ()
tickFailure (Metrics metricsRef) = modifyIORef metricsRef $ \m ->
  m {failureCount = 1 + failureCount m}

timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
  startTime <- getCurrentTime
  result <- action
  endTime <- getCurrentTime
  modifyIORef metrics $ \oldMetrics ->
    let oldDurationValue =
          fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)

        runDuration =
          floor . nominalDiffTimeToSeconds $
            diffUTCTime endTime startTime

        newDurationValue = oldDurationValue + runDuration
     in oldMetrics
          { callDuration =
              Map.insert actionName newDurationValue $
                callDuration oldMetrics
          }
  pure result

timePureFunction :: Metrics -> String -> a -> IO a
timePureFunction (Metrics metrics) actionName action = do
  startTime <- getCurrentTime
  let !result = action
  -- Without using the bang pattern for result it will be lazy
  -- evaluated, which means that if `result` is never used it will
  -- never executes. The bang will force it to be strictly evaluated.
  endTime <- getCurrentTime
  modifyIORef metrics $ \oldMetrics ->
    let oldDurationValue =
          fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)

        runDuration =
          floor . nominalDiffTimeToSeconds $
            diffUTCTime endTime startTime

        newDurationValue = oldDurationValue + runDuration
     in oldMetrics
          { callDuration =
              Map.insert actionName newDurationValue $
                callDuration oldMetrics
          }
  pure result

displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics metricsStore) = do
  AppMetrics {..} <- readIORef metricsStore
  putStrLn $ "successes: " <> show successCount
  putStrLn $ "failures: " <> show failureCount
  for_ (Map.toList callDuration) $ \(functionName, timing) ->
    putStrLn $ printf "Time spent in \"%s\": %d" functionName timing

main = do
  metrics <- newMetrics
  timeFunction metrics "testing123" effectFn
  timePureFunction metrics "puuuuurrrr" pureFn
  readIORef (appMetricsStore metrics) >>= print
  where
    effectFn :: IO ()
    effectFn = do
      putStrLn "legen... wait for it..."
      threadDelay 1_000_000
      putStrLn "DARY!"

    pureFn :: Int
    pureFn = foldl' (+) 0 [1 .. 500_000_000]
