module MyState where

import Control.Monad.State (State, execState, modify, runState)

data MyState = MyState
  { counter :: Int,
    message :: String
  }
  deriving (Show)

-- Increment the counter and set a message in the state
incrementCounter :: State MyState ()
incrementCounter = do
  modify (\s -> s {counter = counter s + 1})
  setMessage "Counter incremented"

-- Set a message in the state
setMessage :: String -> State MyState ()
setMessage msg = modify (\s -> s {message = msg})

-- A composed stateful action
updateState :: State MyState ()
updateState = do
  incrementCounter
  setMessage "State updated"

initialState :: MyState
initialState = MyState 0 "Initial state"

main :: IO ()
main = do
  let (result, finalState) = runState updateState initialState
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Final State: " ++ show finalState
