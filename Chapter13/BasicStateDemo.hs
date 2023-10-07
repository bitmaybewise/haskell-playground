module BasicStateDemo where

import Control.Monad.State

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
