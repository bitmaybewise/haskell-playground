module Exercises where

ioio :: IO (IO String)
ioio = return . return $ "io io"

ahoi :: IO (IO a) -> IO a
ahoi ioio = ioio >>= id

aListOfIO :: [a] -> [IO a]
aListOfIO aList = fmap return aList

anIOList :: [IO a] -> IO [a]
anIOList aList = mapM id aList
