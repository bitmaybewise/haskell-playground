{-# LANGUAGE OverloadedStrings #-}

module Marvel.ApiClient (Image (..), Series (..), series) where

import Crypto.Hash (MD5, hash)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.ByteString.Char8 qualified as B8
import Data.UnixTime (UnixTime (utSeconds), getUnixTime)
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestQueryString)

data Image = Image
  { path :: String,
    extension :: String
  }
  deriving (Show, Generic)

instance FromJSON Image

data Series = Series
  { id :: Int,
    title :: String,
    startYear :: Int,
    endYear :: Int,
    rating :: String,
    thumbnail :: Image
  }
  deriving (Show, Generic)

instance FromJSON Series

newtype Response = Response
  { results :: [Series]
  }
  deriving (Show, Generic)

instance FromJSON Response where
  parseJSON :: Value -> Parser Response
  parseJSON (Object v) =
    Response <$> do
      resData <- v .: "data"
      resData .: "results"
  parseJSON invalid =
    prependFailure "parsing Marvel response failed, " (typeMismatch "Object" invalid)

series :: String -> String -> IO [Series]
series apikey privatekey = do
  ts <- getUnixTime
  let tsInSeconds = show $ utSeconds ts
      strToHash = tsInSeconds ++ privatekey ++ apikey
      md5 = hash (B8.pack strToHash) :: MD5
      req =
        setRequestQueryString
          [ ("ts", Just $ B8.pack tsInSeconds),
            ("apikey", Just $ B8.pack apikey),
            ("hash", Just $ B8.pack (show md5))
          ]
          "http://gateway.marvel.com/v1/public/series"
  res <- httpJSON req
  return $ results (getResponseBody res :: Response)
