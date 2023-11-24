{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TMDB where

import Control.Monad (forM_)
import Data.Aeson (Value (..), (.:))
import Data.Aeson.Types (FromJSON (parseJSON), Parser, prependFailure, typeMismatch)
import Data.ByteString.Char8 qualified as Char8
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBearerAuth)
import Text.Blaze.Html.Renderer.String qualified as Renderer
import Text.Blaze.Html5
  ( Html,
    body,
    br,
    docTypeHtml,
    img,
    li,
    p,
    toHtml,
    toValue,
    ul,
    (!),
  )
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes (src)

data Movie = Movie
  { id :: Int,
    title :: String,
    releaseDate :: String,
    posterURL :: String,
    voteAverage :: Float
  }
  deriving (Show)

posterBaseURL :: String
posterBaseURL = "https://image.tmdb.org/t/p/w500"

instance FromJSON Movie where
  parseJSON :: Value -> Parser Movie
  parseJSON (Object v) =
    Movie
      <$> v .: "id"
      <*> v .: "title"
      <*> v .: "release_date"
      <*> ( (posterBaseURL ++)
              <$> v .: "poster_path"
          )
      <*> v .: "vote_average"
  parseJSON invalid =
    prependFailure "parsing TMDB movie failed, " (typeMismatch "Object" invalid)

newtype Response = Response
  { results :: [Movie]
  }
  deriving (Show, Generic)

instance FromJSON Response

runTMBD :: String -> IO ()
runTMBD bearearToken = do
  let req =
        setRequestBearerAuth
          (Char8.pack bearearToken)
          "GET https://api.themoviedb.org/3/discover/movie?include_adult=false&include_video=false&language=en-US&page=1&sort_by=popularity.desc"
  response <- httpJSON req
  let resBody = getResponseBody response :: Response
  putStrLn . Renderer.renderHtml . renderMovies $ resBody.results

renderMovies :: [Movie] -> Html
renderMovies movies =
  docTypeHtml $ do
    Html.head $ do
      Html.title "7 days of Haskell"
      body $ do
        p "Popular movies"
        ul $ forM_ movies $ \movie ->
          li $ do
            Html.span $ toHtml movie.title
            br
            img ! src (toValue movie.posterURL)
            br
            Html.span $ toHtml (footnote movie)
  where
    footnote movie =
      "Nota: " ++ show movie.voteAverage ++ " - Lançamento: " ++ movie.releaseDate
