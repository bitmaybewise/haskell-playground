module Content where

import Data.Maybe (fromMaybe)
import Marvel.ApiClient qualified as Marvel
import TMDB.ApiClient qualified as TMDB
import Text.Read (readMaybe)

class Content a where
  title :: a -> String
  release :: a -> String
  url :: a -> String
  rating :: a -> Float

instance Content TMDB.Movie where
  title = TMDB.title
  release = TMDB.releaseDate
  url = TMDB.posterURL
  rating = TMDB.voteAverage

instance Content Marvel.Series where
  title = Marvel.title
  release = show . Marvel.startYear
  url = Marvel.path . Marvel.thumbnail
  rating series = fromMaybe 0 (readMaybe $ Marvel.rating series :: Maybe Float)
