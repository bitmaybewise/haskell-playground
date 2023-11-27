module TMDB where

import TMDB.ApiClient qualified as ApiClient
import TMDB.Html (renderMovies)
import Text.Blaze.Html.Renderer.String qualified as Renderer

runTMBD :: String -> IO ()
runTMBD bearearToken = do
  movies <- ApiClient.discoverMovies bearearToken
  (putStrLn . Renderer.renderHtml . renderMovies) movies
