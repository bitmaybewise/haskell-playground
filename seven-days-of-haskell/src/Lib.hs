module Lib where

import Html (renderContents)
import Marvel.ApiClient qualified as Marvel
import TMDB.ApiClient qualified as TMDB
import Text.Blaze.Html.Renderer.String qualified as Renderer

run :: String -> String -> String -> IO ()
run tmdbBearearToken marvelApikey marvelPrivatekey = do
  movies <- TMDB.discoverMovies tmdbBearearToken
  series <- Marvel.series marvelApikey marvelPrivatekey
  putStrLn . Renderer.renderHtml . renderContents $ movies
  putStrLn . Renderer.renderHtml . renderContents $ series
