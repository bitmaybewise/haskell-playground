{-# LANGUAGE OverloadedStrings #-}

module TMDB.Html (renderMovies) where

import Control.Monad (forM_)
import TMDB.ApiClient (Movie (..))
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
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (src)

renderMovies :: [Movie] -> Html
renderMovies movies =
  docTypeHtml $ do
    H.head $ do
      H.title "7 days of Haskell"
      body $ do
        p "Popular movies"
        ul $ forM_ movies $ \movie ->
          li $ do
            H.span $ toHtml (title movie)
            br
            img ! src (toValue $ posterURL movie)
            br
            H.span $ toHtml (footnote movie)
  where
    footnote movie =
      "Nota: " ++ show (voteAverage movie) ++ " - Lançamento: " ++ releaseDate movie
