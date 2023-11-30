{-# LANGUAGE OverloadedStrings #-}

module Html (renderContents) where

import Content
import Control.Monad (forM_)
import Text.Blaze.Html5
  ( body,
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

renderContents :: (Foldable t, Content a) => t a -> H.Html
renderContents contents =
  docTypeHtml $ do
    H.head $ do
      H.title "7 days of Haskell"
      body $ do
        p "Popular content"
        ul $ forM_ contents $ \content ->
          li $ do
            H.span $ toHtml (title content)
            br
            img ! src (toValue $ url content)
            br
            H.span $ toHtml (footnote content)
  where
    footnote content =
      "Nota: " ++ show (rating content) ++ " - Lançamento: " ++ release content
