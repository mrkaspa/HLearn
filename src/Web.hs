{-# LANGUAGE OverloadedStrings #-}

module Web
  ( wmain ) where

import Web.Scotty

import Data.Monoid (mconcat)

wmain =
  scotty 3000 $ do
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    get "/demo/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
