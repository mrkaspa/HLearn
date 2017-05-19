module Web
  ( wmain
  ) where

import Data.Monoid (mconcat)
import Web.Scotty

wmain =
  scotty 3000 $ do
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    get "/demo/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
