module Web
  ( wmain
  ) where

import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.FilePath.Posix ((</>))
import Text.Hastache
import Text.Hastache.Context
import Web.Scotty
import qualified Data.Text.Lazy.Encoding as TE

wmain :: IO ()
wmain = scotty 3000 $ do
  get "/demo/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  notFound $ renderFromFile "demo.html"

renderFromFile :: String -> ActionM ()
renderFromFile tpl = do
  templateText <- rendered
  html templateText
 where
  tplFile = "./templates" </> tpl
  rendered =
    TE.decodeUtf8 <$> hastacheFile defaultConfig tplFile (mkStrContext context)

template = "Hello, {{name}}!\n\nYou have {{unread}} unread messages."

context "name"   = MuVariable ("Michel" :: String)
context "unread" = MuVariable (100 :: Int)
