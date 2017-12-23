{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Twilio
import Twilio.Messages

main :: IO ()
main =
  runTwilio' (getEnv "TWILIO_ACCOUNT_SID") (getEnv "TWILIO_AUTH_TOKEN") $ do
    let body = PostMessage "NUMBER_TO_TEXT" "YOUR_TWILIO_NUMBER" "Hello, World!"
    message <- post body
    liftIO $ print message
