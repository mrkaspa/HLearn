{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Twilio
import Twilio.Messages

main :: IO ()
main =
  runTwilio' (getEnv "TWILIO_ACCOUNT_SID") (getEnv "TWILIO_AUTH_TOKEN") $ do
    let bodyMessage = PostMessage "NUMBER_TO_TEXT"
                                  "YOUR_TWILIO_NUMBER"
                                  "Hello, World!"
                                  Nothing
    message <- post bodyMessage
    liftIO $ print message
