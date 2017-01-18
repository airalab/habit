{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Web.Bot

type Name    = Text
type Surname = Text
type Age     = Int

hello :: Name -> Surname -> Age -> Message
hello name surname age =
    toMessage $ "Hello, " <> name <> " " <> surname <> "!\n"
             <> "You lost " <> (pack $ show age) <> " years =)"

helloStory :: Story a
helloStory _ = hello <$> question "How your name?"
                     <*> question "How your surname?"
                     <*> question "How old are you?"

helpMsg :: Text
helpMsg = "Hello, I'm hello bot!"

instance APIToken Telegram where
    apiToken = return "bot308165623:AAGeMyzyedc787LZSpzlhwTCWQcO5CAZpAo"

instance Persist Telegram where
    persist = return $ Sqlite "storage.db"

main :: IO ()
main = runBot myStory
  where
    myStory :: Bot Telegram ()
    myStory = storyBot helpMsg [("/hello", helloStory)]
