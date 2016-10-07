{-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Concurrent (threadDelay)
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Web.Telegram.Bot

type Name    = Text
type Surname = Text
type Age     = Int

hello :: Name -> Surname -> Age -> IO BotMessage
hello name surname age = do
    return . toMessage $ "Hello, " <> name <> " " <> surname <> "!\n"
                      <> "You lost " <> (pack $ show age) <> " years =)"

helloStory :: Story
helloStory _ = hello <$> question "How your name?"
                     <*> question "How your surname?"
                     <*> question "How old are you?"

helpMessage :: Text
helpMessage = "Hello, I'm hello bot!"

main :: IO ()
main = runBot config $ do
            storyBot helpMessage [("/hello", helloStory)]
  where config = defaultConfig { token = "bot..." }
