-- |
-- Module      :  Web.Telegram.Bot
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  email@something.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Telegram Bot API microframework for creating story like
-- command handlers:
--
-- @
--      helloStory :: Story
--      helloStory _ = hello <$> question "How your name?"
--                           <*> question "How your surname?"
--                           <*> question "How old are you?"
-- @
--
module Web.Telegram.Bot (
  -- * Exported types
    BotMessage(..)
  , Question(..)
  , Answer(..)
  , Config(..)
  , Story
  -- ** Story makers
  , question
  , question'
  -- ** Bot runners
  , storyBot
  , defaultConfig
  , runBot
  ) where

import Web.Telegram.Bot.Internal
import Web.Telegram.Bot.Types
import Web.Telegram.Bot.Story
