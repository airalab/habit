-- |
-- Module      :  Web.Bot
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  email@something.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Text message bot framework for creating story like:
--
-- @
--      helloStory :: BotConfig a => Story a
--      helloStory _ = hello <$> question "How your name?"
--                           <*> question "How your surname?"
--                           <*> question "How old are you?"
-- @
--
module Web.Bot (
  -- ** Story & message types
    ToMessage(..)
  , Answer(..)
  , Message
  , StoryT
  , Story
  , User
  -- ** Story makers
  , question
  , replica
  , select
  -- ** Bot monad & configuration classes
  , Bot
  , Platform(..)
  , APIToken(..)
  -- ** Bot platforms
  , Telegram
  , FBMessenger
  -- ** Bot storage
  , Persist(..)
  , Connection(..)
  , ConnectInfo(..)
  -- ** Bot runners
  , storyBot
  , forkBot
  , runBot
  -- ** Re-exports
  , yield
  , await
  , lift
  ) where

import Pipes (yield, await, lift)
import Web.Bot.Story.Internal
import Web.Bot.Platform.FBMessenger
import Web.Bot.Platform.Telegram
import Web.Bot.Platform
import Web.Bot.Persist
import Web.Bot.Message
import Web.Bot.Story
import Web.Bot.User
