{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}
-- |
-- Module      :  Web.Telegram.Bot.Types
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Bot types and helper instances.
--
module Web.Telegram.Bot.Types where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Control.Monad.Trans.Reader (ReaderT)
import Web.Telegram.API.Bot (Token(..))
import Network.HTTP.Client (Manager)
import GHC.Generics (Generic)

type Timeout = Int

data Config = Config
  { timeout :: Timeout
    -- ^ Polling timeout in seconds
  , token   :: Token
    -- ^ Telegram Bot API private token
  } deriving (Generic, Show)

instance FromJSON Config
instance ToJSON Config

instance FromJSON Token where
    parseJSON = fmap Token . parseJSON

instance ToJSON Token where
    toJSON (Token t) = toJSON t

-- | Default bot config
defaultConfig :: Config
defaultConfig = Config 10 (Token "")

-- | Telegram bot monad
type BotT = ReaderT (Manager, Config)
type Bot  = BotT IO
