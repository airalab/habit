{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      :  Web.Telegram.Bot.Types
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Bot types and helper instances.
--
module Web.Telegram.Bot.Types where

import Control.Monad.Trans.Reader (ReaderT)
import Network.HTTP.Client (Manager)
import Web.Telegram.API.Bot (Token(..))
import Data.String (IsString(..))
import Data.Text (Text, pack)

type Timeout = Int

data Config = Config
  { timeout :: Timeout
    -- ^ Polling timeout in seconds
  , token   :: Token
    -- ^ Telegram Bot API private token
  } deriving Show

-- | Default bot config
defaultConfig :: Config
defaultConfig = Config 10 ""

-- | Telegram bot monad
type BotT = ReaderT (Manager, Config)
type Bot  = BotT IO

-- | Overload string as IO result
instance IsString a => IsString (IO a) where
    fromString = return . fromString

-- | Overload string as 'Token'
instance IsString Token where
    fromString = Token . pack
