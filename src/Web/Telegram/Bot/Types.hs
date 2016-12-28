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
-- Bot types.
--
module Web.Telegram.Bot.Types where

import Control.Monad.Trans.Reader (ReaderT)
import Web.Telegram.API.Bot (Token)
import Network.HTTP.Client (Manager)

type Timeout = Int

-- | Telegram bot config
class BotConfig a where
    pollTimeout :: a -> Timeout
    pollTimeout = const 10
    -- ^ Updates poll timeout in seconds

    authToken   :: a -> Token
    -- ^ BotFather authentification token

-- | Telegram bot monad
type BotT a = ReaderT (Manager, a)
type Bot a  = BotT a IO
