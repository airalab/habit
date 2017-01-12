{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Web.Telegram.Bot.Types (
    Bot
  , runBot
  , forkBot
  , forkBotFinally
  , Timeout
  , BotConfig(..)
  ) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Web.Telegram.API.Bot (Token, Response(..), getMe)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Concurrent (forkIO, forkFinally, ThreadId)
import Control.Exception (throwIO, SomeException)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client (Manager)
import Control.Monad.IO.Class

type Timeout = Int

-- | Bot params
class BotConfig a where
    pollTimeout :: Bot a Timeout
    pollTimeout = return 10
    -- ^ Updates poll timeout in seconds

    manager :: Bot a Manager
    manager = Bot ask
    -- ^ Connection manager getter

    authToken :: Bot a Token
    -- ^ BotFather authentification token

-- | Telegram bot monad
newtype Bot a b = Bot (ReaderT Manager IO b)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run bot monad
runBot :: (BotConfig a, MonadIO m) => Bot a b -> m b
runBot bot = liftIO $ do
    -- Init connection manager
    manager <- newManager tlsManagerSettings
    -- Run bot
    let Bot m = trySelf >> bot
     in runReaderT m manager
  where
    trySelf = do
        tok <- authToken
        mgr <- manager
        me  <- liftIO (getMe tok mgr)
        liftIO $ case me of
            Left e -> throwIO e
            Right (Response u) ->
                putStrLn $ "Hello! I'm " ++ show u

-- Fork bot thread
forkBot :: BotConfig a => Bot a () -> Bot a ThreadId
forkBot (Bot bot) = Bot (ask >>= liftIO . forkIO . runReaderT bot)

-- Fork bot thread with finalizator
forkBotFinally :: BotConfig a => Bot a b
                              -> (Either SomeException b -> IO ())
                              -> Bot a ThreadId
forkBotFinally (Bot bot) f =
    Bot (ask >>= liftIO . flip forkFinally f . runReaderT bot)
