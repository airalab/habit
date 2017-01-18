{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Module      :  Web.Bot.Platform
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Bot platform type class.
--
module Web.Bot.Platform (
    Bot
  , Platform(..)
  , APIToken(..)
  , getManager
  , forkFinallyBot
  , forkBot
  , runBot
  ) where

import Control.Monad.Logger (MonadLogger(..), LoggingT, runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Concurrent (forkIO, forkFinally, ThreadId)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Exception (throwIO, SomeException)
import Network.HTTP.Client (newManager, Manager)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Base (MonadBase(..))
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Text (Text)

import Web.Bot.Message (Message, ToMessage)
import Web.Bot.User (User)
import Web.Bot.Log

-- | Message bot monad
newtype Bot a b = Bot { unBot :: ReaderT Manager (LoggingT IO) b }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadLogger (Bot a) where
    monadLoggerLog a b c d = Bot (monadLoggerLog a b c d)

instance MonadBase IO (Bot a) where
    liftBase = liftIO

instance MonadBaseControl IO (Bot a) where
    type StM (Bot a) b = b
    liftBaseWith f = Bot $ liftBaseWith $ \r -> f (r . unBot)
    restoreM = return

-- | Message bot platform
-- Different platforms provide message bot API,
-- e.g. Telegram, Viber, Facebook Messenger etc.
-- This is generalized interface to it.
class Platform a where
    -- | Try connection to platform API
    trySelf :: APIToken a => Bot a ()

    -- | Send message to user by platform API
    sendMessage :: (ToMessage msg, APIToken a) => User -> msg -> Bot a ()

    -- | Get user updates by platform API
    messageHandler :: APIToken a
                   => (User -> Message -> Bot a b)
                   -- ^ Incoming message handler
                   -> Bot a c
                   -- ^ Blocking event processing

    -- | Short description of platform
    platformName :: a -> Text

-- Instance of it should be writen by user
class Platform a => APIToken a where
    apiToken :: Bot a Text
    -- ^ Platform API token

-- | TCP-connection manager getter
getManager :: Bot a Manager
{-# INLINE getManager #-}
getManager = Bot ask

-- | Run bot monad
runBot :: (APIToken a, MonadIO m)
       => Bot a b -> m b
runBot bot = liftIO $ do
    -- Init connection manager
    manager <- newManager tlsManagerSettings
    -- Run bot monad
    runStderrLoggingT (runReaderT mBot manager)
  where Bot mBot = trySelf >> bot

-- | Fork bot thread
forkBot :: APIToken a
        => Bot a () -> Bot a ThreadId
forkBot (Bot bot) = do
    t <- Bot (ask >>= forkReader)
    $logDebugS "Bot" ("Forked " <> T.pack (show t))
    return t
  where forkReader = liftIO . forkIO
                   . runStderrLoggingT . runReaderT bot

-- | Fork bot thread with finalizer
forkFinallyBot :: APIToken a
               => Bot a b
               -> (Either SomeException b -> IO ())
               -> Bot a ThreadId
forkFinallyBot (Bot bot) f = do
    t <- Bot (ask >>= forkFinallyReader)
    $logDebugS "Bot" ("Forked finally " <> T.pack (show t))
    return t
  where forkFinallyReader = liftIO . flip forkFinally f
                          . runStderrLoggingT . runReaderT bot
