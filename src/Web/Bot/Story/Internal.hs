{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Web.Bot.Story.Internal
-- Copyright   :  Alexander Krupenkin 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Story bot implementation.
--
module Web.Bot.Story.Internal (storyBot) where

import Control.Concurrent (killThread, ThreadId)
import Data.IntMap.Strict as I
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (forever)
import qualified Data.Text as T
import Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import Pipes

import Web.Bot.Platform
import Web.Bot.Message
import Web.Bot.Story
import Web.Bot.User
import Web.Bot.Log

-- | 'Producer' from 'Chan' creator
fromChan :: MonadIO m => Chan a -> Producer a m ()
fromChan c = forever $ liftIO (readChan c) >>= yield

-- | Incoming messages will be sended
toSender :: MonadIO m => (Message -> m ()) -> Consumer Message m ()
toSender sender = forever $ await >>= lift . sender

-- | Chat ID based message splitter
storyHandler :: (APIToken a, ToMessage help)
             => MVar (IntMap (Chan Message, ThreadId))
             -> Map Message (Story a)
             -> help
             -> User -> Message -> Bot a ()
storyHandler chats stories help user msg = do
    -- Get a chat id
    let newStory item = modifyMVar_ chats
                                    (return . I.insert (userChat user) item)
        deleteStory   = modifyMVar_ chats
                                    (return . I.delete (userChat user))

    chatMap <- liftIO (readMVar chats)
    -- Lookup chat id in the map
    case I.lookup (userChat user) chatMap of
        -- Chat exist => story is run now
        Just (chan, tid) ->
            -- Want to cancel it?
            case msg of
                "/cancel" -> do
                    $logDebugS "Story" ("Cancel request, story "
                                        <> T.pack (show tid) <> " killed.")
                    liftIO (killThread tid)
                    sendMessage user help

                _ -> liftIO (writeChan chan msg)

        -- Is no runned stories
        Nothing ->
            case M.lookup msg stories of
                -- Unknown story, try to help
                Nothing -> do
                    sendMessage user help
                    $logDebugS "Story" ("Unknown story "
                                        <> T.pack (show msg) <> ".")

                -- Story exist
                Just story -> do
                    -- Create chan
                    chan <- liftIO newChan
                    -- Story pipeline
                    let pipeline = fromChan chan
                                >-> (story user >>= yield)
                                >-> toSender (sendMessage user)
                    -- Run story in separate thread
                    tid <- forkFinallyBot (runEffect pipeline)
                                          (const deleteStory)
                    -- Update userMap
                    liftIO (newStory (chan, tid))
                    $logDebugS "Story" ("Story "
                                        <> T.pack (show msg)
                                        <> " spawned at "
                                        <> T.pack (show tid) <> ".")

-- | User story handler
storyBot :: (APIToken a, ToMessage help)
         => help -> Map Message (Story a) -> Bot a ()
storyBot help stories = do
    -- Create map from user chat to it story
    chats <- liftIO (newMVar I.empty)
    -- Run update loop
    $logDebugS "Story" "Init success."
    messageHandler $ storyHandler chats stories help
