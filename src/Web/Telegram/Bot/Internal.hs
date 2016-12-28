-- |
-- Module      :  Web.Telegram.Bot.Handler
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Telegram Bot runners.
--
module Web.Telegram.Bot.Internal (runBot, storyBot, sendMessageBot, forkBot) where

import Control.Concurrent (forkIO, forkFinally, killThread, ThreadId)
import Control.Monad.Trans.Reader (runReaderT, ask)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager, Manager)
import Control.Exception (throwIO)
import Data.IntMap.Strict as I
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (forever)
import Web.Telegram.Bot.Story
import Web.Telegram.Bot.Types
import Data.Text (Text, pack)
import Data.Map.Strict as M
import Web.Telegram.API.Bot
import Pipes

-- | Try connection with Telegram Bot API
trySelf :: Token -> Manager -> IO ()
trySelf tok mgr = do
    me <- getMe tok mgr
    case me of
        Left e -> throwIO e
        Right (Response u) ->
            putStrLn $ "Hello! I'm " ++ show (user_first_name u)

-- | Infinity loop for getting updates from API
updateLoop :: BotConfig a
           => (Update -> Bot a ())
           -- ^ Update handler
           -> Bot a ()
updateLoop handler = go 0
  where updates o t = getUpdates t (Just o) Nothing . Just
        go offset = do
            (manager, config) <- ask
            -- Take updates
            upd <- liftIO $
                updates offset (authToken config) (pollTimeout config) manager
            -- Check for errors
            case result <$> upd of
                Left e   -> liftIO (throwIO e)
                Right [] -> go offset
                Right xs  -> do
                    -- Run handler for any update
                    mapM_ handler xs
                    -- Step for the new offset
                    go (maximum (update_id <$> xs) + 1)

-- | 'Producer' from 'Chan' creator
fromChan :: MonadIO m => Chan a -> Producer a m ()
fromChan c = forever $ liftIO (readChan c) >>= yield

-- | Incoming messages will be sended
toSender :: MonadIO m => (BotMessage -> m ()) -> Consumer BotMessage m ()
toSender sender = forever $ await >>= lift . sender

-- | Chat ID based message splitter
storyHandler :: BotConfig a
             => MVar (IntMap (Chan Message, ThreadId))
             -> Map Text (Story a)
             -> BotMessage
             -> Update
             -> Bot a ()
storyHandler chats stories help = go
  where go (Update{message =
                Just msg@(Message {from = Just user})}) = do
            -- Get a chat id
            let cid           = chat_id (chat msg)
                newStory item = modifyMVar_ chats (return . I.insert cid item)
                deleteStory   = modifyMVar_ chats (return . I.delete cid)

            chatMap <- liftIO (readMVar chats)
            -- Lookup chat id in the map
            case I.lookup cid chatMap of
                -- Chat exist => story is run now
                Just (chan, tid) -> do
                    -- Want to cancel it?
                    case text msg of
                        Just "/cancel" -> do
                            liftIO (killThread tid)
                            sendMessageBot (chat msg) help

                        _ -> liftIO (writeChan chan msg)

                -- Is no runned stories
                Nothing ->
                    case text msg >>= flip M.lookup stories of
                        -- Unknown story, try to help
                        Nothing -> sendMessageBot (chat msg) help

                        -- Story exist
                        Just story -> do
                            -- Create chan
                            chan <- liftIO newChan

                            -- Story pipeline
                            let pipeline = fromChan chan
                                        >-> (story (user, chat msg) >>= yield)
                                        >-> toSender (sendMessageBot (chat msg))

                            -- Story effect
                            (manager, config) <- ask
                            let runStory = runReaderT (runEffect pipeline)
                                                      (manager, config)

                            -- Run story in separate thread
                            tid <- liftIO $ forkFinally runStory
                                                        (const deleteStory)
                            -- Update chanMap
                            liftIO (newStory (chan, tid))
        go _ = return ()

sendMessageBot :: BotConfig a => Chat -> BotMessage -> Bot a ()
sendMessageBot c msg = do
    (manager, config) <- ask
    liftIO $ send (textChatId c) (authToken config) manager msg
  where textChatId = pack . show . chat_id

        send cid tok mgr BotTyping =
            let r = sendChatActionRequest cid Typing
             in sendChatAction tok r mgr >> return ()

        send cid tok mgr (BotText t) =
            let r = (sendMessageRequest cid t)
                  { message_reply_markup = Just replyKeyboardHide
                  , message_parse_mode   = Just Markdown }
             in sendMessage tok r mgr >> return ()

        send cid tok mgr (BotKeyboard txt btnTexts) =
            let btns     = fmap keyboardButton <$> btnTexts
                keyboard = replyKeyboardMarkup btns
                r = (sendMessageRequest cid txt)
                  { message_reply_markup = Just keyboard
                  , message_parse_mode   = Just Markdown }
             in sendMessage tok r mgr >> return ()

-- | User story handler
storyBot :: (BotConfig a, ToBotMessage help) => help -> Map Text (Story a) -> Bot a ()
storyBot help stories = do
    -- Create map from user to it story
    chats <- liftIO (newMVar I.empty)
    -- Run update loop
    updateLoop (storyHandler chats stories $ toMessage help)

-- | Run bot monad
runBot :: BotConfig a => a -> Bot a b -> IO b
runBot config bot = do
    -- Init connection manager
    manager <- newManager tlsManagerSettings
    -- Check connection
    trySelf (authToken config) manager
    -- Run bot
    runReaderT bot (manager, config)

-- Fork bot thread
forkBot :: BotConfig a => Bot a () -> Bot a ThreadId
forkBot bot = ask >>= liftIO . forkIO . runReaderT bot
