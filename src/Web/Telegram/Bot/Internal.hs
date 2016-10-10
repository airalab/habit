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
module Web.Telegram.Bot.Internal (runBot, storyBot) where

import Control.Monad.Trans.Reader (runReaderT, ask)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager, Manager)
import Control.Monad (forever, (>=>))
import Control.Concurrent (forkIO)
import Control.Exception (throwIO)
import Data.IntMap.Strict as I
import Control.Concurrent.Chan
import Control.Concurrent.MVar
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
updateLoop :: Manager
           -> Config
           -> (Token -> Manager -> Update -> IO ())
           -- ^ Update handler
           -> IO ()
updateLoop mgr (Config to tok) handler = go 0
  where updates o = getUpdates tok (Just o) Nothing (Just to) mgr
        go offset = do
            -- Take updates
            updates <- fmap result <$> liftIO (updates offset)
            -- Check for errors
            case updates of
                Left e   -> liftIO (throwIO e)
                Right [] -> go offset
                Right xs  -> do
                    -- Run handler for any update
                    mapM_ (handler tok mgr) xs
                    -- Step for the new offset
                    go (maximum (update_id <$> xs) + 1)

-- | 'Producer' from 'Chan' creator
fromChan :: Chan a -> Producer a IO ()
fromChan c = forever $ lift (readChan c) >>= yield

-- | Incoming messages will be sended
toReply :: Monad m => (BotMessage -> m ()) -> Consumer BotMessage m ()
toReply reply = forever $ await >>= lift . reply

-- | Chat ID based message splitter
storyHandler :: MVar (IntMap (Chan Message))
             -> Map Text Story
             -> BotMessage
             -> Token -> Manager -> Update -> IO ()
storyHandler varChatMap stories help tok mgr = go
  where go (Update { message = Just msg }) = do
            -- Get a chat id
            let cid = chat_id (chat msg)

            -- Read chat map
            chatMap <- readMVar varChatMap

            -- Lookup chat id in the map
            case I.lookup cid chatMap of

                -- Chat exist -> story is run now
                Just chan -> do
                    writeChan chan msg

                    --  Want to cancel it?
                    case text msg of
                        Just "/cancel" -> do
                            _ <- swapMVar varChatMap (I.delete cid chatMap)
                            reply cid help

                        _ -> return ()


                -- Is no runned stories
                Nothing ->
                    case text msg >>= flip M.lookup stories of
                        -- Unknown story, try to help
                        Nothing -> reply cid help

                        -- Story exist
                        Just story -> do
                            -- Create chan and update chanMap
                            chan <- newChan
                            _ <- swapMVar varChatMap (I.insert cid chan chatMap)

                            -- Story pipeline
                            let pipeline = fromChan chan
                                        >-> (story (chat msg) >>= yield)
                                        >-> toReply (reply cid)

                            -- Run story
                            _ <- forkIO $ do
                                runEffect pipeline
                                _ <- swapMVar varChatMap (I.delete cid chatMap)
                                return ()
                            return ()
        go _ = return ()

        reply cid BotTyping = do
            let r = sendChatActionRequest (pack $ show cid) Typing
            _ <- sendChatAction tok r mgr
            return ()

        reply cid (BotText t) = do
            let r = (sendMessageRequest (pack $ show cid) t)
                    { message_reply_markup = Just replyKeyboardHide }
            _ <- sendMessage tok r mgr
            return ()

        reply cid (BotKeyboard txt btnTexts) = do
            let btns = fmap keyboardButton <$> btnTexts
                keyboard = replyKeyboardMarkup btns
                r = (sendMessageRequest (pack $ show cid) txt)
                    { message_reply_markup = Just keyboard }
            _ <- sendMessage tok r mgr
            return ()

-- | User story handler
storyBot :: Question help => help -> Map Text Story -> Bot ()
storyBot help stories = do
    (manager, config) <- ask
    liftIO $ do
        -- Create map from user to it story
        chats <- newMVar I.empty

        -- Run update loop
        updateLoop manager config $
            storyHandler chats stories (toMessage help)

-- | Run bot monad
runBot :: Config -> Bot a -> IO a
runBot config bot = do
    -- Init connection manager
    manager <- newManager tlsManagerSettings
    -- Check connection
    trySelf (token config) manager
    -- Run bot
    runReaderT bot (manager, config)
