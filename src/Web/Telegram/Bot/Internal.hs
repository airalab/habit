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

import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Client (newManager, Manager)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad (forever, (>=>))
import           Control.Concurrent (forkIO)
import           Control.Exception (throwIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Web.Telegram.Bot.Story
import           Web.Telegram.Bot.Types
import           Data.Text (Text, pack)
import           Web.Telegram.API.Bot
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Map (Map)
import qualified Data.Map as M
import           Pipes

-- | Try connection with Telegram Bot API
trySelf :: Token -> Manager -> IO ()
trySelf token manager = do
    me <- getMe token manager
    case me of
        Left e -> throwIO e
        Right GetMeResponse { user_result = u } ->
            putStrLn $ "Hello! I'm " ++ show (user_first_name u)

-- | Infinity loop for getting updates from API
updateLoop :: Token
           -- ^ Bot API token
           -> Manager
           -- ^ Connection manager
           -> Timeout
           -- ^ Polling timeout
           -> (Token -> Manager -> Update -> IO ())
           -- ^ Update handler
           -> IO ()
updateLoop token manager timeout handler = go 0
  where updates o = getUpdates token (Just o) Nothing (Just timeout) manager
        go offset = do
            -- Take updates
            result <- fmap update_result <$> liftIO (updates offset)
            -- Check for errors
            case result of
                Left e   -> liftIO (throwIO e)
                Right [] -> go offset
                Right r  -> do
                    -- Run handler for any update
                    mapM_ (handler token manager) r
                    -- Step for the new offset
                    go (maximum (update_id <$> r) + 1)

-- | 'Producer' from 'Chan' creator
fromChan :: Chan a -> Producer a IO ()
fromChan c = forever $ lift (readChan c) >>= yield

-- | Incoming messages will be sended
toReply :: Monad m => (BotMessage -> m ()) -> Consumer BotMessage m ()
toReply reply = forever $ await >>= lift . reply

runAction :: Monad m => m BotMessage -> Pipe a BotMessage m ()
runAction a = do
    yield BotTyping
    lift a >>= yield

-- | Chat ID based message splitter
storyHandler :: MVar (IntMap (Chan Message))
             -> Map Text Story
             -> BotMessage
             -> Token -> Manager -> Update -> IO ()
storyHandler varChatMap stories help token manager = go
  where go (Update { message = Just msg }) = do
            -- Get a chat id
            let cid = chat_id (chat msg)

            -- Read chat map
            chatMap <- readMVar varChatMap

            -- Lookup chat id in the map
            case IM.lookup cid chatMap of

                -- Chat exist -> story is run now
                Just chan -> do
                    writeChan chan msg

                    --  Want to cancel it?
                    case text msg of
                        Just "/cancel" -> do
                            swapMVar varChatMap (IM.delete cid chatMap)
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
                            swapMVar varChatMap (IM.insert cid chan chatMap)

                            -- Run story
                            _ <- forkIO $
                                runEffect $ fromChan chan
                                         >-> (story cid >>= runAction)
                                         >-> toReply (reply cid)
                            return ()
        go _ = return ()

        reply cid BotTyping = do
            let r = sendChatActionRequest (pack $ show cid) Typing
            _ <- sendChatAction token r manager
            return ()

        reply cid (BotText t) = do
            let r = sendMessageRequest (pack $ show cid) t
            _ <- sendMessage token r manager
            return ()

-- | User story handler
storyBot :: Question help => help -> Map Text Story -> Bot ()
storyBot help stories = do
    (manager, config) <- ask
    liftIO $ do
        -- Create map from user to it story
        chats <- newMVar IM.empty

        -- Run update loop
        updateLoop (token config) manager (timeout config) $
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
