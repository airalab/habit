{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Web.Bot.Platform.Telegram
-- Copyright   :  Alexander Krupenkin 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Telegram bot API support.
--
module Web.Bot.Platform.Telegram (Telegram) where

import qualified Web.Telegram.API.Bot as API
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Web.Bot.Platform
import Web.Bot.Message
import Web.Bot.User
import Web.Bot.Log

-- | Telegram Bot API 2.0 platform
data Telegram

instance Platform Telegram where
    trySelf        = _trySelf
    sendMessage    = _sendMessage
    messageHandler = _messageHandler
    platformName   = const "Telegram Bot API 2.0"

_trySelf :: APIToken a => Bot a ()
_trySelf = do
    token   <- apiToken
    manager <- getManager
    self    <- liftIO (API.getMe (API.Token token) manager)
    name    <- fmap platformName returnPlatform
    $logInfoS "Telegram" ("Platform: " <> name)
    case self of
        Left e -> do $logErrorS "Telegram" ("Init failure: " <> T.pack (show e))
                     liftIO (throwIO e)
        Right (API.Response u) ->
            $logInfoS "Telegram" ("Init success, bot name: " <> API.user_first_name u)
  where returnPlatform :: Bot a a
        returnPlatform = return undefined

-- | Updates pull timeout in seconds
-- So it should be large number, but less TCP timeout
pullTimeout :: Num a => a
pullTimeout = 10

-- | Infinity loop for getting updates from API
_messageHandler :: APIToken a
                => (User -> Message -> Bot a b)
                -> Bot a c
_messageHandler handler = go 0
  where updates o t = API.getUpdates t (Just o) Nothing . Just
        go offset = do
            manager <- getManager
            token   <- apiToken
            -- Take updates
            upd <- liftIO (updates offset (API.Token token) pullTimeout manager)
            -- Check for errors
            case API.result <$> upd of
                Left e   -> do $logDebugS "Telegram"
                                          ("Pull updates failure: " <> T.pack (show e))
                               go offset
                Right [] -> go offset
                Right xs -> do
                    -- Run handler for any update
                    mapM_ (withUpdate handler) xs
                    -- Step for the new offset
                    go (maximum (API.update_id <$> xs) + 1)

withUpdate :: APIToken a
           => (User -> Message -> Bot a b)
           -> API.Update
           -> Bot a ()
withUpdate f update = case go of
    Just (user, msg) -> f user msg >> return ()
    Nothing -> return ()
  where
    go = (,) <$> mkUser <*> mkMessage
    formatUserName u = API.user_first_name u <>
        case API.user_last_name u of
            Just last_name -> " " <> last_name
            Nothing -> ""
    mkUser = User <$> (fmap (API.chat_id . API.chat) $ API.message update)
                  <*> (fmap formatUserName (API.message update >>= API.from))
                  -- TODO: User identity
                  <*> return ""
    mkMessage = MsgText <$> (API.message update >>= API.text)

_sendMessage :: (ToMessage msg, APIToken a) => User -> msg -> Bot a ()
_sendMessage user msg = do
    manager <- getManager
    token   <- apiToken
    res <- liftIO (send (API.Token token) manager $ toMessage msg)
    case res of
        Right _ -> return ()
        Left e -> do $logErrorS "Telegram" ("Failure send to " <> userName user
                                            <> " with " <> T.pack (show e))
                     _sendMessage user msg
  where cid = T.pack $ show $ userChat user

        send tok mgr MsgTyping =
            let r = API.sendChatActionRequest cid API.Typing
             in API.sendChatAction tok r mgr >>= return . fmap (const ())

        send tok mgr (MsgText t) =
            let r = (API.sendMessageRequest cid t)
                  { API.message_reply_markup = Just API.replyKeyboardHide
                  , API.message_parse_mode   = Just API.Markdown }
             in API.sendMessage tok r mgr >>= return . fmap (const ())

        send tok mgr (MsgKeyboard txt btnTexts) =
            let btns     = fmap API.keyboardButton <$> btnTexts
                keyboard = API.replyKeyboardMarkup btns
                r = (API.sendMessageRequest cid txt)
                  { API.message_reply_markup = Just keyboard
                  , API.message_parse_mode   = Just API.Markdown }
             in API.sendMessage tok r mgr >>= return . fmap (const ())
