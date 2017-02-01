{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds       #-}
-- |
-- Module      :  Web.Bot.Platform.FBMessenger
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Facebook Messenger bot API support.
--
module Web.Bot.Platform.FBMessenger (FBMessenger) where

import Crypto.Hash (hash, Digest, Keccak_256)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai.Handler.Warp (run)
import Control.Exception (throwIO)
import System.Environment (getEnv)
import qualified Data.Text as T
import Control.Concurrent.Chan
import Web.FBMessenger.API.Bot
import Control.Monad (forever)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Proxy
import Servant

import Web.Bot.Platform
import Web.Bot.Message
import Web.Bot.User
import Web.Bot.Log

-- | Facebook Messenger platform API
data FBMessenger

instance Platform FBMessenger where
    trySelf        = _trySelf
    sendMessage    = _sendMessage
    messageHandler = _messageHandler
    platformName   = const "Facebook Messenger platform"

_trySelf :: APIToken a => Bot a ()
_trySelf = do
    name <- fmap platformName returnPlatform
    $logInfoS "FBMessenger" ("Platform: " <> name)

    page <- pageToken
    manager <- getManager
    res <- liftIO $ subscribedApps (Just page) manager
    case res of
        Left e -> $logErrorS "FBMessenger" (T.pack $ show e)
        Right s ->
            if subscription_success s
               then $logInfoS "FBMessenger" "Subscription ENABLED"
               else $logWarnS "FBMessenger" "Subscription DISABLED"
  where returnPlatform :: Bot a a
        returnPlatform = return undefined

pageToken :: APIToken a => Bot a Token
pageToken = fmap (Token . head . T.split (== ':')) apiToken

verifyToken :: APIToken a => Bot a Text
verifyToken = fmap ((!! 1) . T.split (== ':')) apiToken

type WebHookAPI =
    "webhook" :> QueryParam "hub.verify_token" Text :>
                 QueryParam "hub.challenge" Text :> Get '[PlainText] Text :<|>
    "webhook" :> ReqBody '[JSON] RemoteEventList :> Post '[PlainText,JSON] Text

webHookAPI :: Proxy WebHookAPI
webHookAPI = Proxy

server :: Text -> Chan EventMessage -> Server WebHookAPI
server token ch = webhook_verify :<|> webhook_message
  where
    webhook_verify :: Maybe Text -> Maybe Text -> ExceptT ServantErr IO Text
    webhook_verify (Just verifyToken) (Just challenge)
        | verifyToken == token = return challenge
    webhook_verify tk ch = throwError err500

    webhook_message :: RemoteEventList -> ExceptT ServantErr IO Text
    webhook_message (RemoteEventList res) = do
        liftIO $ writeList2Chan ch (concatMap evt_messaging res)
        return "{\"status\":\"fulfilled\"}"

sha3 :: Text -> Text
sha3 x = T.pack (show digest)
  where digest :: Digest Keccak_256
        digest = hash (encodeUtf8 x)

_messageHandler :: APIToken a
                => (User -> Message -> Bot a b)
                -> Bot a c
_messageHandler handler = do
    chan  <- liftIO newChan
    token <- verifyToken
    port  <- liftIO (getEnv "PORT")

    forkBot $ liftIO $
        run (read port) $ serve webHookAPI (server token chan)

    page    <- pageToken
    manager <- getManager
    forever $ do
        ev <- liftIO $ readChan chan
        res <- liftIO $ getUserProfileInfo (Just page) (evtSenderId ev) manager
        let ident = sha3 $ "fbmessenger-" <> evtSenderId ev
            user = User (read $ T.unpack $ evtSenderId ev)
                        (case res of
                          Right profile -> usr_first_name profile
                                        <> usr_last_name profile
                          Left _ -> evtSenderId ev)
                        ident
        case evtContent ev of
            EmTextMessage _ _ t -> handler user (MsgText t)
            EmPostback t        -> handler user (MsgText t)
            _ -> return undefined
        return ()

_sendMessage :: (ToMessage msg, APIToken a) => User -> msg -> Bot a ()
_sendMessage user msg = do
    manager <- getManager
    token   <- pageToken
    res     <- liftIO (send manager token $ toMessage msg)
    case res of
        Right _ -> return ()
        Left e -> do $logErrorS "FBMessenger" ("Failure send to "
                        <> userName user <> " with " <> T.pack (show e))
                     _sendMessage user msg
  where chat = T.pack $ show $ userChat user
        send mgr tok (MsgText text) = do
            let (Just rcpt) = recipient (Just chat) Nothing
                messageReq  = sendTextMessageRequest Nothing rcpt text
            sendTextMessage (Just tok) messageReq mgr

        send mgr tok (MsgKeyboard text btns) = do
            let (Just rcpt) = recipient (Just chat) Nothing
                messageReq = sendButtonTemplateMessageRequest Nothing rcpt text $
                    zipWith postbackButton (concat btns) (concat btns)
            sendStructuredMessage (Just tok) messageReq mgr

        send _ _ _ = return (Right undefined)
