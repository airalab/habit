-- |
-- Module      :  Web.Bot.Story
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Story is a dialog like abstraction for processing sparsed
-- by messages data from user.
--
-- @
--      hello :: Monad m => Text -> Text -> Int -> m BotMessage
--      hello name surname age = toMessage $
--          "Hello, " <> name <> " " <> surname <> "!\n"
--       <> "You lost " <> (T.pack $ show age) <> " years =)"
--
--      helloStory :: BotConfig a => Story a
--      helloStory _ = hello <$> question "How your name?"
--                           <*> question "How your surname?"
--                           <*> question "How old are you?"
-- @
--
module Web.Bot.Story where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Text.Read (signed, decimal, double)
import Control.Monad.IO.Class (MonadIO)
import Pipes (Pipe, await, yield)
import qualified Data.Text as T
import Data.Text (Text)

import Web.Bot.Message (Message(..), ToMessage(..))
import Web.Bot.Platform (Bot)
import Web.Bot.User (User)

-- | Story is a pipe from user message to bot message
-- and result is a final bot message.
type Story a = User -> StoryT (Bot a) Message

-- | Story transformer is based on 'Pipe'
-- with fixed 'Message' in/out.
type StoryT = Pipe Message Message

-- | User message reply parser.
class Answer a where
    parse :: MonadIO m => Message -> ExceptT Text m a

instance Answer Text where
    parse (MsgText x) = return x
    parse _           = throwE "Please send text message."

instance Answer Double where
    parse x = do
        t <- parse x
        case signed double t of
            Left e -> throwE (T.pack e)
            Right (v, "") -> return v
            Right _       -> throwE "Please use only 0-9 and '.' chars."

instance Answer Integer where
    parse x = do
        t <- parse x
        case signed decimal t of
            Left e -> throwE (T.pack e)
            Right (v, x) -> if T.null x
                            then return v
                            else throwE "Please use only 0-9 chars."

instance Answer Int where
    parse x = do
        v <- parse x
        return (fromIntegral (v :: Integer))

instance Answer Word where
    parse x = do
        v <- parse x
        return (fromIntegral (v :: Integer))

-- | Reply keyboard selection
select :: (MonadIO m, Answer a) => Text -> [[Text]] -> StoryT m a
{-# INLINE select #-}
select q = replica . MsgKeyboard q

-- | Bot text question.
question :: (MonadIO m, Answer a) => Text -> StoryT m a
{-# INLINE question #-}
question = replica

-- | Generalized story maker.
-- The replica send message to user, when answer isn't parsed
-- the error be sended and wait for correct answer.
replica :: (ToMessage a, MonadIO m, Answer b) => a -> StoryT m b
replica msg = do
    yield (toMessage msg)
    res <- runExceptT . parse =<< await
    yield MsgTyping
    case res of
        Left e  -> replica e
        Right a -> return a
