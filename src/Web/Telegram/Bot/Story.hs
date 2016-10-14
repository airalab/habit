-- |
-- Module      :  Web.Telegram.Bot.Story
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Story is a dialog like abstraction for processing data from
-- user sparsed by messages.
--
-- @
--      hello :: Monad m => Text -> Text -> Int -> m BotMessage
--      hello name surname age = toMessage $
--          "Hello, " <> name <> " " <> surname <> "!\n"
--       <> "You lost " <> (pack $ show age) <> " years =)"
--
--      helloStory :: Story
--      helloStory _ = hello <$> question "How your name?"
--                           <*> question "How your surname?"
--                           <*> question "How old are you?"
-- @
--
module Web.Telegram.Bot.Story where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Web.Telegram.API.Bot (Message, Chat, text)
import Data.Text.Read (signed, decimal, double)
import Control.Monad.IO.Class (MonadIO)
import Pipes (Pipe, await, yield, lift)
import Web.Telegram.Bot.Types (Bot)
import Data.Text (Text, pack)

-- | Story is a pipe from user message to bot message
-- and result is a final message bot.
type Story  = Chat -> StoryT Bot BotMessage
type StoryT = Pipe Message BotMessage

-- | Bot message data.
data BotMessage
  = BotTyping
  | BotText Text
  | BotKeyboard Text [[Text]]
  deriving Show

-- | Bot message typeclass for conversion.
class ToBotMessage a where
    toMessage :: a -> BotMessage

-- | Idenity instance
instance ToBotMessage BotMessage where
    toMessage = id

-- | Simple text question send text message from bot
instance ToBotMessage Text where
    toMessage = BotText

-- | The value can be passed to story handler function.
class Answer a where
    parse :: MonadIO m => Message -> ExceptT Text m a

-- | Simple text answer, pass any text message
instance Answer Text where
    parse x =
        case text x of
            Just t  -> return t
            Nothing -> throwE "Please send text message."

instance Answer Double where
    parse x = do
        t <- parse x
        case signed double t of
            Left e -> throwE (pack e)
            Right (v, _) -> return v

instance Answer Integer where
    parse x = do
        t <- parse x
        case signed decimal t of
            Left e -> throwE (pack e)
            Right (v, _) -> return v

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
select q = replica . BotKeyboard q

-- | Bot text question.
question :: (MonadIO m, Answer a) => Text -> StoryT m a
{-# INLINE question #-}
question = replica

-- | Generalized story maker.
-- The replica send to user, when answer isn't parsed
-- the error send to user and waiting for correct answer.
replica :: (ToBotMessage a, MonadIO m, Answer b) => a -> StoryT m b
replica msg = do
    yield (toMessage msg)
    res <- lift . runExceptT . parse =<< await
    yield BotTyping
    case res of
        Left e  -> replica e
        Right a -> return a
