{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
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
--      hello name surname age = return . toMessage $
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

import Control.Exception (try, SomeException, displayException)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Web.Telegram.API.Bot (Message, text)
import Pipes (Pipe, await, yield, lift)
import Data.Text (Text, unpack, pack)
import Text.Read (readMaybe)

-- | Story is a pipe from Message to question
-- and result is a final message.
type Story  = Int -> StoryT IO (IO BotMessage)
type StoryT = Pipe Message BotMessage

-- | Bot replica message.
data BotMessage
  = BotTyping
  | BotText Text
  | BotKeyboard (Text, [[Text]])

-- | Bot question conversion typeclass.
class Question a where
    toMessage :: a -> BotMessage

-- | Idenity instance
instance Question BotMessage where
    toMessage = id

-- | Simple text question send text message from bot
instance Question Text where
    toMessage = BotText

-- | The value can be passed to story handler function.
class Answer a where
    parse :: (MonadError Text m, MonadIO m) => Message -> m a

-- | Simple text answer, pass any text message
instance Answer Text where
    parse x =
        case text x of
            Just t  -> return t
            Nothing -> throwError "Please send text message."

instance Answer Double where
    parse x = do
        t <- parse x
        case readMaybe (unpack t) of
            Just v -> return v
            Nothing -> throwError "Please send floating value."

instance Answer Float where
    parse x = do
        v <- parse x
        return (realToFrac (v :: Double))

instance Answer Integer where
    parse x = do
        t <- parse x
        case readMaybe (unpack t) of
            Just v -> return v
            Nothing -> throwError "Please send integer value."

instance Answer Int where
    parse x = do
        v <- parse x
        return (fromIntegral (v :: Integer))

instance Answer Word where
    parse x = do
        v <- parse x
        return (fromIntegral (v :: Integer))

-- | Lift 'BotMessage' generator into pipe
liftAction :: IO BotMessage -> Pipe a BotMessage IO ()
liftAction gen = do
    yield BotTyping
    lift (handleError <$> try gen) >>= yield
  where handleError :: Either SomeException BotMessage -> BotMessage
        handleError (Right a) = a
        handleError (Left e) = BotText (pack $ displayException e)

-- | Reply keyboard selection
select :: Answer a => IO (Text, [[Text]]) -> StoryT IO a
select = question' . fmap BotKeyboard

-- | 'Text' message question maker
question :: Answer a => IO Text -> StoryT IO a
question = question'

-- | Generalized question story maker.
-- The question send to user, when answer isn't parsed
-- the error send to user and waiting for correct answer.
question' :: (Question q, Answer a) => IO q -> StoryT IO a
question' mq = do
    liftAction (toMessage <$> mq)
    res <- lift . runExceptT . parse =<< await
    case res of
        Left e  -> question (return e)
        Right a -> return a

-- | Return value wrapped with 'BotMessage'
returnQ :: (Monad m, Question q) => q -> m BotMessage
returnQ = return . toMessage
