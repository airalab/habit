-- |
-- Module      :  Web.Bot.Message
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Common used message type.
--
module Web.Bot.Message where

import Data.String (IsString(..))
import Data.Text (Text)

-- | Generalized message type
data Message
  = MsgTyping
  -- ^ When is typing
  | MsgText Text
  -- ^ Simple text
  | MsgKeyboard Text [[Text]]
  -- ^ Interactive keyboard
  deriving (Eq, Ord, Show)

instance IsString Message where
    fromString = MsgText . fromString

-- | Convert any data to message
class ToMessage a where
    toMessage :: a -> Message

-- | Idenity instance
instance ToMessage Message where
    toMessage = id

instance ToMessage Text where
    toMessage = MsgText
