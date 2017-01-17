-- |
-- Module      :  Web.Bot.Log
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Logging utils.
--
module Web.Bot.Log (
    logDebug
  , logInfo
  , logWarn
  , logError
  , logDebugS
  , logInfoS
  , logWarnS
  , logErrorS
) where

import Control.Monad.Logger
