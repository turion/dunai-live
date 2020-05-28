{-# LANGUAGE RecordWildCards #-}

module Control.Monad.Trans.MSF.State
  ( module Control.Monad.Trans.MSF.State
  , module X
  ) where

-- base
import Data.Data

-- transformers
import Control.Monad.Trans.State as X

-- dunai-live
import Data.MonadicStreamFunction

runStateS_
  :: (Data s, Monad m)
  => MSF (StateT s m) a     b
  ->             s
  -> MSF           m  a (s, b)
runStateS_ Cell { .. } s0 = feedback s0 $ Cell
  { cellStep  = \state (a, s) -> do
      ((b, state'), s') <- runStateT (cellStep state a) s
      return (((s', b), s'), state')
  , ..
  }
