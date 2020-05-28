{-# LANGUAGE RecordWildCards #-}

module Control.Monad.Trans.MSF.Writer
  ( module Control.Monad.Trans.MSF.Writer
  , module X
  ) where

-- base
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Writer as X

-- dunai-live
import Data.MonadicStreamFunction

runWriterS
  :: Monad          m
  => MSF (WriterT w m) a     b
  -> MSF            m  a (w, b)
runWriterS Cell { .. } = Cell { cellStep = \s a -> fmap (\((b, s), w) -> ((w, b), s)) $ runWriterT $ cellStep s a, .. }
runWriterS ArrM { .. } = ArrM { runArrM = fmap swap . runWriterT . runArrM }
