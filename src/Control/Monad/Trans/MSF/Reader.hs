{-# LANGUAGE RecordWildCards #-}

module Control.Monad.Trans.MSF.Reader where

-- transformers
import Control.Monad.Trans.Reader

-- dunai-live
import Data.MonadicStreamFunction

readerS :: Monad m => MSF m (r, a) b -> MSF (ReaderT r m) a b
readerS Cell { .. } = Cell { cellStep = \s a -> ReaderT $ \r -> cellStep s (r, a), .. }
readerS ArrM { .. } = ArrM { runArrM = \a -> ReaderT $ \r -> runArrM (r, a) }

runReaderS :: Monad m => MSF (ReaderT r m) a b -> MSF m (r, a) b
runReaderS Cell { .. } = Cell { cellStep = \s (r, a) -> runReaderT (cellStep s a) r, .. }
runReaderS ArrM { .. } = ArrM { runArrM = \(r, a) -> runReaderT (runArrM a) r }
