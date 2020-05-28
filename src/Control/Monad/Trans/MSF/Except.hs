{-# LANGUAGE Arrows #-}

module Control.Monad.Trans.MSF.Except
  ( module Control.Monad.Trans.MSF.Except
  , module X
  ) where

-- base
import Data.Data

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except as X

-- essence-of-live-coding
import LiveCoding.CellExcept as X
import LiveCoding.Exceptions
import LiveCoding.Exceptions.Finite as X

-- dunai-live
import Data.MonadicStreamFunction

type MSFExcept m a b e = CellExcept m a b e

throwS :: Monad m => MSF (ExceptT e m) e a
throwS = arrM throwE

once :: (Monad m, Data e, Finite e) => (a -> m e) -> MSFExcept m a b e
once f = try $ arrM (lift . f) >>> throwS

once_ :: (Monad m, Data e, Finite e) => m e -> MSFExcept m a b e
once_ = once . const

throwOn' :: Monad m => MSF (ExceptT e m) (Bool, e) ()
throwOn' = proc (b, e) -> if b
  then throwS  -< e
  else returnA -< ()

step :: (Monad m, Data e, Finite e) => (a -> m (b, e)) -> MSFExcept m a b e
step f = try $ proc a -> do
  n      <- sumC            -< 1
  (b, e) <- arrM (lift . f) -< a
  _      <- throwOn'        -< (n > (1 :: Int), e)
  returnA                   -< b

throwMaybe :: Monad m => MSF (ExceptT e m) (Maybe e) (Maybe a)
throwMaybe = mapMaybeS throwS

runMSFExcept :: Monad m => MSFExcept m a b e -> MSF (ExceptT e m) a b
runMSFExcept = runCellExcept

data Empty

exceptS :: (Data e, Monad m) => MSF (ExceptT e m) a b -> MSF m a (Either e b)
exceptS = runExceptC

currentInput :: (Data e, Finite e, Monad m) => MSFExcept m e b e
currentInput = try throwS
