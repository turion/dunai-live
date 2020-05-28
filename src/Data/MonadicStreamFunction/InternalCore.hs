module Data.MonadicStreamFunction.InternalCore where

-- transformers
import Control.Monad.Trans.Class

-- essence-of-live-coding
import LiveCoding.Cell

type MSF m a b = Cell m a b

unMSF :: Monad m => MSF m a b -> a -> m (b, MSF m a b)
unMSF = step

morphS = hoistCell

liftTransS
  :: (Monad m, MonadTrans t)
  => MSF          m  a b
  -> MSF       (t m) a b
liftTransS = liftCell

reactimate
  :: Monad m
  => MSF m () ()
  -> m ()
reactimate cell = do
  (_, cell') <- unMSF cell ()
  reactimate cell'
