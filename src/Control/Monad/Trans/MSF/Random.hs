module Control.Monad.Trans.MSF.Random where

-- base
import Data.Data

-- MonadRandom
import Control.Monad.Random

-- dunai-live
import Control.Monad.Trans.MSF.State
import Data.MonadicStreamFunction

runRandS :: (Data g, RandomGen g, Functor m, Monad m)
         => MSF (RandT g m) a b
         -> g -- ^ The initial random number generator.
         -> MSF m a (g, b)
runRandS = runStateS_ . morphS (StateT . runRandT)
