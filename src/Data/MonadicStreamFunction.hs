{-# LANGUAGE Arrows #-}

module Data.MonadicStreamFunction
  ( module Data.MonadicStreamFunction
  , module X
  ) where

-- base
import Control.Arrow as X
import qualified Control.Category as Category
import Data.Data
import Data.Tuple (swap)

-- simple-affine-space
import Data.VectorSpace

-- essence-of-live-coding
import LiveCoding.Cell as X hiding (step, Parallel, Sensor)
import LiveCoding.Cell.Feedback as X hiding (Feedback)

-- dunai-live
import Data.MonadicStreamFunction.InternalCore as X

mapMaybeS :: Monad m => MSF m a b -> MSF m (Maybe a) (Maybe b)
mapMaybeS msf = proc maybeA -> case maybeA of
  Just a  -> arr Just <<< msf -< a
  Nothing -> returnA          -< Nothing

iPre firsta = feedback firsta $ arr swap

sumS :: (Monad m, Num a, Data a) => MSF m a a
sumS = sumC

count :: (Monad m, Num b, Data b) => MSF m a b
count = arr (const 1) >>> sumS

withSideEffect :: Monad m => (a -> m b) -> MSF m a a
withSideEffect method = (Category.id &&& arrM method) >>> arr fst

accumulateWith :: (Data s, Monad m) => (a -> s -> s) -> s -> MSF m a s
accumulateWith f s0 = feedback s0 $ arr g
  where
    g (a, s) = let s' = f a s in (s', s')

unfold :: (Data a, Monad m) => (a -> (b, a)) -> a -> MSF m () b
unfold f a = feedback a (arr (snd >>> f))

constantly :: Arrow a => b -> a c b
constantly = arr . const

elementwise2 :: Arrow a => (c -> d -> e) -> a b c -> a b d -> a b e
elementwise2 op a1 a2 = (a1 &&& a2) >>^ uncurry op

-- TODO Should go into essence-of-live-coding utils
sumFrom :: (Monad m, VectorSpace v s, Data v) => v -> Cell m v v
sumFrom n0 = feedback n0 $ proc (n, acc) -> returnA -< (acc, acc ^+^ n)

mappendFrom :: (Data n, Monoid n, Monad m) => n -> MSF m n n
mappendFrom = accumulateWith mappend

mappendS :: (Data n, Monoid n, Monad m) => MSF m n n
mappendS = mappendFrom mempty
