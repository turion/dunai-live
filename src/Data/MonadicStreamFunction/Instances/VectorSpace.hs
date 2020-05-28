{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Data.MonadicStreamFunction.Instances.VectorSpace where

-- base
import Control.Arrow

-- simple-affine-space
import Data.VectorSpace

-- dunai-live
import Data.MonadicStreamFunction

-- | Vector-space instance for 'MSF's.
instance (Monad m, VectorSpace v s) => VectorSpace (MSF m a v) s where
  zeroVector   = constantly zeroVector
  r   *^ msf   = msf >>^ (r *^)
  msf ^/ r     = msf >>^ (^/ r)
  (^+^)        = elementwise2 (^+^)
  (^-^)        = elementwise2 (^-^)
  negateVector = (>>^ negateVector)
