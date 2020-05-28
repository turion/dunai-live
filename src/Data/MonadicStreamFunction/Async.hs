{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Data.MonadicStreamFunction.Async (concatS) where

-- base
import Data.Data

-- essence-of-live-coding
import LiveCoding.Cell

-- dunai-live
import Data.MonadicStreamFunction

concatS
  :: (Monad m, Data b)
  => MSF   m a [b]
  -> MSF   m a  b
concatS Cell { cellStep = innerStep, .. } = Cell { cellState = Concat { buffer = ([] :: [b]), .. }, .. }
  where
    cellStep Concat { .. } a = do
      case buffer of
        (b : bs) -> return (b, Concat { buffer = bs, .. })
        []       -> do
          (bs, cellState) <- innerStep cellState a
          cellStep Concat { .. } a
concatS cell@ArrM {} = concatS $ toCell cell

data Concat s b = Concat
  { cellState :: s
  , buffer    :: [b]
  }
  deriving Data
