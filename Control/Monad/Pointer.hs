{-# LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

module Control.Monad.Pointer where

import Control.Monad.IO.Class
import Control.Applicative

import Control.Monad.Pointer.PointableIn
import Control.Monad.Pointer.TypeFns

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader


--test :: (MTSet '[StateT Int, ReaderT Double] m, MonadIO m) => m String
test = do x <- mpoint $ helper 42
          y <- mpoint get
          liftIO $ print x
          return (show $ (x::Double) + fromIntegral (y::Int))

--this signature is not necessary, given the Double type hint in test
--helper :: (MA m) => Double -> ReaderT Double m Double
helper x = (*x) <$> ask


--NoMonomorphismRestriction allow to remove exec type signature
--exec :: IO (String, Int)
exec = test <:: flip runStateT (3::Int) <:: flip runReaderT (10::Double) <:: flip runReaderT (1::Int)
