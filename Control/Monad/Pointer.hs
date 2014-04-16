{-# LANGUAGE DataKinds, ConstraintKinds #-}

module Control.Monad.Pointer where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative

import Control.Monad.Pointer.PointableIn
import Control.Monad.Pointer.TypeFns

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Functor.Identity


test :: (MTSet '[StateT Int, ReaderT Double] m, MonadIO m) => m String
test = do x <- mpoint $ helper 42
          y <- mpoint get
          liftIO $ print x
          return (show $ (x::Double) + fromIntegral (y::Int))

helper :: (Num t, MA m) => t -> ReaderT t m t
helper x = (*x) <$> ask

x :: IO (String, Int)
x = test <:: flip runStateT (3::Int) <:: flip runReaderT (10::Double) <:: flip runReaderT (1::Int)
