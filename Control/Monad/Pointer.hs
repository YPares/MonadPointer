{-# LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Pointer where

import Control.Monad.IO.Class
import Control.Applicative

import Control.Monad.Pointer.PointableIn
import Control.Monad.Pointer.TypeFns

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader


test :: (MTSet '[StateT Int, ReaderT Double] m, MonadIO m) => m String
test = do x <- mpoint $ helper 42
          y <- mpoint get
          liftIO $ print x
          return (show $ (x::Double) + fromIntegral (y::Int))

--this signature is not necessary, given the Double type hint in test
--helper :: (MA m) => Double -> ReaderT Double m Double
helper x = (*x) <$> ask

-- ^ If the type signature is removed, type checker will complain
-- because it won't have the proof that downstack the 3 transformers
-- required are not duplicated. The error message is a bit long &
-- ugly, but this is what it means.
exec :: IO (String, Int)
exec = test <:: flip runStateT (3::Int) <:: flip runReaderT (10::Double) <:: flip runReaderT (1::Int)
