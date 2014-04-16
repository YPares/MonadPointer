{-# LANGUAGE TypeOperators, KindSignatures, Rank2Types, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances, ScopedTypeVariables #-}

module Control.Monad.Pointer where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Functor.Identity

{-
newtype ListedT (tfs :: [(* -> *) -> * -> *]) m a = ListedT (m a)
  deriving (Functor, Monad, Applicative)

type family (::>) mt1 listedM2 where
  mt1 ::> (ListedT l m2 a) = ListedT (mt1 ': l) (mt1 m2) a

newtype (::>) (mt1 :: (* -> *) -> * -> *) (m2 :: * -> *) a = PendingStackCell (mt1 m2 a)
  deriving (Functor, Monad, Applicative, MonadIO)

infixr 6 ::>

{-# INLINE stackFirst #-}
stackFirst :: (mt1 ::> pstackrest) a -> mt1 pstackrest a
stackFirst (PendingStackCell action) = action
-}

{-# INLINE (<::) #-}
(<::) :: (mt1 :: (* -> *) -> * -> *) (pstackrest :: * -> *) a -> (mt1 pstackrest a -> c) -> c
action <:: f = f action

infixl 6 <::

--class PointToBase 

class PointableIn (pstack :: * -> *) (mttarget :: (* -> *) -> * -> *) where
  mpoint :: (forall (m :: * -> *) . (Functor m, Applicative m, Monad m) => mttarget m a) -> pstack a

instance (Functor pstackrest, Applicative pstackrest, Monad pstackrest)
         => PointableIn (mt1 pstackrest) mt1 where
  {-# INLINE mpoint #-}
  mpoint action = action

instance (PointableIn pstackrest mt2, MonadTrans mt1, Monad pstackrest)
         => PointableIn (mt1 pstackrest) mt2 where
  {-# INLINE mpoint #-}
  mpoint action = lift (mpoint action)

type At mt a = (Functor m, Applicative m, Monad m) => mt m a


test :: StateT Int (ReaderT Double (ReaderT Double IO)) String
test = do x <- mpoint $ helper 42
          liftIO $ print x
          return (show (x::Double))

helper :: (Num t) => t -> At (ReaderT t) t
helper x = (*x) <$> ask


x = test <:: flip runStateT 3 <:: flip runReaderT 10 <:: flip runReaderT 1
