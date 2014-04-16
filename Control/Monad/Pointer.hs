{-# LANGUAGE TypeOperators, KindSignatures, Rank2Types, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds, UndecidableInstances #-}


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
stackFirst :: (mt1 ::> stackrest) a -> mt1 stackrest a
stackFirst (PendingStackCell action) = action
-}

{-# INLINE (<::) #-}
(<::) :: (mt1 :: (* -> *) -> * -> *) (stackrest :: * -> *) a -> (mt1 stackrest a -> c) -> c
action <:: f = f action

infixl 6 <::

--class PointToBase 

class (Functor stack, Applicative stack, Monad stack)
  => PointableIn (stack :: * -> *) (mttarget :: (* -> *) -> * -> *) where
  mpoint :: (forall (m :: * -> *) . (Functor m, Applicative m, Monad m) => mttarget m a) -> stack a

instance (Functor stackrest, Applicative stackrest, Monad stackrest,
          Functor (mt1 stackrest), Applicative (mt1 stackrest), Monad (mt1 stackrest))
         => PointableIn (mt1 stackrest) mt1 where
  {-# INLINE mpoint #-}
  mpoint action = action

instance (PointableIn stackrest mt2, MonadTrans mt1,
          Functor stackrest, Applicative stackrest, Monad stackrest,
          Functor (mt1 stackrest), Applicative (mt1 stackrest), Monad (mt1 stackrest))
         => PointableIn (mt1 stackrest) mt2 where
  {-# INLINE mpoint #-}
  mpoint action = lift (mpoint action)

type At mt a = (Functor m, Applicative m, Monad m) => mt m a
-- ^ Prevents sharing of the m type!


type family MTSetGetConstraints
            cst (stack :: * -> *)
            (l :: [(* -> *) -> * -> *]) where
  MTSetGetConstraints cst stack (mt ': rest) =
                                 MTSetGetConstraints (cst, PointableIn stack mt) stack rest
  MTSetGetConstraints cst stack '[] = cst

type MTSet (l :: [(* -> *) -> * -> *]) (stack :: * -> *) =
  (Functor stack, Applicative stack, Monad stack, MTSetGetConstraints () stack l)


test :: (MTSet '[StateT Int, ReaderT Double] m, MonadIO m) => m String
test = do x <- mpoint $ helper 42
          liftIO $ print x
          return (show (x::Double))

helper :: (Num t) => t -> At (ReaderT t) t
helper x = (*x) <$> ask

x :: IO (String, Int)
x = test <:: flip runStateT (3::Int) <:: flip runReaderT (10::Double) <:: flip runReaderT (1::Double)
