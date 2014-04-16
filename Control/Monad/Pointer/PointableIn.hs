{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, ConstraintKinds, OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Control.Monad.Pointer.PointableIn where

import Control.Monad.Trans.Class
import Control.Applicative

import Data.List.TypeLevel

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

type MA m = (Monad m, Applicative m)
-- ^ Just an alias until GHC 7.10 comes out & Applicative becomes a
-- superclass of Monad

type family StackToList (stack :: * -> *) where
  StackToList (mt1 stackrest) = mt1 ': StackToList stackrest
  StackToList lastmonad = '[]

type FreeFromDuplicates (stack :: * -> *) = AllDifferent (StackToList stack) ~ True

class (MA stack, FreeFromDuplicates stack)
      => PointableIn (stack :: * -> *) (mttarget :: (* -> *) -> * -> *) where
  mpoint :: (forall (m :: * -> *) . (MA m) => mttarget m a) -> stack a

instance (FreeFromDuplicates (mt1 stackrest), MA stackrest, MA (mt1 stackrest))
         => PointableIn (mt1 stackrest) mt1 where
  {-# INLINE mpoint #-}
  mpoint action = action

instance (FreeFromDuplicates (mt1 stackrest), PointableIn stackrest mt2, MonadTrans mt1,
          MA stackrest, MA (mt1 stackrest))
         => PointableIn (mt1 stackrest) mt2 where
  {-# INLINE mpoint #-}
  mpoint action = lift (mpoint action)
