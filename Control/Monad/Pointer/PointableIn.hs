{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE GADTs, UndecidableInstances #-}

module Control.Monad.Pointer.PointableIn where

import Control.Monad.Trans.Class
import Control.Applicative

import Data.Type.Equality

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
(<::) :: (MemberOf (StackToList stackrest) mt1 ~ False)
         => (mt1 :: (* -> *) -> * -> *) (stackrest :: * -> *) a -> (mt1 stackrest a -> c) -> c
action <:: f = f action

infixl 6 <::

--class PointToBase 

type MA m = (Monad m, Applicative m)
-- ^ Just an alias until GHC 7.10 comes out & Applicative becomes a
-- superclass of Monad

type family StackToList (stack :: * -> *) :: [(* -> *) -> * -> *] where
  StackToList (mt1 stackrest) = mt1 ': StackToList stackrest
  StackToList lastmonad = '[]

-- The b bool tells if we reached the end of the recursion
class (MA stack, ExactlyOnceIn (StackToList stack) mttarget ~ True)
      => PointableInClass (b :: Bool) (stack :: * -> *) (mttarget :: (* -> *) -> * -> *) where
  mpoint :: (forall (m :: * -> *) . (MA m) => mttarget m a) -> stack a

instance (MemberOf (StackToList stackrest) mt1 ~ False, MA stackrest, MA (mt1 stackrest))
         => PointableInClass True (mt1 stackrest) mt1 where
  {-# INLINE mpoint #-}
  mpoint action = action

instance (ExactlyOnceIn (StackToList (mt1 stackrest)) mt2 ~ True,
          PointableIn stackrest mt2, MonadTrans mt1,
          MA stackrest, MA (mt1 stackrest))
         => PointableInClass False (mt1 stackrest) mt2 where
  {-# INLINE mpoint #-}
  mpoint action = lift (mpoint action)

data SaneStackView (mt1 :: (* -> *) -> * -> *) (stackrest :: * -> *) (mttarget :: (* -> *) -> * -> *) where
  SSW :: (MemberOf (StackToList stackrest) mttarget ~ False)
         => SaneStackView mt1 stackrest mttarget

type family PointableIn (stack :: * -> *) (mttarget :: (* -> *) -> * -> *) where
  PointableIn (mt1 stackrest) mt1 = PointableInClass True (mt1 stackrest) mt1
  PointableIn (mt1 stackrest) mt2 = PointableInClass False (mt1 stackrest) mt2
