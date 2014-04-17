{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Pointer.PointableIn where

import Control.Monad.Trans.Class
import Control.Applicative

import Data.List.TypeLevel


{-# INLINE (<::) #-}
(<::) :: (mt1 :: (* -> *) -> * -> *) (stackrest :: * -> *) a -> (mt1 stackrest a -> c) -> c
action <:: f = f action

infixl 6 <::

--class PointToBase 

type MA m = (Monad m, Applicative m)
-- ^ Just an alias until GHC 7.10 comes out & Applicative becomes a
-- superclass of Monad

type family OnTopOf (stack :: * -> *) (mttarget :: (* -> *) -> * -> *) where
  OnTopOf (mt1 stackrest) mt1 = True
  OnTopOf _s _m = False

type family StackToList (stack :: * -> *) :: [(* -> *) -> * -> *] where
  StackToList (mt1 stackrest) = mt1 ': StackToList stackrest
  StackToList lastmonad = '[]

-- The b bool tells if we reached the end of the recursion
class (MA stack, OnTopOf stack mttarget ~ flag, ExactlyOnceIn (StackToList stack) mttarget ~ True)
      => PointableIn (flag :: Bool) (stack :: * -> *) (mttarget :: (* -> *) -> * -> *) where
  mpoint :: (forall (m :: * -> *) . (MA m) => mttarget m a) -> stack a

instance (MemberOf (StackToList stackrest) mt1 ~ False, MA stackrest, MA (mt1 stackrest))
         => PointableIn True (mt1 stackrest) mt1 where
  {-# INLINE mpoint #-}
  mpoint action = action

instance (OnTopOf (mt1 stackrest) mt2 ~ False,
          ExactlyOnceIn (StackToList (mt1 stackrest)) mt2 ~ True,
          PointableIn (OnTopOf stackrest mt2) stackrest mt2,
          MonadTrans mt1, MA stackrest, MA (mt1 stackrest))
         => PointableIn False (mt1 stackrest) mt2 where
  {-# INLINE mpoint #-}
  mpoint action = lift (mpoint action)
  
