{-# LANGUAGE TypeOperators, KindSignatures, Rank2Types, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, PolyKinds #-}
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

type MA m = (Monad m, Applicative m)
-- ^ Just an alias until GHC 7.10 comes out & Applicative becomes a
-- superclass of Monad

type family StackToList (stack :: * -> *) where
  StackToList (mt1 stackrest) = mt1 ': StackToList stackrest
  StackToList lastmonad = '[]

type family (:&&) (a :: Bool) (b :: Bool) :: Bool where
  True :&& True = True
  _a   :&& _b   = False

type family Not (a :: Bool) :: Bool where
  Not True = False
  Not False = True

type family MemberOf (l :: [k]) (x :: k) :: Bool where
  MemberOf '[] _x = False
  MemberOf (x ': _xs) x = True
  MemberOf (_y ': xs) x = MemberOf xs x

type family AllDifferent (l :: [k]) :: Bool where
  AllDifferent '[] = True
  AllDifferent (x ': rest) = Not (MemberOf rest x) :&& AllDifferent rest

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


type family MTSetGetConstraints
            cst (stack :: * -> *)
            (l :: [(* -> *) -> * -> *]) where
  MTSetGetConstraints cst stack (mt ': rest) =
                                 MTSetGetConstraints (cst, PointableIn stack mt) stack rest
  MTSetGetConstraints cst stack '[] = cst

type MTSet (l :: [(* -> *) -> * -> *]) (stack :: * -> *) =
  (MA stack, MTSetGetConstraints () stack l)


test :: (MTSet '[StateT Int, ReaderT Double] m, MonadIO m) => m String
test = do x <- mpoint $ helper 42
          y <- mpoint get
          liftIO $ print x
          return (show $ (x::Double) + fromIntegral (y::Int))

helper :: (Num t, MA m) => t -> ReaderT t m t
helper x = (*x) <$> ask

x :: IO (String, Int)
x = test <:: flip runStateT (3::Int) <:: flip runReaderT (10::Double) <:: flip runReaderT (1::Int)
