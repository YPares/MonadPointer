{-# LANGUAGE KindSignatures, DataKinds, ConstraintKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}

module Control.Monad.Pointer.TypeFns where

import Control.Monad.Pointer.PointableIn

type family MTSetGetConstraints
            cst (stack :: * -> *)
            (l :: [(* -> *) -> * -> *]) where
  MTSetGetConstraints cst stack (mt ': rest) =
                                 MTSetGetConstraints (cst, PointableIn (OnTopOf stack mt) stack mt) stack rest
  MTSetGetConstraints cst stack '[] = cst

type MTSet (l :: [(* -> *) -> * -> *]) (stack :: * -> *) =
  (MA stack, MTSetGetConstraints () stack l)
