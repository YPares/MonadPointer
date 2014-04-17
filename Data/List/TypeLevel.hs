{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, PolyKinds, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.List.TypeLevel where

import Data.Type.Equality


type family (:&&) (a :: Bool) (b :: Bool) :: Bool where
  True :&& True = True
  _a   :&& _b   = False

type family Not (a :: Bool) :: Bool where
  Not True = False
  Not False = True

type family MemberOf (l :: [k]) (x :: k) :: Bool where
  MemberOf '[] _x = False
  MemberOf (x ': _xs) x = True
  MemberOf (y ': xs) x = Not (y == x) :&& MemberOf xs x

type family AllDifferent (l :: [k]) :: Bool where
  AllDifferent '[] = True
  AllDifferent (x ': xs) = Not (MemberOf xs x) :&& AllDifferent xs

type family ExactlyOnceIn (l :: [k]) x :: Bool where
  ExactlyOnceIn '[] _x = False
  ExactlyOnceIn (x ': xs) x = Not (MemberOf xs x)
  ExactlyOnceIn (y ': xs) x = Not (y == x) :&& ExactlyOnceIn xs x
