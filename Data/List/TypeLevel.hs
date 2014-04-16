{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, PolyKinds, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.List.TypeLevel where

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
