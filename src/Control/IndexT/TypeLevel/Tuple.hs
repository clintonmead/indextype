{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-|
Type family and class definitions for dealing with tuples.

"Control.IndexT.Tuple" but with tuples raised to the type level and polykinded.
-}

module Control.IndexT.TypeLevel.Tuple (
  TupleConstraint,
  HomoTupleConstraint,
  IsTuple,
  IsHomoTuple
  )
where

import Control.IndexT.TypeLevel (IndexT)

import GHC.TypeLits (Nat)
import GHC.Exts (Constraint)
import Data.Functor.Identity (Identity(Identity))
import Data.Type.Equality (type (~~))
{-|
Type level version of 'Control.IndexT.TupleConstraint'
-}
type family TupleConstraint (n :: Nat) (a :: k) :: Constraint
type instance TupleConstraint 0 a = a ~~ '()
type instance TupleConstraint 2 a = a ~~ '(IndexT 0 a, IndexT 1 a)
type instance TupleConstraint 3 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a)
type instance TupleConstraint 4 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a)
type instance TupleConstraint 5 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a)
type instance TupleConstraint 6 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a)
type instance TupleConstraint 7 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a)
type instance TupleConstraint 8 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a)
type instance TupleConstraint 9 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a)
type instance TupleConstraint 10 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a)
type instance TupleConstraint 11 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a)
type instance TupleConstraint 12 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a, IndexT 11 a)
type instance TupleConstraint 13 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a, IndexT 11 a, IndexT 12 a)
type instance TupleConstraint 14 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a, IndexT 11 a, IndexT 12 a, IndexT 13 a)
type instance TupleConstraint 15 a = a ~~ '(IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a, IndexT 11 a, IndexT 12 a, IndexT 13 a, IndexT 14 a)



{-|
'HomoTupleConstraint' simply further constrains 'TupleConstraint' so that all the elements are the same.

So @HomoTupleConstraint 3 t@ basically says @t ~ (u,u,u)@ for some @u@,

(\"Homo\" is short for \"Homogeneous\". As in, all the same. Or like milk.)
-}
type family HomoTupleConstraint (n :: Nat) a :: Constraint
type instance HomoTupleConstraint 0 a = (TupleConstraint 0 a)
type instance HomoTupleConstraint 1 a = (TupleConstraint 1 a)
type instance HomoTupleConstraint 2 a = (TupleConstraint 2 a, IndexT 0 a ~ IndexT 1 a)
type instance HomoTupleConstraint 3 a = (TupleConstraint 3 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a)
type instance HomoTupleConstraint 4 a = (TupleConstraint 4 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a)
type instance HomoTupleConstraint 5 a = (TupleConstraint 5 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a)
type instance HomoTupleConstraint 6 a = (TupleConstraint 6 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a)
type instance HomoTupleConstraint 7 a = (TupleConstraint 7 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a)
type instance HomoTupleConstraint 8 a = (TupleConstraint 8 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a)
type instance HomoTupleConstraint 9 a = (TupleConstraint 9 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a)
type instance HomoTupleConstraint 10 a = (TupleConstraint 10 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a)
type instance HomoTupleConstraint 11 a = (TupleConstraint 11 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a)
type instance HomoTupleConstraint 12 a = (TupleConstraint 12 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a, IndexT 10 a ~ IndexT 11 a)
type instance HomoTupleConstraint 13 a = (TupleConstraint 13 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a, IndexT 10 a ~ IndexT 11 a, IndexT 11 a ~ IndexT 12 a)
type instance HomoTupleConstraint 14 a = (TupleConstraint 14 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a, IndexT 10 a ~ IndexT 11 a, IndexT 11 a ~ IndexT 12 a, IndexT 12 a ~ IndexT 13 a)
type instance HomoTupleConstraint 15 a = (TupleConstraint 15 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a, IndexT 10 a ~ IndexT 11 a, IndexT 11 a ~ IndexT 12 a, IndexT 12 a ~ IndexT 13 a, IndexT 13 a ~ IndexT 14 a)

{-|
GHC does not allow you to partially apply type families (or any type declaration for that matter).
So if you have a type of @* -> Constraint@ you can't pass @TupleConstraint 2@, because 'TupleConstraint' is partially
applied and this is not allowed.

But you can partially apply classes.

So 'IsTuple' is basically the same as 'TupleConstraint' except that it's a class, not a type family.
-}
class (TupleConstraint n a) => IsTuple n a
instance (TupleConstraint n a) => IsTuple n a

{-|
The version of 'IsTuple' for homogenous tuples (i.e. all the same type).
-}
class (HomoTupleConstraint n a) => IsHomoTuple n a
instance (HomoTupleConstraint n a) => IsHomoTuple n a
