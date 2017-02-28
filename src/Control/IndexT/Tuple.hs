{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Type family and class definitions for dealing with tuples.

See the "Control.IndexT" module description for an overview.
-}

module Control.IndexT.Tuple (
  TupleN,
  TupleConstraint,
  HomoTupleConstraint,
  IsTuple,
  IsHomoTuple
  )
where

import Control.IndexT (IndexT)

import GHC.TypeLits (Nat)
import GHC.Exts (Constraint)
import Data.Functor.Identity (Identity)

{-|
'TupleN' seems a bit weird, but it's an important part of defining constraints that allow one to say "@t@ is a pair"
in 'TupleConstraint'.
-}
type family TupleN (n :: Nat) a
type instance TupleN 0 a = ()
type instance TupleN 1 a = Identity a
type instance TupleN 2 a = (IndexT 0 a, IndexT 1 a)
type instance TupleN 3 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a)
type instance TupleN 4 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a)
type instance TupleN 5 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a)
type instance TupleN 6 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a)
type instance TupleN 7 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a)
type instance TupleN 8 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a)
type instance TupleN 9 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a)
type instance TupleN 10 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a)
type instance TupleN 11 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a)
type instance TupleN 12 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a, IndexT 11 a)
type instance TupleN 13 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a, IndexT 11 a, IndexT 12 a)
type instance TupleN 14 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a, IndexT 11 a, IndexT 12 a, IndexT 13 a)
type instance TupleN 15 a = (IndexT 0 a, IndexT 1 a, IndexT 2 a, IndexT 3 a, IndexT 4 a, IndexT 5 a, IndexT 6 a, IndexT 7 a, IndexT 8 a, IndexT 9 a, IndexT 10 a, IndexT 11 a, IndexT 12 a, IndexT 13 a, IndexT 14 a)

{-|
To best explain this, lets consider the particular example @TupleConstraint 2@.

As @TupleN 2 t = (IndexT 0 t, IndexT 1 t)@ we get:

> TupleConstraint 2 t = t ~ (IndexT 0 t, IndexT 1 t)

What does this say? Well, firstly, as @t ~ (IndexT 0 t, IndexT 1 t)@, it must be a pair at least.

What are the elements of the pair? Well, the first element of @t@ is @IndexT 0 t@.

And what's @IndexT 0 t@ defined as? The first element of @t@.

So we know that the first element of @t@ is well, the first element of @t@.

Which tells us nothing at all.

We can go through the same argument with the second element of @t@.

So all we know after this is that @t@ is a pair. @TupleConstraint 2 t@ is the same as saying @t@ is a pair.

So @TupleConstraint n t@ basically says @t@ is a n-tuple.
-}
type TupleConstraint (n :: Nat) a = a ~ TupleN n a

{-|
'HomoTupleConstraint' simply further constrains 'TupleConstraint' so that all the elements are the same.

So @HomoTupleConstraint 3 t@ basically says @t ~ (u,u,u)@ for some @u@,

("Homo" is short for "Homogeneous". Like milk.)
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

