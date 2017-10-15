{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}

{-|
This module provides a way to constrain types to be data constructors, much like
"Control.IndexT.Tuple" and "Control.IndexT.Function".

It also provides type families for accessing the elements of those data constructors,
both the constructors themselves and the parameters to them.

Note I haven't yet wrote code to generate many instances for these, so currently only
constructors with up to two parameters is supported. Just nag me if your application
needs more.
-}
module Control.IndexT.Constructor (
  IndexC, IndexCK,
  -- $getConstructorDocs
  GetConstructor1,
  GetConstructor2,
  IsData
)
where

import GHC.TypeLits (Nat)
import GHC.Exts (Constraint)
import Data.Kind (Type)

{-|
> IndexC i n (f a_0 a_1 .. a_(n-1))

the ith (zero based) parameter of the constructor with n parameters, i.e. @a_i@

This however only allows constructors with parameters of type :: @Type@,
not of other kinds (see 'IndexCK')
-}
type IndexC (n :: Nat) (i :: Nat) a = IndexCK Type n i a

{-|
> Just like 'IndexC' but has an additional kind parameter
-}
type family IndexCK k (n :: Nat) (i :: Nat) a = (r :: k)

type instance IndexCK k 1 0 (_ (a :: k)) = a
type instance IndexCK k 2 0 (_ (a :: k) _) = a
type instance IndexCK k 2 1 (_ _ (a :: k)) = a



{- $getConstructorDocs
These functions actually get the constructor, Unfortunately these are separate named functions instead
of being indexed by @n@ because they have different kinds, i.e. 'GetConstructor1' is @* -> *@
whereas 'GetConstructor2' is @* -> * -> *@. If there's a better way of doing this let me know.
-}
type family GetConstructor1 a where
  GetConstructor1 (f _) = f

type family GetConstructor2 a where
  GetConstructor2 (f _ _) = f

type Data1 a = (GetConstructor1 a) (IndexC 1 0 a)
type IsData1 a = (a ~ Data1 a)

type Data2 a = (GetConstructor2 a) (IndexC 2 0 a) (IndexC 2 1 a)
type IsData2 a = (a ~ Data2 a)

{-|
Much like 'Control.IndexT.Tuple.IsTuple' and 'Control.IndexT.Function.IsFunction', @IsData m t@ asserts that
@t@ is a data constructor with @n@ variables.
-}
type family IsData (n :: Nat) a :: Constraint
type instance IsData 1 a = IsData1 a
type instance IsData 2 a = IsData2 a
