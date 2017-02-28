{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Type family and class definitions for dealing with functions.

Many of these definitions are similar to the ones that deal with tuples, in which case
it refers to the documentation in "Control.IndexT.Tuple".

Also see the "Control.IndexT" module description for an overview.
-}


module Control.IndexT.Function (
  ResultT,
  FuncN,
  FuncConstraint,
  HomoFuncConstraint,
  HomoArgFuncConstraint,
  IsFunc,
  IsHomoFunc,
  IsHomoArgFunc
  )
where

import Control.IndexT (IndexT)

import GHC.TypeLits (Nat)
import GHC.Exts (Constraint)


{-|
'ResultT' is used to get the result type of functions, which 'IndexT' can not be used for as discussed in
it's documentation.

@ResultT n t@ gets the result of @t@ treated as an @n@ argument function. For example:

> ResultT 2 (a -> b -> c) == c

note that:

> ResultT 2 (a -> b -> c -> d) == c -> d

which makes sense, as the result of applying two arguments to this function is @(c -> d)@.
-}
type family ResultT (n :: Nat) a
type instance ResultT 0 (a) = a
type instance ResultT 1 (_ -> a) = a
type instance ResultT 2 (_ -> _ -> a) = a
type instance ResultT 3 (_ -> _ -> _ -> a) = a
type instance ResultT 4 (_ -> _ -> _ -> _ -> a) = a
type instance ResultT 5 (_ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 6 (_ -> _ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 7 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 8 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 9 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 10 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 11 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 12 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 13 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 14 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a) = a
type instance ResultT 15 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a) = a

{-|
Like 'Control.IndexT.TupleN', but for functions.
-}
type family FuncN (n :: Nat) a
type instance FuncN 0 a = a
type instance FuncN 1 a = IndexT 0 a -> ResultT 1 a
type instance FuncN 2 a = IndexT 0 a -> IndexT 1 a -> ResultT 2 a
type instance FuncN 3 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> ResultT 3 a
type instance FuncN 4 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> ResultT 4 a
type instance FuncN 5 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> ResultT 5 a
type instance FuncN 6 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> ResultT 6 a
type instance FuncN 7 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> IndexT 6 a -> ResultT 7 a
type instance FuncN 8 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> IndexT 6 a -> IndexT 7 a -> ResultT 8 a
type instance FuncN 9 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> IndexT 6 a -> IndexT 7 a -> IndexT 8 a -> ResultT 9 a
type instance FuncN 10 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> IndexT 6 a -> IndexT 7 a -> IndexT 8 a -> IndexT 9 a -> ResultT 10 a
type instance FuncN 11 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> IndexT 6 a -> IndexT 7 a -> IndexT 8 a -> IndexT 9 a -> IndexT 10 a -> ResultT 11 a
type instance FuncN 12 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> IndexT 6 a -> IndexT 7 a -> IndexT 8 a -> IndexT 9 a -> IndexT 10 a -> IndexT 11 a -> ResultT 12 a
type instance FuncN 13 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> IndexT 6 a -> IndexT 7 a -> IndexT 8 a -> IndexT 9 a -> IndexT 10 a -> IndexT 11 a -> IndexT 12 a -> ResultT 13 a
type instance FuncN 14 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> IndexT 6 a -> IndexT 7 a -> IndexT 8 a -> IndexT 9 a -> IndexT 10 a -> IndexT 11 a -> IndexT 12 a -> IndexT 13 a -> ResultT 14 a
type instance FuncN 15 a = IndexT 0 a -> IndexT 1 a -> IndexT 2 a -> IndexT 3 a -> IndexT 4 a -> IndexT 5 a -> IndexT 6 a -> IndexT 7 a -> IndexT 8 a -> IndexT 9 a -> IndexT 10 a -> IndexT 11 a -> IndexT 12 a -> IndexT 13 a -> IndexT 14 a -> ResultT 15 a

{-|
Like 'Control.IndexT.TupleConstraint', but for functions.
-}
type FuncConstraint (n :: Nat) a = a ~ FuncN n a

{-|
Like 'Control.IndexT.HomoTupleConstraint', but for functions, where all arguments and the result type are the same.
-}
type HomoFuncConstraint (n :: Nat) a = (HomoArgFuncConstraint n a, ResultT n a ~ IndexT 0 a)

{-|
Like 'HomoFuncConstraint', but only constrains all the arguments to be the same type, not the result.
-}
type family HomoArgFuncConstraint (n :: Nat) a :: Constraint
type instance HomoArgFuncConstraint 0 a = (FuncConstraint 0 a)
type instance HomoArgFuncConstraint 1 a = (FuncConstraint 1 a)
type instance HomoArgFuncConstraint 2 a = (FuncConstraint 2 a, IndexT 0 a ~ IndexT 1 a)
type instance HomoArgFuncConstraint 3 a = (FuncConstraint 3 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a)
type instance HomoArgFuncConstraint 4 a = (FuncConstraint 4 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a)
type instance HomoArgFuncConstraint 5 a = (FuncConstraint 5 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a)
type instance HomoArgFuncConstraint 6 a = (FuncConstraint 6 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a)
type instance HomoArgFuncConstraint 7 a = (FuncConstraint 7 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a)
type instance HomoArgFuncConstraint 8 a = (FuncConstraint 8 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a)
type instance HomoArgFuncConstraint 9 a = (FuncConstraint 9 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a)
type instance HomoArgFuncConstraint 10 a = (FuncConstraint 10 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a)
type instance HomoArgFuncConstraint 11 a = (FuncConstraint 11 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a)
type instance HomoArgFuncConstraint 12 a = (FuncConstraint 12 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a, IndexT 10 a ~ IndexT 11 a)
type instance HomoArgFuncConstraint 13 a = (FuncConstraint 13 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a, IndexT 10 a ~ IndexT 11 a, IndexT 11 a ~ IndexT 12 a)
type instance HomoArgFuncConstraint 14 a = (FuncConstraint 14 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a, IndexT 10 a ~ IndexT 11 a, IndexT 11 a ~ IndexT 12 a, IndexT 12 a ~ IndexT 13 a)
type instance HomoArgFuncConstraint 15 a = (FuncConstraint 15 a, IndexT 0 a ~ IndexT 1 a, IndexT 1 a ~ IndexT 2 a, IndexT 2 a ~ IndexT 3 a, IndexT 3 a ~ IndexT 4 a, IndexT 4 a ~ IndexT 5 a, IndexT 5 a ~ IndexT 6 a, IndexT 6 a ~ IndexT 7 a, IndexT 7 a ~ IndexT 8 a, IndexT 8 a ~ IndexT 9 a, IndexT 9 a ~ IndexT 10 a, IndexT 10 a ~ IndexT 11 a, IndexT 11 a ~ IndexT 12 a, IndexT 12 a ~ IndexT 13 a, IndexT 13 a ~ IndexT 14 a)

{-|
Like 'Control.IndexT.IsTuple', but for functions.
-}
class (FuncConstraint n a) => IsFunc n a
instance (FuncConstraint n a) => IsFunc n a

{-|
Like 'Control.IndexT.IsHomoTuple', but for functions.
-}
class (HomoFuncConstraint n a) => IsHomoFunc n a
instance (HomoFuncConstraint n a) => IsHomoFunc n a

{-|
A classed based constraint for 'HomoArgFuncConstraint'.
-}
class (HomoArgFuncConstraint n a) => IsHomoArgFunc n a
instance (HomoArgFuncConstraint n a) => IsHomoArgFunc n a
