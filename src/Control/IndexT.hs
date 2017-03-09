{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Control.IndexT
Copyright   : Clinton Mead, 2017
License     : MIT
Maintainer  : clintonmead@gmail.com
Stability   : experimental
Portability : GHC

This package is useful for dealing with for what I have called "indexed types". This is perhaps not a great choice of
name (alternative suggestions welcome) but basically I'm talking about types where you can "index" them like an array.
A tuple is a good example.

This particular module gives you a type function 'IndexT' that allows you to get the type of the say, third
element of a tuple.

But the other modules in this package, "Control.IndexT.Tuple" and "Control.IndexT.Function", build on this.

For example, occasionally you'll want to write a constraint like this:

> t ~ (t1,t2)

i.e. @t@ is a pair. But if @t1@ and @t2@ are not in scope, GHC will complain.

This package contains some type family and class defined constraints that allow you to
make constraints like "@t@ is a pair".

The type families are open, so you can extend this
module for your own data types.

More detailed usage information is in the documentation, both below. and also in "Control.IndexT.Tuple".

Currently this library deals with tuples up to length 15 and functions with up to 15 arguments.
Let me know if you need anything bigger.
-}

module Control.IndexT (
  IndexT
  ) where

import GHC.TypeLits (Nat)
import Data.Functor.Identity (Identity)

{-|
'IndexT' is the core type family of this module. @IndexT n a@ gets the type of the "nth" element of @a@.
In this module, 'IndexT' is defined on tuples and functions, and is zero based.

So

> IndexT 0 (a, b, c) == a

and

> IndexT 0 (a -> b -> c) == a

Note the following:

> IndexT 1 (a -> b -> c) == b
> IndexT 2 (a, b, c) == c

but...

> IndexT 2 (a -> b -> c) /= c -- (it's actually not defined)

This is because the way functions are defined. Consider a function of three arguments:

> f :: a -> b -> c -> d

For this function, we want:

> IndexT 2 (a -> b -> c -> d) == c

But if we defined 'IndexT' like the following:

> IndexT 2 (a -> b -> c) = c

Then we would find that:

> IndexT 2 (a -> b -> c -> d) == IndexT 2 (a -> b -> (c -> d)) == (c -> d)

Which is not right. For this reason, 'IndexT' can not get the "result" type of functions, you'll need to use 'ResultT' for that.
-}
type family IndexT (i :: Nat) a
type instance IndexT 0 (Identity a) = a
type instance IndexT 0 (a, _) = a
type instance IndexT 1 (_, a) = a
type instance IndexT 0 (a, _, _) = a
type instance IndexT 1 (_, a, _) = a
type instance IndexT 2 (_, _, a) = a
type instance IndexT 0 (a, _, _, _) = a
type instance IndexT 1 (_, a, _, _) = a
type instance IndexT 2 (_, _, a, _) = a
type instance IndexT 3 (_, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _) = a
type instance IndexT 3 (_, _, _, a, _) = a
type instance IndexT 4 (_, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _) = a
type instance IndexT 5 (_, _, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _, _) = a
type instance IndexT 5 (_, _, _, _, _, a, _) = a
type instance IndexT 6 (_, _, _, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _, _, _) = a
type instance IndexT 5 (_, _, _, _, _, a, _, _) = a
type instance IndexT 6 (_, _, _, _, _, _, a, _) = a
type instance IndexT 7 (_, _, _, _, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _, _, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _, _, _, _) = a
type instance IndexT 5 (_, _, _, _, _, a, _, _, _) = a
type instance IndexT 6 (_, _, _, _, _, _, a, _, _) = a
type instance IndexT 7 (_, _, _, _, _, _, _, a, _) = a
type instance IndexT 8 (_, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 5 (_, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 6 (_, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 7 (_, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 8 (_, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 9 (_, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 5 (_, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 6 (_, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 7 (_, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 8 (_, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 9 (_, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 10 (_, _, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 5 (_, _, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 6 (_, _, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 7 (_, _, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 8 (_, _, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 9 (_, _, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 10 (_, _, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 11 (_, _, _, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 5 (_, _, _, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 6 (_, _, _, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 7 (_, _, _, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 8 (_, _, _, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 9 (_, _, _, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 10 (_, _, _, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 11 (_, _, _, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 12 (_, _, _, _, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 5 (_, _, _, _, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 6 (_, _, _, _, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 7 (_, _, _, _, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 8 (_, _, _, _, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 9 (_, _, _, _, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 10 (_, _, _, _, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 11 (_, _, _, _, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 12 (_, _, _, _, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 13 (_, _, _, _, _, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 (_, a, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 (_, _, a, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 (_, _, _, a, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 4 (_, _, _, _, a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 5 (_, _, _, _, _, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 6 (_, _, _, _, _, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 7 (_, _, _, _, _, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 8 (_, _, _, _, _, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 9 (_, _, _, _, _, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 10 (_, _, _, _, _, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 11 (_, _, _, _, _, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 12 (_, _, _, _, _, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 13 (_, _, _, _, _, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 14 (_, _, _, _, _, _, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 (a -> _) = a
type instance IndexT 1 (_ -> a -> _) = a
type instance IndexT 2 (_ -> _ -> a -> _) = a
type instance IndexT 3 (_ -> _ -> _ -> a -> _) = a
type instance IndexT 4 (_ -> _ -> _ -> _ -> a -> _) = a
type instance IndexT 5 (_ -> _ -> _ -> _ -> _ -> a -> _) = a
type instance IndexT 6 (_ -> _ -> _ -> _ -> _ -> _ -> a -> _) = a
type instance IndexT 7 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> a -> _) = a
type instance IndexT 8 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a -> _) = a
type instance IndexT 9 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a -> _) = a
type instance IndexT 10 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a -> _) = a
type instance IndexT 11 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a -> _) = a
type instance IndexT 12 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a -> _) = a
type instance IndexT 13 (_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> a -> _) = a
