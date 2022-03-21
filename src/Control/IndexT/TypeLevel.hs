{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-|
Module      : Control.IndexT
Copyright   : Clinton Mead, 2017
License     : BSD3
Maintainer  : clintonmead@gmail.com
Stability   : experimental
Portability : GHC

This module just has the 'Control.IndexT.IndexT' type extraction function but raised to type level
-}

module Control.IndexT.TypeLevel (
  IndexT
  ) where

import GHC.TypeLits (Nat)
import Data.Functor.Identity (Identity(Identity))

{-|
'Control.IndexT.IndexT' on polykinded type level tuples.

Note this currently is only defined on tuples.
-}
type family IndexT (i :: Nat) (a :: k) = (r :: k')
type instance IndexT 0 ('Identity a) = a
type instance IndexT 0 '(a, _) = a
type instance IndexT 1 '(_, a) = a
type instance IndexT 0 '(a, _, _) = a
type instance IndexT 1 '(_, a, _) = a
type instance IndexT 2 '(_, _, a) = a
type instance IndexT 0 '(a, _, _, _) = a
type instance IndexT 1 '(_, a, _, _) = a
type instance IndexT 2 '(_, _, a, _) = a
type instance IndexT 3 '(_, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _) = a
type instance IndexT 3 '(_, _, _, a, _) = a
type instance IndexT 4 '(_, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _) = a
type instance IndexT 5 '(_, _, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _, _) = a
type instance IndexT 5 '(_, _, _, _, _, a, _) = a
type instance IndexT 6 '(_, _, _, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _, _, _) = a
type instance IndexT 5 '(_, _, _, _, _, a, _, _) = a
type instance IndexT 6 '(_, _, _, _, _, _, a, _) = a
type instance IndexT 7 '(_, _, _, _, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _, _, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _, _, _, _) = a
type instance IndexT 5 '(_, _, _, _, _, a, _, _, _) = a
type instance IndexT 6 '(_, _, _, _, _, _, a, _, _) = a
type instance IndexT 7 '(_, _, _, _, _, _, _, a, _) = a
type instance IndexT 8 '(_, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 5 '(_, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 6 '(_, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 7 '(_, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 8 '(_, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 9 '(_, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 5 '(_, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 6 '(_, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 7 '(_, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 8 '(_, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 9 '(_, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 10 '(_, _, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 5 '(_, _, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 6 '(_, _, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 7 '(_, _, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 8 '(_, _, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 9 '(_, _, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 10 '(_, _, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 11 '(_, _, _, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 5 '(_, _, _, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 6 '(_, _, _, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 7 '(_, _, _, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 8 '(_, _, _, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 9 '(_, _, _, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 10 '(_, _, _, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 11 '(_, _, _, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 12 '(_, _, _, _, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 5 '(_, _, _, _, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 6 '(_, _, _, _, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 7 '(_, _, _, _, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 8 '(_, _, _, _, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 9 '(_, _, _, _, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 10 '(_, _, _, _, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 11 '(_, _, _, _, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 12 '(_, _, _, _, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 13 '(_, _, _, _, _, _, _, _, _, _, _, _, _, a) = a
type instance IndexT 0 '(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 1 '(_, a, _, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 2 '(_, _, a, _, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 3 '(_, _, _, a, _, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 4 '(_, _, _, _, a, _, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 5 '(_, _, _, _, _, a, _, _, _, _, _, _, _, _, _) = a
type instance IndexT 6 '(_, _, _, _, _, _, a, _, _, _, _, _, _, _, _) = a
type instance IndexT 7 '(_, _, _, _, _, _, _, a, _, _, _, _, _, _, _) = a
type instance IndexT 8 '(_, _, _, _, _, _, _, _, a, _, _, _, _, _, _) = a
type instance IndexT 9 '(_, _, _, _, _, _, _, _, _, a, _, _, _, _, _) = a
type instance IndexT 10 '(_, _, _, _, _, _, _, _, _, _, a, _, _, _, _) = a
type instance IndexT 11 '(_, _, _, _, _, _, _, _, _, _, _, a, _, _, _) = a
type instance IndexT 12 '(_, _, _, _, _, _, _, _, _, _, _, _, a, _, _) = a
type instance IndexT 13 '(_, _, _, _, _, _, _, _, _, _, _, _, _, a, _) = a
type instance IndexT 14 '(_, _, _, _, _, _, _, _, _, _, _, _, _, _, a) = a
