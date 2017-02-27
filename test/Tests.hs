{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.IndexT
import Test.Hspec

default ()

data TT where
  TT :: (forall t. (Integral (IndexT 0 t), Integral (IndexT 1 t), IsTuple 2 t) => t) -> TT

data HTT where
  HTT :: (forall t. (Num (IndexT 0 t), IsHomoTuple 2 t) => t) -> HTT

data FT where
  FT :: (forall t. (Num (IndexT 0 t), IndexT 1 t ~ Bool, IndexT 0 t ~ ResultT 2 t, IsFunc 2 t) => t) -> FT

data HFT where
  HFT :: (forall t. (Num (IndexT 0 t), IsHomoFunc 2 t) => t) -> HFT

f1 :: TT -> Integer
f1 (TT (x :: (Int, Integer))) = toInteger (fst x) + toInteger (snd x)

f2 :: Num a => HTT -> a
f2 (HTT x) = fst x * snd x

f3 :: (Num a) => FT -> a -> Bool -> a
f3 (FT f) x y = f x y

f4 :: (Num a) => HFT -> a -> a -> a
f4 (HFT f) x y = f x y

x1 :: Integer
x1 = f1 (TT (40,2))

x2 :: Int
x2 = f2 (HTT (12,7))

x3 :: Int
x3 = f3 (FT (\x y -> if y then x*2 else x*3)) 5 False

x4 :: Integer
x4 = f4 (HFT (+)) 40 3

main :: IO ()
main = hspec $ do
  it "Tuple Test" $ x1 `shouldBe` 42
  it "HomoTuple Test" $ x2 `shouldBe` 84
  it "Func Test" $ x3 `shouldBe` 15
  it "HomoFunc Test" $ x4 `shouldBe` 43