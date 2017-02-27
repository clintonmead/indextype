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
import Test.HUnit
import System.Exit (exitSuccess, exitFailure)

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

test1 = TestCase (assertEqual "Tuple Test" x1 42)
test2 = TestCase (assertEqual "HomoTuple Test" x2 84)
test3 = TestCase (assertEqual "Func Test" x3 15)
test4 = TestCase (assertEqual "HomoFunc Test" x4 43)

tests = TestList [test1, test2, test3, test4]

runTestTTAndExit :: Test -> IO ()
runTestTTAndExit tests = do
  c <- runTestTT tests
  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure

main :: IO ()
main = runTestTTAndExit tests
