module SourceGen where

import Data.List (intercalate)

typeVar = "a"

normalTF :: String -> Int -> String
normalTF name i = normalTFS name i typeVar

normalTFS :: String -> Int -> String -> String
normalTFS name i s = name ++ " " ++ show i ++ " " ++ s

indexTStr = "IndexT"
indexTF i = normalTF "IndexT" i
indexTFS i s = normalTFS "IndexT" i s

funcIndex n i = if (i /= n - 1) then indexTF i else resultTF i

indexNTStr = "IndexNT"
indexNTFS n i s = indexNTStr ++ " " ++ show n ++ " " ++ show i ++ " " ++ s
indexNTF n i = indexNTFS n i typeVar

resultTStr = "ResultT"
resultTFS = normalTFS "ResultT"
resultTF i = normalTF "ResultT" i

tupleTypesName = "TupleN"
funcTypesName = "FuncN"

tupleConstraintName = "TupleConstraint"
funcConstraintName = "FuncConstraint"

homoTupleConstraintName = "HomoTupleConstraint"
homoFuncConstraintName = "HomoFuncConstraint"

tupleClassName = "IsTuple"
funcClassName = "IsFunc"

homoTupleClassName = "IsHomoTuple"
homoFuncClassName = "IsHomoFunc"

genSeq :: String -> Int -> Int -> String
genSeq sep n i = "(" ++ concatMap f [0..last_n] ++ ")" where
  last_n = n-1
  f i' = (if i' == i then typeVar else "_") ++ (if i' == last_n then "" else sep)

genLine :: (Int -> Int -> String -> String) -> String -> Int -> Int -> String
genLine f sep n i = ((f n i) (genSeq sep n i)) ++ " = " ++ typeVar ++ "\n"

genInstances :: (Int -> Int -> String -> String) -> String -> Int -> Int -> String
genInstances f sep n_start n_end =
  concat [genLine f sep n' i | n' <- [n_start..n_end], i <- [0..n'-1]]

genTuplesNoN :: Int -> String
genTuplesNoN n = genInstances (\_ i s -> "type instance " ++ indexTFS i s) ", " 2 n

genFuncsNoN n = concatMap (\i -> genLine (\_ i s -> "type instance " ++ indexTFS i s) " -> " (i+2) i) [0..(n-2)]

genFuncsResult n = concatMap (\i -> genLine (\_ i s -> "type instance " ++ resultTFS i s) " -> " (i+1) i) [0..n]

genNFunc :: Int -> Int -> String -> String
genNFunc n i s = "type instance " ++ indexNTFS n i s

genTuplesN :: Int -> String
genTuplesN n = genInstances genNFunc ", " 2 n

genFuncsN :: Int -> String
genFuncsN n = genInstances genNFunc " -> " 2 n

allResultDefs n =
  "type family " ++ resultTStr ++ " (n :: Nat) " ++ typeVar ++ "\n" ++
  --"type instance" ++ indexTStr ++ " 0 " ++ typeVar ++ " -> _ = " ++ typeVar ++ "\n" ++
  --"type instance" ++ indexTStr ++ " i " ++ " _ -> " ++ typeVar ++ " = " ++ indexTStr ++ " (i - 1) " ++ typeVar ++ "\n" ++
  genFuncsResult n ++ "\n"


allNoNDefs n =
  "type family " ++ indexTStr ++ " (i :: Nat) " ++ typeVar ++ "\n" ++
  --"type instance" ++ indexTStr ++ " 0 " ++ typeVar ++ " -> _ = " ++ typeVar ++ "\n" ++
  --"type instance" ++ indexTStr ++ " i " ++ " _ -> " ++ typeVar ++ " = " ++ indexTStr ++ " (i - 1) " ++ typeVar ++ "\n" ++
  "type instance " ++ indexTStr ++ " 0 (Identity " ++ typeVar ++ ") = " ++ typeVar ++ "\n" ++
  genTuplesNoN n ++
  genFuncsNoN n ++ "\n"


allNDefs n =
  "type family " ++ indexNTStr ++ " (n :: Nat) (i :: Nat) " ++ typeVar ++ "\n" ++
  "type instance " ++ indexNTStr ++ " 1 0 (Identity " ++ typeVar ++ ") = " ++ typeVar ++ "\n" ++
  genTuplesN n ++
  genFuncsN n ++ "\n\n" ++
  "type " ++ resultTStr ++ " (n :: Nat) " ++ typeVar ++ " = " ++ indexNTStr ++ " n (n + 1) " ++ typeVar ++ "\n"

allDefs n =
  allNoNDefs n ++
--  allNDefs n ++
  allResultDefs n

typesLine :: (Int -> Int -> String) -> String -> Bool -> String -> Int -> Int -> String
typesLine f conName parenthesis sep left_n n = line where
  last_n = n -1
  rhs = concatMap (\i -> f n i ++ if (i == last_n) then "" else sep) [0..last_n]
  rhs_par = if parenthesis then "(" ++ rhs ++ ")" else rhs
  line = typeInstanceStr1N conName left_n ++ rhs_par ++ "\n"

tupleTypes :: Int -> String
tupleTypes n = typesLine (\n i -> indexTF i) tupleTypesName True ", " n n

funcTypes :: Int -> String
funcTypes n = typesLine funcIndex funcTypesName False " -> " (n-1) n

typeFamilyLine name constraint = "type family " ++ name ++ " (n :: Nat) " ++ typeVar ++ (if constraint then " :: Constraint" else "") ++ "\n"
typeInstanceStr1N name n = "type instance " ++ name ++ " " ++ show n ++ " " ++ typeVar ++ " = "

genericTypes :: (Int -> String) -> String -> [(Int, String)] -> Int -> Int -> String
genericTypes f name extras start_n end_n =
  typeFamilyLine name False ++
  concatMap (\(n, s) -> typeInstanceStr1N name n ++ s ++ "\n") extras ++
  concatMap f [start_n..end_n] ++
  "\n"

homoConstraints addResult name heteroName start_n end_n =
  typeFamilyLine name True ++
  concatMap f [start_n..end_n] ++ "\n" where
    f i = typeInstanceStr1N name i ++ "(" ++ middle ++ ")\n" where
      middle = normalTF heteroName i ++ (if addResult && (i /= 0) then ", " ++ resultTF i ++ " ~ " ++ indexTF 0 else "") ++ (if i >= 2 then ", " else "") ++ concatMap g [0..(i-2)] where
        g i' = indexTF i' ++ " ~ " ++ indexTF (i' + 1) ++ (if i' /= i - 2 then ", " else "")


allTupleTypes n = genericTypes tupleTypes tupleTypesName [(0,"()"),(1,"Identity " ++ typeVar)] 2 n
allFuncTypes n  = genericTypes funcTypes funcTypesName [(0,typeVar)] 2 (n + 1)
homoTupleConstraints = homoConstraints False homoTupleConstraintName tupleConstraintName 0
homoFuncConstraints = homoConstraints True homoFuncConstraintName funcConstraintName 0

allConstraints n =
  allTupleTypes n ++
  allFuncTypes n ++
  f tupleConstraintName tupleTypesName ++
  f funcConstraintName funcTypesName ++
  "\n" ++
  homoTupleConstraints n ++
  homoFuncConstraints n where
    f conName name = "type " ++ conName ++ " (n :: Nat) " ++ typeVar ++ " = " ++ typeVar ++ " ~ " ++ name ++ " n " ++ typeVar ++ "\n"

genClass constraintName className = f "class" ++ f "instance" ++ "\n" where
  f s = s ++ " (" ++ constraintName ++ " n " ++ typeVar ++ ") => " ++ className ++ " n " ++ typeVar ++ "\n"

extsStr = concatMap ext [
  "TypeFamilies",
  "DataKinds",
  "KindSignatures",
  "ConstraintKinds",
  "MultiParamTypeClasses",
  "UndecidableInstances",
  "UndecidableSuperClasses",
  "FlexibleInstances",
  "TypeOperators"
  ]
  where
    ext s = "{-# LANGUAGE " ++ s ++ " #-}\n"


imports =
  "import GHC.TypeLits (Nat, type (-))\n" ++
  "import GHC.Exts (Constraint)\n" ++
  "import Data.Functor.Identity (Identity)\n"

allClasses =
  genClass tupleConstraintName tupleClassName ++
  genClass homoTupleConstraintName homoTupleClassName ++
  genClass funcConstraintName funcClassName ++
  genClass homoFuncConstraintName homoFuncClassName

moduleHeader modName = "module " ++ modName ++ " (\n" ++ intercalate ",\n" (map ("  " ++) [
  indexTStr,
  resultTStr,
  --indexNTStr,

  tupleTypesName,
  funcTypesName,

  tupleConstraintName,
  funcConstraintName,

  homoTupleConstraintName,
  homoFuncConstraintName,

  tupleClassName,
  funcClassName,

  homoTupleClassName,
  homoFuncClassName
  ]) ++ "\n  ) where\n"

genCode modName n =
  extsStr ++ "\n" ++
  moduleHeader modName ++
  "\n" ++
  imports ++
  "\n" ++
  allDefs n ++
  "\n" ++
  allConstraints n ++
  "\n" ++
  allClasses ++
  "\n"