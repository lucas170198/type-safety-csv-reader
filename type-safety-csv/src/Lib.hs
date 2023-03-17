{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Lib where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal (w2c, c2w)
import Data.Kind

-- | Using GADT's to choose the encode mode (name indexed or number indexed).

-- | Here, mode is a panthom type, just to choose the compression mode.
type Name = String
type Index = Int


data CSVFile mode cols lin where
    NameIndexed :: Vector cols String -> Vector lin (Vector cols String) -> CSVFile Name cols lin
    NumberIndexed :: Vector lin (Vector cols String) -> CSVFile Index cols lin
deriving instance Show (CSVFile m c l)
deriving instance Eq (CSVFile m c l)

-- | User indicates which type of compression mode we want
data FileType opt where
    Header :: FileType String
    NoHeader :: FileType Int
deriving instance Show (FileType opt)
deriving instance Eq (FileType opt)

-- | Abstraction of ByteString, to deal with Strings on the logic
class Container a where
    type Elem a
    elements :: a -> [Elem a]

instance Container BL.ByteString where
    type Elem BL.ByteString = Char
    elements a = fmap w2c (BL.unpack a)


-- | Safe access to Rows and columns
data Nat = Z | S Nat
    deriving Eq
    deriving Show

type Vector :: Nat -> Type -> Type
data Vector n a where
    VNil :: Vector 'Z a
    (:>) :: a -> Vector n a -> Vector ('S n) a
infixr 5 :>
deriving instance Show a => Show (Vector n a)
deriving instance Eq a => Eq (Vector n a)

type ExVector :: Type -> Type
data ExVector a where
    MkExVector :: Vector n a -> ExVector a
deriving instance Show a => Show (ExVector a)

type (+) :: Nat -> Nat -> Nat
type family m + n where
    Z + n = n
    (S m) + n = S (m + n)

type SNat :: Nat -> Type
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
deriving instance Show (SNat n)

(++) :: Vector n a -> Vector m a -> Vector (n + m) a
VNil ++ v = v
(x :> xs) ++ v = x :> (xs Lib.++ v)

-- | Finity GADT's. Used to generate possible valid index
data Fin :: Nat -> Type where
    FinZ :: Fin ('S n) -- 0th index to Vec n non-enpty
    FinS :: Fin n 
          -> Fin ('S n)
deriving instance Show (Fin n)

mapSafeVec :: (a -> b) -> Vector n a -> Vector n b
mapSafeVec _ VNil = VNil
mapSafeVec f (x :> xs) = f x :> mapSafeVec f xs

index :: Fin n -> Vector n a -> a
index FinZ (x :> _) = x
index (FinS idx) (_ :>  xs) = index idx xs

-- | Some Helpers functions
split :: Char -> String -> [String]
split _ "" = [""]
split delim (x : xs) | x == delim = "" : (split delim xs)
                     | otherwise  = (x : head (split delim xs)) : tail (split delim xs)

toList :: Vector n a -> [a]
toList VNil    = []
toList (x :> xs) = x : toList xs

toVector :: SNat n -> [a] -> Vector n a
toVector SZ [] = VNil
toVector (SS n) (x : xs) =  x :> (toVector n xs)
toVector (SS _) [] = undefined --- Never should happend if you pass nat equals to string size 
toVector SZ (_:_) = undefined --- Never should happend if you pass nat equals to string size
                     
--- | Converts ByteString to internal expected model
splitLinesAdapter :: Char -> BL.ByteString -> [[String]]
splitLinesAdapter delim s = split delim <$> (lines . elements) s

-- |Get Column by index number
getColumnByIndex :: Fin c -> CSVFile i c l -> Vector l String
getColumnByIndex i (NameIndexed _ r) = mapSafeVec (\c -> index i c) r
getColumnByIndex i (NumberIndexed r) = mapSafeVec (\c -> index i c) r

-- | Get Row
getRow :: Fin l -> CSVFile i c l -> Vector c String
getRow i (NameIndexed _ r) = index i r
getRow i (NumberIndexed r) = index i r

-- | Get Headers
getHeaders :: CSVFile Name c l -> Vector c String
getHeaders (NameIndexed h _) = h

-- | TODO: Get of the csv file to automatich identify that
decode :: FileType indexT -> SNat col -> SNat lin -> BL.ByteString -> Maybe (CSVFile indexT col lin)
decode Header col lin s = case splitLinesAdapter ',' s of
    (header : xs) -> Just $ NameIndexed (toVector col header) (mapSafeVec (\x -> toVector col x) (toVector lin xs))
    _ -> Nothing
decode NoHeader col lin s = Just $ NumberIndexed $ (mapSafeVec (\x -> toVector col x) (toVector lin unpakced))
    where unpakced = (splitLinesAdapter ',' s)

--- | This is necessary because I can't generate a SNat from an int :(
sNat2 = (SS (SS SZ))
sNat3 = (SS (SS (SS SZ)))

cidx0 = FinZ
cidx1 = FinS FinZ
cidx2 = FinS (FinS FinZ)
cidx3 = FinS (FinS (FinS FinZ))
cidx4 = FinS (FinS (FinS (FinS FinZ)))


--- | Example usage functions
loadingCSVToType :: IO()
loadingCSVToType = do
    csvData <- BL.readFile "test.csv"
    case decode Header sNat2 sNat3 csvData  of
        Nothing -> putStrLn "Error on decode"
        Just v -> print v

getCsvHeaders :: IO()
getCsvHeaders = do
    csvData <- BL.readFile "test.csv"
    case decode Header sNat2 sNat3 csvData  of
        Nothing -> putStrLn "Error on decode"
        Just v ->  print (show (getHeaders v))

getCsvColumn :: IO()
getCsvColumn = do
    csvData <- BL.readFile "test.csv"
    case decode Header sNat2 sNat3 csvData  of
        Nothing -> putStrLn "Error on decode"
        Just v ->  print (show (getColumnByIndex cidx0 v))

-- | Try to uncomment it, we will se that doenst compile (safe csv table doent allow forbide columns)
getCsvColumnWrong :: IO()
getCsvColumnWrong = do
    csvData <- BL.readFile "test.csv"
    case decode Header sNat2 sNat3 csvData  of
        Nothing -> putStrLn "Error on decode"
        Just v ->  print (show (getColumnByIndex cidx2 v))


-- | Referencias
--- https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html
--- https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
--- https://haskell.pesquisa.ufabc.edu.br/desenvolvimento-orientado-a-tipos/06.typefamily/

