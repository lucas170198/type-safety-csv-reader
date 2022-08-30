{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Lib
    ( 

    ) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal (w2c)
import Data.Kind


-- | Some Helpers functions
split :: Char -> String -> [String]
split _ "" = [""]
split delim (x : xs) | x == delim = "" : (split delim xs)
                     | otherwise  = (x : head (split delim xs)) : tail (split delim xs)

-- | Using GADT's to choose the encode mode (name indexed or number indexed).

-- | Here, mode is a panthom type, just to choose the compression mode.
type Name = String
type Index = Int


data CSVFile mode cols lin where
    NameIndexed :: Vector cols String -> Vector lin (Vector cols String) -> CSVFile Name cols lin
    NumberIndexed :: Vector lin (Vector cols String) -> CSVFile Index cols lin
deriving instance Show (CSVFile m c l)

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

type Vector :: Nat -> Type -> Type
data Vector n a where
    VNil :: Vector 'Z a
    (:>) :: a -> Vector n a -> Vector ('S n) a
infixr 5 :>
deriving instance Show a => Show (Vector n a)

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

--- >>> ("5" :> VNil) Lib.++ ("1" :> VNil)
--- "5" :> ("1" :> VNil)
---

-- | Finity GADT's. Used to generate possible valid index
data Fin :: Nat -> Type where
    FinZ :: Fin ('S n) -- 0th index to Vec n non-enpty
    FinS :: Fin n -> Fin ('S n)
deriving instance Show (Fin n)

mapSafeVec :: (a -> b) -> Vector n a -> Vector n b
mapSafeVec _ VNil = VNil
mapSafeVec f (x :> xs) = f x :> mapSafeVec f xs

index :: Fin n -> Vector n a -> a
index FinZ (x :> _) = x
index (FinS idx) (_ :>  xs) = index idx xs

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

--- >>> v = NameIndexed ("Nome" :> "Idade" :> VNil) (("Lucas" :> "24" :> VNil) :> ("Geo" :> "23" :> VNil) :> VNil)
--- >>> getColumnByIndex (FinS (FinS FinZ)) v

strToMatriz :: [[String]] -> Vector n (Vector m String)
strToMatriz = undefined

strToVector :: [String] -> Vector c String
strToVector = undefined


toList :: Vector n a -> [a]
toList VNil    = []
toList (x :> xs) = x : toList xs


-- | TODO: Implement a way to convert a vector to a sizedVector
decode :: FileType indexT -> BL.ByteString -> Maybe (CSVFile indexT lin col)
decode Header s = case splitLinesAdapter ',' s of
    (header : xs) -> Just $ NameIndexed (strToVector header) (strToMatriz xs)
    _ -> Nothing
decode NoHeader s = Just $ NumberIndexed $ strToMatriz (splitLinesAdapter ',' s)

-- somefunc :: IO ()
-- somefunc = do
--     csvData <- BL.readFile "test.csv"
--     case decode Header csvData of
--         Nothing -> putStrLn "Error on decode"
--         Just v -> print v



