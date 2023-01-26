{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Generics.html#g:3

module AdvancedShow (
    AdvancedShow(..)
) where

import qualified GHC.Generics as Gen
import GHC.Generics ((:+:), (:*:)(..))
import qualified Data.Data as Gen

class AdvancedShow a where
    advShow :: a -> String
    default advShow :: (Gen.Generic a, AdvancedShow' (Gen.Rep a)) => a -> String
    advShow val = unlines . advShow' $ Gen.from val

instance AdvancedShow Int where
    advShow = show

class AdvancedShow' (f :: * -> *) where
    advShow' :: f a -> [String]
    
instance AdvancedShow' Gen.V1 where
    advShow' val = []

instance AdvancedShow' Gen.U1 where
    advShow' val = []

instance (AdvancedShow' a, AdvancedShow' b) => AdvancedShow' (a :+: b) where
    advShow' (Gen.L1 val) = advShow' val
    advShow' (Gen.R1 val) = advShow' val

instance (AdvancedShow' a, AdvancedShow' b) => AdvancedShow' (a :*: b) where
    advShow' (first :*: second) = linesFirst' ++ advShow' second where
        linesFirst = advShow' first
        linesFirst' = head linesFirst : (map (\(_:line') -> '|' : line') . tail $ linesFirst)

instance (AdvancedShow c) => AdvancedShow' (Gen.K1 i c) where
    advShow' (Gen.K1 val) = let (head:rest) = lines $ advShow val in ("└─ " ++ head) : map ("   " ++) rest

instance (Gen.Constructor k, AdvancedShow' c) => AdvancedShow' (Gen.C1 k c) where
    advShow' :: (Gen.Constructor k, AdvancedShow' c) => Gen.C1 k c a -> [String]
    advShow' con@(Gen.M1 val) = Gen.conName con : advShow' val

instance (Gen.Datatype k, AdvancedShow' c) => AdvancedShow' (Gen.D1 k c) where
    advShow' typ@(Gen.M1 val) = advShow' val 

instance (Gen.Selector k, AdvancedShow' c) => AdvancedShow' (Gen.S1 k c) where
    advShow' con@(Gen.M1 val) = advShow' val