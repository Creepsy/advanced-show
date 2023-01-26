{-# LANGUAGE DeriveGeneric #-}
module Main where
import GHC.Generics (Generic)
import AdvancedShow (AdvancedShow (advShow))

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Generic, Show)

instance AdvancedShow a => AdvancedShow (Tree a) where

main :: IO ()
main = do
    let test = Node 10 (Node 11 Leaf Leaf) (Node 12 (Node 13 Leaf Leaf) Leaf) :: Tree Int

    putStrLn . advShow $ test
    print test
