{-# LANGUAGE DeriveGeneric #-}
module Main where
import GHC.Generics (Generic)
import AdvancedShow (AdvancedShow (advShow), advPrint)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Generic, Show)
data NamedTree a = 
    NamedLeaf | 
    NamedNode {
        value :: a,
        left :: NamedTree a,
        right :: NamedTree a
    }
    deriving (Generic, Show)

instance AdvancedShow a => AdvancedShow (NamedTree a)
instance AdvancedShow a => AdvancedShow (Tree a)

data BigTree a = BigTree {
    treeType :: a,
    height :: Int
} deriving (Generic)

instance AdvancedShow a => AdvancedShow (BigTree a)

main :: IO ()
main = do
    let test = Node [10, 22] (Node [12, 2] Leaf Leaf) (Node [2] (Node [1, 2, 3, 4] Leaf Leaf) Leaf) :: Tree [Int]
        namedTest = NamedNode "12" (NamedNode "test" NamedLeaf NamedLeaf) (NamedNode "tasty" (NamedNode "ye" NamedLeaf NamedLeaf) NamedLeaf) :: NamedTree String
    
    putStrLn "Normal Tree:"
    putStrLn "Show:"
    print test
    putStrLn "Advanced Show:"
    advPrint test

    putStrLn ""

    putStrLn "Named Tree:"
    putStrLn "Show:"
    advPrint namedTest
    putStrLn "Advanced Show:"
    advPrint namedTest

    let treeee = BigTree {treeType = [BigTree {treeType = 1, height = 1}, BigTree {treeType = 2, height = 1}], height = 1} :: BigTree [BigTree Int]
    -- let treeee = [BigTree {treeType = 1, height = 1}, BigTree {treeType = 2, height = 1}] :: [BigTree Int]

    advPrint treeee
