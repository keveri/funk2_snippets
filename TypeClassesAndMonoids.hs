module TypeClassesAndMonoids where

import Prelude hiding (Monoid, mappend, mempty)

class Size a where
    getSize :: a -> Int

class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a

type Item = Int

data Box = Box [Item] deriving (Show)

instance Monoid Box where
    mempty = Box []
    mappend (Box xs) (Box ys) = Box $ xs ++ ys
    mconcat = foldr mappend mempty

instance Size Box where
    getSize (Box xs) = length xs

main :: IO ()
main = do
    let b1 = Box [1,2]
        b2 = Box [3,4]
    putStrLn "Individual boxes" >> mapM_ print [b1,b2]
    putStrLn "Combined boxes"   >> print (b1 `mappend` b2)
    putStrLn "Size of a box"    >> print (getSize b1)
