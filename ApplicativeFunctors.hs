module ApplicativeFunctors where

import Prelude hiding (Applicative, Functor, fmap, pure, (<*>))

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

data Perhaps a = Exactly a | Blank deriving (Show)

instance Functor Perhaps where
    fmap _ Blank       = Blank
    fmap f (Exactly x) = Exactly $ f x

instance Applicative Perhaps where
    pure = Exactly
    Blank <*> _ = Blank
    (Exactly f) <*> x = fmap f x

main :: IO ()
main = do
    putStrLn "Functor"
    print $ fmap (+1) $ Exactly 1
    print $ fmap (+1) Blank
    print $ fmap length $ Exactly "123"
    putStrLn "Applicative functor"
    print $ pure (+) <*> Exactly 1 <*> Exactly 1
    print $ pure (*2) <*> Blank
