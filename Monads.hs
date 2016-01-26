module Monads where

class Nomad m where
    back   :: a -> m a
    (>>>=) :: m a -> (a -> m b) -> m b
    (>>>)  :: m a -> m b -> m b

data Perhaps a = Exactly a | Blank deriving (Show)

instance Nomad (Perhaps) where
    back = Exactly
    Blank >>>= _ = Blank
    Exactly x >>>= f = f x
    x >>> y = x >>>= const y

addOne :: Int -> Perhaps Int
addOne = Exactly . (+) 1

main :: IO ()
main = do
    print $ back 1 >>>= addOne
    print $ Blank >>>= addOne
    putStrLn "Type:" >> getLine >>= (\x -> putStrLn $ "You said: " ++ x)
