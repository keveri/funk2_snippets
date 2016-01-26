{-# LANGUAGE DeriveFunctor #-}
module FreeMonads where

data Free f a = Pure a
              | Free (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap f fx = do { x <- fx; return $ f x }

instance Functor f => Applicative (Free f) where
    pure      = Pure
    ff <*> fx = do { f <- ff; x <- fx; return $ f x }

instance Functor f => Monad (Free f) where
    return         = Pure
    (Free x) >>= f = Free $ (>>= f) <$> x
    (Pure x) >>= f = f x


type Password = String
type Login = Free Action
type State = Password

data Action n = Tell String n
              | Load Password n
              | Guess (Bool -> n)
              | Accept n
              | SelfDestruct n
              deriving (Functor)


tell :: String -> Login ()
tell x = Free $ Tell x $ Pure ()

load :: Password -> Login ()
load p = Free $ Load p $ Pure ()

guess :: Login Bool
guess = Free $ Guess Pure

accept :: Login ()
accept = Free $ Accept $ Pure ()

selfDestruct :: Login ()
selfDestruct = Free $ SelfDestruct $ Pure ()


login :: Login ()
login = do
    let secret = "1234"
    load secret
    tell "What is the secret?"
    correct <- guess
    if correct
        then accept
        else selfDestruct


runLogin :: Password -> Login () -> IO ()
runLogin p (Free (Tell x n)) = putStr x >> runLogin p n
runLogin _ (Free (Load p n)) = runLogin p n
runLogin p (Free (Guess f))  = do
    line <- getLine
    runLogin p $ f $ line == p

runLogin _ (Free (Accept _))       = putStrLn "Access granted."
runLogin _ (Free (SelfDestruct _)) = putStrLn "Self-destruct sequence initiated!"
runLogin _ (Pure ())               = putStrLn ""
