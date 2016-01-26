-- Doesn't make much sense but shows a way of using ReaderT monad transformer...
module MonadTransformers where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)

data Config = Config
    { cUser :: String
    , cPass :: String
    }

-- function using config and IO
checkConf :: Config -> String -> IO Bool
checkConf cfg x = do
    let user = cUser cfg
    putStrLn $ "the user is: " ++ user
    return $ (==) user x

-- function not using config, but it uses a function that requires it
func :: Config -> String -> IO String
func cfg x = do
    let stuff = True
    result <- checkConf cfg x
    if result && stuff
        then return "YEY"
        else return "NEY"

--
-- Similar stuff, but now stacking IO and Reader monad so we can use both here.
--

func' :: String -> ReaderT Config IO String
func' x = do
    user <- reader cUser
    let stuff  = True
        result = x == user
    liftIO (putStrLn $ "the user is: " ++ user)
    if result && stuff
        then return "YEY"
        else return "NEY"

main :: IO ()
main = do
    let cfg = Config "user" "pass"
    runReaderT (func' "rms") cfg >>= print
