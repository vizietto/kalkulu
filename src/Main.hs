module Main where

import Kalkulu.Parser
-- import Kalkulu.Pattern
import Kalkulu.Evaluation (evaluate)
import Kalkulu.Environment
import Kalkulu.Kernel
import Text.ParserCombinators.Parsec
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Free
import System.Console.Haskeline
import Data.Maybe (fromJust)

main :: IO ()
main = do
  env <- defaultEnvironment
  runInputT defaultSettings (loop env)
  where loop env = do
          minput <- getInputLine "kalkulu> "
          liftIO $ case parse (expr False) "kalkulu" (fromJust minput) of
            Left err  -> putStrLn $ "error: " ++ show err
            Right val -> do
                e  <- run env (conversion val)
                e' <- run env $ evaluate e
                print e'
          loop env
        
run :: Environment -> Kernel a -> IO a
run env action = case runIdentity (runFreeT action) of
  Pure x -> return x
  _ -> undefined
