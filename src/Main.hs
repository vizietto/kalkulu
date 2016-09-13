module Main where

import Kalkulu.Parser
import Kalkulu.Evaluation (evaluate)
import Kalkulu.Environment
import Text.ParserCombinators.Parsec
import Control.Monad.Trans
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
