module Main where

import Kalkulu.Parser
-- import Kalkulu.Pattern
import Kalkulu.Evaluation (evaluate)
import Kalkulu.DefaultEnv (defaultEnv)
import Text.ParserCombinators.Parsec
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import System.Console.Haskeline
import Data.Maybe (fromJust)

main :: IO ()
main = do
  env <- defaultEnv
  runInputT defaultSettings (loop env)
  where loop env = do
          minput <- getInputLine "kalkulu> "
          case parse (expr False) "kalkulu" (fromJust minput) of
            Left err -> (liftIO $ putStrLn $ "error: " ++ show err) >> loop env
            Right val -> (liftIO $ runReaderT ((conversion val) >>= evaluate) env >>= print) >> loop env
        
