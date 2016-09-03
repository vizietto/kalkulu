module Main where

import Kalkulu.Parser
import qualified Kalkulu.Expression
import qualified Kalkulu.Evaluation
import Kalkulu.Kernel
import Text.ParserCombinators.Parsec
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import System.Console.Haskeline
import Data.Maybe (fromJust)
import Data.List (intercalate)

main :: IO ()
main = do
  env <- defaultEnv
  runInputT defaultSettings (loop env)
  where loop env = do
          minput <- getInputLine "kalkulu> "
          case parse (expr False) "kalkulu" (fromJust minput) of
            Left err -> (liftIO $ putStrLn $ "error: " ++ show err) >> loop env
            Right val -> (liftIO $ runReaderT (conversion val) env >>= print) >> loop env
        
