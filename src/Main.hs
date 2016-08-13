module Main where

import Kalkulu.Parser (expr)
import Text.ParserCombinators.Parsec
import Control.Monad.Trans
import System.Console.Haskeline
import Data.Maybe (fromJust)

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine "kalkulu> "
          case parse (expr False) "kalkulu" (fromJust minput) of
           Left err -> (liftIO $ putStrLn $ "error: " ++ show err) >> loop
           Right val -> (liftIO $ print val) >> loop