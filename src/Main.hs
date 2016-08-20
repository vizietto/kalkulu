module Main where

import Kalkulu.Parser
import Text.ParserCombinators.Parsec
import Control.Monad.Trans
import System.Console.Haskeline
import Data.Maybe (fromJust)
import Data.List (intercalate)

showExpr :: Expr -> String
showExpr (Number x)   = show x
showExpr (String x)   = show x
showExpr (Builtin x)  = show x
showExpr (Symbol x)   = x
showExpr (Cmp h args) = (showExpr h) ++ "[" ++ (intercalate ", " (map showExpr args)) ++ "]"

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine "kalkulu> "
          case parse (expr False) "kalkulu" (fromJust minput) of
           Left err -> (liftIO $ putStrLn $ "error: " ++ show err) >> loop
           Right val -> (liftIO $ putStrLn $ showExpr val) >> loop
