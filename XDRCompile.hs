module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Data.XDR.Parser
import Data.XDR.PrettyPrinter
import Data.XDR.PrettyPrintC

----------------------------------------------------------------

-- data Flag = Include FilePath
--           | Define String Integer
--           | PreProcess

-- options :: [OptDescr Flag]
-- options =
--     [ Option ['I'] ["include"] (ReqArg Include "INCLUDE DIR") "directory to search for XDR source files"

showResult (Left errs) = hPutStrLn stderr (unlines . map show $ errs)
showResult (Right spec) = putStrLn $ ppXDR spec

doFile f = parseFile [] f >>= showResult

doStdin = do
  txt <- B.getContents
  let ast = parseString [] txt "<stdin>"
  showResult ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> doStdin
    [f] -> doFile f
    _ -> usage

usage = do
  putStrLn "usage: xdrgen <file>"
  exitFailure

----------------------------------------------------------------
