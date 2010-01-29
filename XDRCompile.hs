module Main where

import Control.Applicative
import System.Console.GetOpt
import System.Environment
import System.Exit

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

main :: IO ()
main = do
  txt <- getContents
  putStrLn $ case parseString [] txt "" of
               Left errs  -> unlines . map show $ errs
               Right spec -> ppCImpl $ spec

----------------------------------------------------------------
