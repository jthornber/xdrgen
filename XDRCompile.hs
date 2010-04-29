module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Data.XDR.AST
import Data.XDR.Parser
import Data.XDR.PrettyPrinter
import Data.XDR.PrettyPrintC
import Data.XDR.PrettyPrintRpc

----------------------------------------------------------------

data Opts = Opts {
      optFormat :: String,
      optHeader :: Bool,
      optSource :: Bool
    }

header = "usage: xdrgen [option...] <file...>"

options :: [OptDescr (Opts -> Opts)]
options = [
 Option ['f'] ["format"] (ReqArg (\s o -> o {optFormat = s}) "FORMAT")
            "print format",
 Option ['h'] ["header"] (NoArg (\o -> o {optHeader = True}))
            "header file",
 Option ['c'] ["source"] (NoArg (\o -> o {optSource = True}))
            "source file"]

usage = usageInfo header options

-- data Flag = Include FilePath
--           | Define String Integer
--           | PreProcess

-- options :: [OptDescr Flag]
-- options =
--     [ Option ['I'] ["include"] (ReqArg Include "INCLUDE DIR") "directory to search for XDR source files"

data Printer = Printer {
      ppFormat :: String,
      ppHeader :: Maybe FilePath -> Specification -> String,
      ppSource :: Maybe FilePath -> Specification -> String
    }

printers :: [Printer]
printers = [Printer "c" ppCHeader ppCImpl,
            Printer "rpc" ppRpcHeader ppRpcSource,
            Printer "xdr" ppXDR ppXDR]

die :: String -> IO a
die s = do
    hPutStrLn stderr s
    exitFailure

getPrinter :: String -> Maybe Printer
getPrinter name = find ((==name) . ppFormat) printers

showResult :: Opts -> Maybe FilePath -> Either [ParseError] Specification -> IO ()
showResult opts file (Left errs) = die (unlines . map show $ errs)

showResult opts file (Right spec) = do
    maybe err f (getPrinter name)
  where
    err = die $ "invalid format `" ++ name ++ "'\n" ++ usage
    f p = do
        when (optHeader opts)
          (putStrLn $ ppHeader p file spec)
        when (optSource opts)
          (putStrLn $ ppSource p file spec)
    name = optFormat opts

parseOpts :: [String] -> IO ([Opts -> Opts], [String])
parseOpts args =
    case getOpt RequireOrder options args of
      (opts, files, []) -> return (opts, files)
      (_, _, errs) -> die (concat errs ++ usage)

defaultOpts :: Opts
defaultOpts = Opts {
              optFormat = "c",
              optHeader = False,
              optSource = False
            }

processOpts :: [Opts -> Opts] -> Opts
processOpts [] = defaultOpts
processOpts opts = foldl (flip ($)) defaultOpts opts

defines :: [(String, Integer)]
defines = [("FALSE", 0), ("TRUE", 1)]

processFile :: Opts -> FilePath -> IO ()
processFile opts file = parseFile defines file >>=
                        showResult opts (Just file)

processFiles :: Opts -> [FilePath] -> IO ()
processFiles opts [] = do
  txt <- B.getContents
  let ast = parseString defines txt "<stdin>"
  showResult opts Nothing ast

processFiles opts files = do
    forM_ files (processFile opts)

main :: IO ()
main = do
  args <- getArgs
  (optList, files) <- parseOpts args
  let opts = processOpts optList
  processFiles opts files
