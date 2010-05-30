module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Prelude hiding (FilePath)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO hiding (FilePath)
import System.Path

import Data.XDR.AST
import Data.XDR.Parser
import Data.XDR.PrettyPrinter
import Data.XDR.PrettyPrintC
import Data.XDR.PrettyPrintJava
import Data.XDR.PrettyPrintRpc

----------------------------------------------------------------

data Flag = Include String
          | Format String

options :: [OptDescr Flag]
options = [ Option "I" ["include"] (ReqArg Include "INCLUDE DIR") "directory to search for XDR source files"
          , Option "f" ["format"] (ReqArg Format "FORMAT") "output format"
          ]

header :: String
header = "usage: xdrgen [option...] <file...>"

usage :: String
usage = usageInfo header options

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts args = case getOpt Permute options args of
                   (flags, files, []) -> return (flags, files)
                   (_, _, errs)       -> die $ concat errs ++ usage

----------------------------------------------------------------

type Formatter = Specification -> String

formatters :: Map String Formatter
formatters = M.fromList [ ("c-header",   ppCHeader)
                        , ("c-impl",     ppCImpl)
                        , ("java",       ppJava)
                        , ("rpc-header", ppRpcHeader)
                        , ("rpc-impl",   ppRpcImpl)
                        , ("xdr",        ppXDR)
                        ]

processFile :: [Flag] -> AbsDir -> AbsFile -> IO (Either [ParseError] Specification)
processFile flags cwd file = parseFile options file
    where
      options = [ Imports [mkAbsPath cwd i | Include i <- flags]
                , Defines []
                ]

runFormatter :: Formatter -> Either [ParseError] Specification -> IO ()
runFormatter _ (Left errs)  = die (unlines . map show $ errs)
runFormatter f (Right spec) = putStrLn . f $ spec

main :: IO ()
main = do
  (flags, files) <- parseOpts =<< getArgs

  let format = withDefault "xdr" [f | Format f <- flags]
  case M.lookup format formatters of
    Nothing        -> die "can't find formatter"
    Just formatter -> do
         cwd <- asAbsDir <$> getCurrentDirectory
         forM_ (map (mkAbsPath cwd) files) $ \file ->
             processFile flags cwd file >>= runFormatter formatter

withDefault :: a -> [a] -> a
withDefault x []    = x
withDefault _ (x:_) = x

die :: String -> IO a
die s = do
    hPutStrLn stderr s
    exitFailure

----------------------------------------------------------------