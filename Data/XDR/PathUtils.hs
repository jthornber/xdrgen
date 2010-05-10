module Data.XDR.PathUtils
       ( pathLookup
       , getPathString'
       ) where


import System.Directory
import System.FilePath (hasDrive)
import System.Path

pathLookup :: [AbsDir] -> RelFile -> IO (Maybe AbsFile)
pathLookup [] rf = return Nothing
pathLookup (include:includes) rf = do
  let af = include </> rf
  b <- fileExist' af
  if b then return $ Just af else pathLookup includes rf

fileExist' :: (AbsRelClass ar) => Path ar fd -> IO Bool
fileExist' = doesFileExist . getPathString'


getPathString' :: (AbsRelClass ar) => Path ar fd -> String
getPathString' = normaliseDrive . getPathString

normaliseDrive :: String -> String
normaliseDrive s@('/':xs) =
    if hasDrive xs then xs
    else s
normaliseDrive s = s
