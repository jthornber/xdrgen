module Data.XDR.PathUtils
       ( pathLookup
       ) where


import Prelude hiding (FilePath)
import System.Path
import System.Posix.Files

pathLookup :: [AbsDir] -> RelFile -> IO (Maybe AbsFile)
pathLookup [] rf = return Nothing
pathLookup (include:includes) rf = do
  let af = include </> rf
  b <- fileExist' af
  if b then return $ Just af else pathLookup includes rf
  
fileExist' :: Path ar fd -> IO Bool
fileExist' = fileExist . getPathString

