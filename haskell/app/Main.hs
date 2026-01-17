module Main (main) where

import MyLib
import System.Environment (getArgs, getProgName)
import System.IO

main :: IO ()
main = do
  _ <- MyLib.withFile' "haskell.cabal" ReadMode (\h -> hGetContents h >>= print)
  args <- getArgs
  progName <- getProgName
  print ""
  print ""
  print "---------------------------------------------------"
  print ""
  print args
  print progName
  return ()
