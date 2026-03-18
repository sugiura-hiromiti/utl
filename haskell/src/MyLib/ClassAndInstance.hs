module MyLib.ClassAndInstance where

import GHC.IO.IOMode (IOMode)
import System.IO (Handle, hClose, openFile)

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  hndl <- openFile path mode
  rslt <- f hndl
  _ <- hClose hndl
  return rslt
