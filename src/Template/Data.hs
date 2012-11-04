module Template.Data where


import           Import

import           Control.Arrow (second)
import           Data.Char     (toLower)
import           Data.Function (on)
import           Repository    (ChangeInfo)


data    DirEntry    = Dir       { dirName           :: String
                                }
                    | File      { fileName          :: String
                                , fileChangeInfo    :: Maybe ChangeInfo
                                }

instance Eq DirEntry where
    (==) = (==) `on` typeAndName


instance Ord DirEntry where
    compare = compare `on` second (map toLower) . typeAndName


typeAndName :: DirEntry -> (Bool,String)
typeAndName (Dir name)      = (False,name)
typeAndName (File name _)   = (True,name)
