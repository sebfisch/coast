module Template.Data where


import           Import

import           Data.Function (on)
import           Repository    (ChangeInfo)


data    DirEntry    = DirEntry  { isFile            :: Bool
                                , entryName         :: String
                                , entryChangeInfo   :: Maybe ChangeInfo
                                }


instance Eq DirEntry where
    (==) = (==) `on` typeAndName


instance Ord DirEntry where
    compare = compare `on` typeAndName


typeAndName :: DirEntry -> (Bool,String)
typeAndName DirEntry{..} = (isFile, entryName)
