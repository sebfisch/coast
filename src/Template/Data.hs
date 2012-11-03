module Template.Data where


import           Import
import           Repository (ChangeInfo)


data    DirEntry    = Dir       { dirName           :: String
                                }
                    | File      { fileName          :: String
                                , fileChangeInfo    :: Maybe ChangeInfo
                                }
