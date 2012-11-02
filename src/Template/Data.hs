module Template.Data where


import           Import
import           Repository (ChangeInfo, Repo)


data    DirEntry    = Dir       { dirName           :: String
                                }
                    | File      { fileName          :: String
                                , fileChangeInfo    :: Maybe (ChangeInfo Repo)
                                }
