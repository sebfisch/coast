module Template.Data where


import           Import


data    DirEntry    = DirEntry      { isDirectory           :: Bool
                                    , fileName              :: FilePath
                                    }
