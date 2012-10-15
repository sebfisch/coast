module Template.Data where


import Import                       -- Yesod's Prelude


data    DirEntry    = DirEntry      { isDirectory           :: Bool
                                    , fileName              :: FilePath
                                    }
