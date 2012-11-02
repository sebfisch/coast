module Repository.Darcs (

    DarcsRepo, darcsRepo,

    module Repository

    ) where


import           Import
import           Repository


newtype DarcsRepo   = DarcsRepo     { darcsRepoDir          :: FilePath     }


darcsRepo :: FilePath -> Repo
darcsRepo = someRepo . DarcsRepo


instance Repository DarcsRepo where
    repoDir             = darcsRepoDir
    lastChangeInfo _ _  = return Nothing