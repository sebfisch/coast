module Repository.Darcs where


import           Import
import           Repository.Interface
import           System.Directory     (doesDirectoryExist)
import           System.FilePath      ((</>))


newtype DarcsRepo   = DarcsRepo     { darcsRepoDir          :: FilePath     }


instance IsRepository DarcsRepo where
    repoDir             = darcsRepoDir
    lastChangeInfo _ _  = return Nothing


isDarcsRepository :: FilePath -> IO Bool
isDarcsRepository path = do
    isDir <- doesDirectoryExist path
    if isDir then
        doesDirectoryExist $ path </> "_darcs"
      else
        return False
