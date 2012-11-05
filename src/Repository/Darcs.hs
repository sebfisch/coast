module Repository.Darcs where


import           Import

import           Repository.Interface

import           Darcs.Patch.Info         (PatchInfo)
import           Darcs.Patch.Inspect      (PatchInspect, listTouchedFiles)
import           Darcs.Patch.PatchInfoAnd (PatchInfoAnd, info)
import           Darcs.Patch.Set          (newset2RL)
import           Darcs.Repository         (RepoJob (..), readRepo,
                                           withRepositoryDirectory)
import           Darcs.Witnesses.Ordered  (mapRL)

import           System.Directory         (doesDirectoryExist)
import           System.FilePath          ((</>))


isDarcsRepository :: FilePath -> IO Bool
isDarcsRepository path = do
    isDir <- doesDirectoryExist path
    if isDir then
        doesDirectoryExist $ path </> "_darcs"
      else
        return False


newtype DarcsRepo   = DarcsRepo     { darcsRepoDir          :: FilePath     }


data DarcsChange where
    Change :: PatchInspect p => PatchInfoAnd p x y -> DarcsChange


instance IsRepository DarcsRepo where
    type ChangeType DarcsRepo = DarcsChange

    repoDir = darcsRepoDir

    reverseChanges repo =
        withRepositoryDirectory [] (repoDir repo) $ RepoJob $ \darcs ->
            mapRL Change . newset2RL <$> readRepo darcs


instance IsChange DarcsChange where
    type ChangeInfoType DarcsChange = PatchInfo

    changedFiles (Change p) = listTouchedFiles p
    changeInfo   (Change p) = info p


-- instance IsChangeInfo PatchInfo is defined in Interface module
