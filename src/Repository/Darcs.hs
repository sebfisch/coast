module Repository.Darcs where


import           Import

import           Repository.Interface

import           Darcs.Patch.Info         (PatchInfo)
import           Darcs.Patch.Inspect      (listTouchedFiles)
import           Darcs.Patch.PatchInfoAnd (info)
import           Darcs.Patch.Set          (newset2RL)
import           Darcs.Repository         (RepoJob (..), readRepo,
                                           withRepositoryDirectory)
import           Darcs.Witnesses.Ordered  (mapRL)
import           Darcs.Witnesses.Sealed   (seal2, unseal2)

import           Data.Maybe               (listToMaybe)
import           System.Directory         (doesDirectoryExist)
import           System.FilePath          ((</>), equalFilePath)


newtype DarcsRepo   = DarcsRepo     { darcsRepoDir          :: FilePath     }


instance IsRepository DarcsRepo where
    type ChangeInfoType DarcsRepo = PatchInfo

    repoDir = darcsRepoDir

    -- http://stackoverflow.com/questions/12941793/how-to-use-the-darcs-library-to-query-information-about-patches
    -- TODO: don't search through patches again for every file in directory
    lastChangeInfo repo file =
        withRepositoryDirectory [] (repoDir repo) $ RepoJob $ \darcs -> do
            patches <- mapRL seal2 . newset2RL <$> readRepo darcs
            -- patches <- filterPatches darcs [file] patches -- darcs 2.9+
            let wanted = unseal2 (any (equalFilePath file) . listTouchedFiles)
                patch  = listToMaybe . filter wanted $ patches
            return $ unseal2 info <$> patch


isDarcsRepository :: FilePath -> IO Bool
isDarcsRepository path = do
    isDir <- doesDirectoryExist path
    if isDir then
        doesDirectoryExist $ path </> "_darcs"
      else
        return False
