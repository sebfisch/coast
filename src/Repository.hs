module Repository

    ( darcsRepo         -- FilePath -> Repository

    , Repository

    , repoDir           -- Repository -> FilePath
    , reverseChanges    -- Repository -> IO [Change]

    , recentChanges     -- Repository -> [FilePath] -> IO [(FilePath,Change)]

    , Change

    , changedFiles      -- Change -> [FilePath]
    , changeInfo        -- Change -> ChangeInfo

    , ChangeInfo

    , changeTime        -- ChangeInfo -> CalendarTime
    , changeAuthor      -- ChangeInfo -> Text
    , changeSummary     -- ChangeInfo -> Text
    , changeMessage     -- ChangeInfo -> Text

    , (<$$>)

    ) where


import           Import

import           Repository.Darcs
import           Repository.Interface

import           Data.List            (isPrefixOf, partition)
import           System.FilePath      (dropTrailingPathSeparator, normalise)


darcsRepo :: FilePath -> Repository
darcsRepo = someRepo . DarcsRepo


recentChanges :: Repository -> [FilePath] -> IO [(FilePath,Change)]
recentChanges repo files = do
    changes <- reverseChanges repo
    -- darcs-2.9 has `filterPatches`, which would be useful here
    return $ changesAffecting files changes


changesAffecting :: [FilePath] -> [Change] -> [(FilePath,Change)]
changesAffecting []     _       = []
changesAffecting _      []      = []
changesAffecting files  (c:cs)  = zip affected (repeat c)
                                    ++ changesAffecting unaffected cs
  where
    touched                 = map normalise $ changedFiles c
    (affected,unaffected)   = partition (\f -> any (isPartOf f) touched) files


isPartOf :: FilePath -> FilePath -> Bool
isPartOf name changed =
    isPrefixOf name changed ||
    dropTrailingPathSeparator name == changed
