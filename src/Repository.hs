module Repository

    ( Repository

    , darcsRepo         -- FilePath -> Repository

    , repoDir           -- Repository -> FilePath

    , ChangeInfo

    , lastChangeInfo    -- Repository -> FilePath -> IO (Maybe ChangeInfo)

    , changeTime        -- ChangeInfo -> CalendarTime
    , changeAuthor      -- ChangeInfo -> Text
    , changeSummary     -- ChangeInfo -> Text
    , changeMessage     -- ChangeInfo -> Text

    ) where


import           Import

import           Repository.Darcs
import           Repository.Interface


darcsRepo :: FilePath -> Repository
darcsRepo = Repo . DarcsRepo
