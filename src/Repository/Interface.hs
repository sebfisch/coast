module Repository.Interface where


import           Prelude

import           Darcs.Patch.Info (PatchInfo, piAuthor, piDate, piLog, piName)

import           Data.Text        (Text, pack)
import           System.Time      (CalendarTime)


class IsChangeInfo (ChangeInfoType repo) => IsRepository repo where
    type ChangeInfoType repo

    repoDir         :: repo -> FilePath
    lastChangeInfo  :: repo -> FilePath -> IO (Maybe (ChangeInfoType repo))


class IsChangeInfo info where
    changeTime      :: info -> CalendarTime
    changeAuthor    :: info -> Text
    changeSummary   :: info -> Text
    changeMessage   :: info -> Text


data Repository where
    Repo :: IsRepository repo => repo -> Repository


data ChangeInfo where
    Info :: IsChangeInfo info => info -> ChangeInfo


instance IsRepository Repository where
    type ChangeInfoType Repository = ChangeInfo

    repoDir         (Repo repo)         = repoDir repo
    lastChangeInfo  (Repo repo) file    = Info <$$> lastChangeInfo repo file


instance IsChangeInfo ChangeInfo where
    changeTime      (Info info)         = changeTime    info
    changeAuthor    (Info info)         = changeAuthor  info
    changeSummary   (Info info)         = changeSummary info
    changeMessage   (Info info)         = changeMessage info


(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap


-- otherwise orphan instances defined here


instance IsChangeInfo PatchInfo where
    changeTime      info = piDate info
    changeAuthor    info = pack . init . takeWhile ('<'/=) $ piAuthor info
    changeSummary   info = pack $ piName info
    changeMessage   info = pack . unlines $ piLog info
