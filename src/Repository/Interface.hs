module Repository.Interface
    ( Repository, Change, ChangeInfo

    , someRepo

    , IsRepository(..), IsChange(..), IsChangeInfo(..)
    
    , (<$$>)

    ) where


import           Prelude

import           Darcs.Patch.Info (PatchInfo, piAuthor, piDate, piLog, piName)

import           Data.Text        (Text, pack)
import           System.Time      (CalendarTime)


class IsChange (ChangeType repo) => IsRepository repo where
    type ChangeType repo

    repoDir         :: repo -> FilePath
    reverseChanges  :: repo -> IO [ChangeType repo]


class IsChangeInfo (ChangeInfoType change) => IsChange change where
    type ChangeInfoType change

    changedFiles    :: change -> [FilePath]
    changeInfo      :: change -> ChangeInfoType change


class IsChangeInfo info where
    changeTime      :: info -> CalendarTime
    changeAuthor    :: info -> Text
    changeSummary   :: info -> Text
    changeMessage   :: info -> Text


data Repository where
    Repo :: IsRepository repo => repo -> Repository


someRepo :: IsRepository repo => repo -> Repository
someRepo = Repo


instance IsRepository Repository where
    type ChangeType Repository  = Change

    repoDir         (Repo repo) = repoDir repo
    reverseChanges  (Repo repo) = Change <$$> reverseChanges repo


data Change where
    Change :: IsChange change => change -> Change


instance IsChange Change where
    type ChangeInfoType Change  = ChangeInfo

    changedFiles    (Change c)  = changedFiles c
    changeInfo      (Change c)  = Info $ changeInfo c


data ChangeInfo where
    Info :: IsChangeInfo info => info -> ChangeInfo


instance IsChangeInfo ChangeInfo where
    changeTime      (Info info) = changeTime    info
    changeAuthor    (Info info) = changeAuthor  info
    changeSummary   (Info info) = changeSummary info
    changeMessage   (Info info) = changeMessage info


(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap


-- otherwise orphan instances defined here


instance IsChangeInfo PatchInfo where
    changeTime      info = piDate info
    changeAuthor    info = pack . init . takeWhile ('<'/=) $ piAuthor info
    changeSummary   info = pack $ piName info
    changeMessage   info = pack . unlines $ piLog info
