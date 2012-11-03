module Repository.Interface where


import           Prelude

import           Data.Text       (Text)
import           System.Time     (CalendarTime)


class IsRepository repo where
    data ChangeInfoType repo

    repoDir         :: repo -> FilePath
    lastChangeInfo  :: repo -> FilePath -> IO (Maybe (ChangeInfoType repo))

    changeTime      :: ChangeInfoType repo -> CalendarTime
    changeAuthor    :: ChangeInfoType repo -> Text
    changeSummary   :: ChangeInfoType repo -> Text
    changeMessage   :: ChangeInfoType repo -> Text


data Repository where
    Repo :: IsRepository repo => repo -> Repository


instance IsRepository Repository where

    data ChangeInfoType Repository where
        Info    :: IsRepository repo
                => ChangeInfoType repo -> ChangeInfoType Repository

    repoDir         (Repo repo)         = repoDir repo
    lastChangeInfo  (Repo repo) file    = Info <$$> lastChangeInfo repo file

    changeTime      (Info info)         = changeTime    info
    changeAuthor    (Info info)         = changeAuthor  info
    changeSummary   (Info info)         = changeSummary info
    changeMessage   (Info info)         = changeMessage info


(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
