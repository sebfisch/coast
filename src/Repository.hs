module Repository (

    Repository(..), Repo, someRepo

    ) where


import           Import

import           System.Time  (CalendarTime)


class Repository repo
  where
    data ChangeInfo repo

    repoDir         :: repo -> FilePath
    lastChangeInfo  :: repo -> FilePath -> IO (Maybe (ChangeInfo repo))

    changeTime      :: ChangeInfo repo -> CalendarTime
    changeAuthor    :: ChangeInfo repo -> Text
    changeSummary   :: ChangeInfo repo -> Text
    changeMessage   :: ChangeInfo repo -> Text


data Repo where
    Repo :: Repository repo => repo -> Repo


someRepo :: Repository repo => repo -> Repo
someRepo = Repo


instance Repository Repo where

    data ChangeInfo Repo where
        Info :: Repository repo => ChangeInfo repo -> ChangeInfo Repo

    repoDir         (Repo repo)         = repoDir repo
    lastChangeInfo  (Repo repo) file    = Info <$$> lastChangeInfo repo file

    changeTime      (Info info)         = changeTime    info
    changeAuthor    (Info info)         = changeAuthor  info
    changeSummary   (Info info)         = changeSummary info
    changeMessage   (Info info)         = changeMessage info


(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
