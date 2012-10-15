module Handler.Home (

    getHomeR

    ) where


import Import                   -- Yesod's Prelude
import Template                 (homepage)

import Control.Monad            (filterM)
import System.Directory         (getDirectoryContents, doesDirectoryExist)
import System.FilePath          ((</>))


getHomeR :: Handler RepHtml
getHomeR = do
    contents <- liftIO $ getDirectoryContents reposPath
    repos <- liftIO $ filterM isDarcsRepos contents
    defaultLayout $ homepage repos $ \name -> RepoR [name]


isDarcsRepos :: FilePath -> IO Bool
isDarcsRepos name = do
    isDir <- doesDirectoryExist fullName
    if isDir then
        doesDirectoryExist $ fullName </> "_darcs"
      else
        return False
  where
    fullName = reposPath </> name
