module Handler.Home where

import Import
import Template

import System.Directory
import Control.Monad

getHomeR :: Handler RepHtml
getHomeR = do
    contents <- liftIO $ getDirectoryContents reposPath
    repos <- liftIO $ filterM isDarcsRepos contents
    defaultLayout $ homepage repos $ \name -> RepoR [name]

isDarcsRepos :: FilePath -> IO Bool
isDarcsRepos name = do
    isDir <- doesDirectoryExist fullName
    if isDir then do
        let darcsPath = makePath [fullName,"_darcs"]
        doesDirectoryExist darcsPath
      else
        return False
  where
    fullName = makePath [reposPath, name]