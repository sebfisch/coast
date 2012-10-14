{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import System.Directory
import Text.Lucius
import Control.Monad

getHomeR :: Handler RepHtml
getHomeR = do
    let links           = [(MsgHome,HomeR)]
        top_navigation  = $(widgetFile "top-navigation")
        makeRoute name  = RepoR [name]
    contents <- liftIO $ getDirectoryContents reposPath
    repos <- liftIO $ filterM isDarcsRepos contents
    defaultLayout $ do
        toWidget $(luciusFile "templates/repos-dir.lucius")
        setTitle "Coast - Code Host"
        $(widgetFile "homepage")

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