module Handler.Repo where

import Import
import System.Directory (doesDirectoryExist,doesFileExist)
import Data.List (intercalate)

getRepoR :: [String] -> Handler RepHtml
getRepoR []             = redirect HomeR
getRepoR names@(name:_) = do
    let links = [(name, RepoR [name])]
        top_navigation = $(widgetFile "top-navigation")
        path = intercalate "/" ("var":"repos":names)

    isDir   <- liftIO $ doesDirectoryExist path
    isFile  <- liftIO $ doesFileExist path

    if isDir then do
        defaultLayout $(widgetFile "homepage")
      else if isFile then do
        defaultLayout $(widgetFile "homepage")
      else notFound