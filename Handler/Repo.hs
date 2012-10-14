module Handler.Repo where

import Import
import System.Directory
import Data.List
import Data.Char
import Data.Function
import Data.String

reposPath :: FilePath
reposPath = "var/repos"

makePath :: [FilePath] -> FilePath
makePath path = intercalate "/" path

getRepoR :: [String] -> Handler RepHtml
getRepoR [] = redirect HomeR
getRepoR names@(name:others) = do
    let links = [(MsgCode, RepoR [name])]
        top_navigation = $(widgetFile "top-navigation")
        fullPath = makePath (reposPath:names)

    isDir   <- liftIO $ doesDirectoryExist fullPath
    isFile  <- liftIO $ doesFileExist fullPath

    if isDir then do
        contents <- liftIO $ getAnnotatedContents (null others) fullPath
        let makeRoute file = RepoR $ names ++ [file]
        defaultLayout $ do
            setTitle $ fromString $ makePath names
            $(widgetFile "repos-dir")
      else if isFile then do
        defaultLayout $(widgetFile "homepage")
      else notFound

getAnnotatedContents :: Bool -> FilePath -> IO [(Bool,FilePath)]
getAnnotatedContents isTopLevel fullPath = do
    contents <- getDirectoryContents fullPath
    let filtered    = [file | file <- contents, not $ file `elem` hiddenFiles]
        sorted      = sortOn (map toLower) filtered
    mapM (markDirectory fullPath) sorted
  where
    hiddenFiles = "." : if isTopLevel then ["..","_darcs"] else []

markDirectory :: FilePath -> FilePath -> IO (Bool,FilePath)
markDirectory prefix file = do 
    isDir <- liftIO $ doesDirectoryExist $ makePath [prefix,file]
    return (isDir,file)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)
