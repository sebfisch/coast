module Handler.Repo where

import Import
import System.Directory
import Data.List
import Data.Char
import Data.Function

reposPath :: FilePath
reposPath = "var/repos"

makePath :: [FilePath] -> FilePath
makePath path = intercalate "/" path

getRepoR :: [String] -> Handler RepHtml
getRepoR []             = redirect HomeR
getRepoR names@(name:_) = do
    let links = [(name, RepoR [name])]
        top_navigation = $(widgetFile "top-navigation")
        path = makePath names
        fullPath = makePath (reposPath:names)

    isDir   <- liftIO $ doesDirectoryExist fullPath
    isFile  <- liftIO $ doesFileExist fullPath

    if isDir then do
        contents <- liftIO $
                        (   fmap (sortOn (map toLower)) $
                            fmap (filter ((/='.') . head)) $
                            fmap (filter (/="_darcs")) $
                            getDirectoryContents fullPath
                        ) >>= mapM (markDirectory fullPath)
        let makeRoute file = RepoR $ names ++ [file]
        defaultLayout $(widgetFile "repos-dir")
      else if isFile then do
        defaultLayout $(widgetFile "homepage")
      else notFound

markDirectory :: FilePath -> FilePath -> IO (Bool,FilePath)
markDirectory prefix file = do 
    isDir <- liftIO $ doesDirectoryExist $ makePath [prefix,file]
    return (isDir,file)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)
