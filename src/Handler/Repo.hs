module Handler.Repo (

    getRepoR

    ) where


import Import                   -- Yesod's Prelude
import Template                 (DirEntry(..), repos_dir, repos_file)

import Data.Char                (ord, toLower)
import Data.Conduit             (runResourceT, ($$))
import Data.Conduit.Binary      (sourceFile)
import Data.Maybe               (isJust)
import System.Directory         (doesDirectoryExist, doesFileExist
                                ,getDirectoryContents)
import System.FilePath          (joinPath, (</>))

import qualified Data.ByteString as B
import qualified Data.Text.IO as T


getRepoR :: [String] -> Handler RepHtml
getRepoR [] = redirect HomeR
getRepoR names@(_:others) = do
    let fullPath = joinPath (reposPath:names)

    isDir   <- liftIO $ doesDirectoryExist fullPath
    isFile  <- liftIO $ doesFileExist fullPath

    if isDir then do
        contents <- liftIO $ getAnnotatedContents (null others) fullPath
        defaultLayout $ repos_dir names contents $
                            \file -> RepoR $ names ++ [file]
      else if isFile then do
        rawRequested <- isJust <$> lookupGetParam "raw"
        isText <- liftIO $ guessIfTextFile fullPath
        if rawRequested then do
            sendFile (guessContentType isText fullPath) fullPath
          else do
            maybeText <- liftIO $ readIfTextFile isText fullPath
            defaultLayout $ repos_file names maybeText
      else
        notFound


getAnnotatedContents :: Bool -> FilePath -> IO [DirEntry]
getAnnotatedContents isTopLevel fullPath = do
    contents <- getDirectoryContents fullPath
    let filtered    = [file | file <- contents, not $ file `elem` hiddenFiles]
        sorted      = sortOn (map toLower) filtered
    mapM (markDirectory fullPath) sorted
  where
    hiddenFiles = "." : if isTopLevel then ["..","_darcs"] else []


markDirectory :: FilePath -> FilePath -> IO DirEntry
markDirectory prefix file = do 
    isDir <- liftIO $ doesDirectoryExist $ prefix </> file
    return $ DirEntry isDir file


guessIfTextFile :: FilePath -> IO Bool
guessIfTextFile fullPath = runResourceT $ do
    sourceFile fullPath $$ do
        chunk <- peek
        return $ maybe True isProbablyText chunk


isProbablyText :: B.ByteString -> Bool
isProbablyText bs
    | B.null bs     = True
    | 0 `B.elem` bs = False
    | otherwise     = 3 * B.length nonTextBytes <= B.length bs
  where
    nonTextBytes    = B.filter (not . (`elem` asciiChars)) bs
    asciiChars      = [32..126] ++
                        map (fromInteger . fromIntegral . ord) "\b\t\n\f\r"


guessContentType :: Bool -> FilePath -> ContentType
guessContentType isText _fullPath
    | isText    = typePlain
    | otherwise = typeOctet


readIfTextFile :: Bool -> FilePath -> IO (Maybe Text)
readIfTextFile isText fullPath
    | isText    = Just <$> T.readFile fullPath
    | otherwise = return Nothing
