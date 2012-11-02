module Handler.Repo (

    getRepoR

    ) where


import           Import

import           Repository.Darcs    (Repo, darcsRepo, lastChangeInfo)
import           Template            (DirEntry (..), repos_dir, repos_file)

import           Data.Char           (ord, toLower)
import           Data.Conduit        (runResourceT, ($$))
import           Data.Conduit.Binary (sourceFile)
import           Data.Maybe          (isJust)
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      getDirectoryContents)
import           System.FilePath     (joinPath, takeExtension, (</>))

import qualified Data.ByteString     as B
import qualified Data.Text.IO        as T


getRepoR :: [String] -> Handler RepHtml
getRepoR [] = redirect HomeR
getRepoR names@(name:others) = do
    let repoPath = joinPath [reposPath,name]
        fullPath = joinPath (reposPath:names)

    isDir   <- liftIO $ doesDirectoryExist fullPath
    isFile  <- liftIO $ doesFileExist fullPath

    if isDir then do
        contents <- liftIO $ getAnnotatedContents fullPath repoPath others
        defaultLayout $ repos_dir names contents $
                            \file -> RepoR $ names ++ [file]
      else if isFile then do
        rawRequested    <- isJust <$> lookupGetParam "raw"
        isText          <- liftIO $ guessIfTextFile fullPath
        if rawRequested then do
            sendFile (guessContentType isText fullPath) fullPath
          else do
            maybeText <- liftIO $ readIfTextFile isText fullPath
            defaultLayout $ repos_file names maybeText
      else
        notFound


getAnnotatedContents :: FilePath -> FilePath -> [String] -> IO [DirEntry]
getAnnotatedContents fullPath repoPath dirs = do
    contents <- getDirectoryContents fullPath
    let filtered    = [file | file <- contents, not $ file `elem` hiddenFiles]
        sorted      = sortOn (map toLower) filtered
    mapM (annotate fullPath (darcsRepo repoPath) dirs) sorted
  where
    hiddenFiles = "." : if null dirs then ["..","_darcs"] else []


annotate :: FilePath -> Repo -> [String] -> String -> IO DirEntry
annotate prefix repo dirs name = do
    isDir <- liftIO $ doesDirectoryExist $ prefix </> name
    if isDir then
        return $ Dir name
      else
        File name <$> lastChangeInfo repo (joinPath $ dirs ++ [name])


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
guessContentType isText fullPath =
    maybe (if isText then typePlain else typeOctet) id $
        lookup (takeExtension fullPath) contentTypes
  where
    contentTypes =
        [(".html",typeHtml) ,(".txt",typePlain) ,(".json",typeJson)
        ,(".xml",typeXml)   ,(".atom",typeAtom) ,(".rss",typeRss)
        ,(".jpg",typeJpeg)  ,(".jpeg",typeJpeg) ,(".png",typePng)
        ,(".gif",typeGif)   ,(".svg",typeSvg)   ,(".js",typeJavascript)
        ,(".css",typeCss)   ,(".flv",typeFlv)   ,(".ogv",typeOgv)]


readIfTextFile :: Bool -> FilePath -> IO (Maybe Text)
readIfTextFile isText fullPath
    | isText    = Just <$> T.readFile fullPath
    | otherwise = return Nothing
