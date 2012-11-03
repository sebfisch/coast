module Handler.Repo (

    getRepoR

    ) where


import           Import

import           Repository          (Repository, lastChangeInfo, repoDir)
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
getRepoR []             = redirect HomeR
getRepoR (name:path)    = do
    maybeRepo <- lookupRepository name

    case maybeRepo of
        Nothing   -> notFound
        Just repo -> serveRepo repo name path

serveRepo :: Repository -> String -> [String] -> Handler RepHtml
serveRepo repo name path = do
    let fullPath = joinPath (repoDir repo:path)

    isDir   <- liftIO $ doesDirectoryExist fullPath
    isFile  <- liftIO $ doesFileExist fullPath

    if isDir then do
        contents <- liftIO $ getAnnotatedContents repo path
        defaultLayout $ repos_dir (name:path) contents $
                            \file -> RepoR $ name : path ++ [file]
      else if isFile then do
        rawRequested    <- isJust <$> lookupGetParam "raw"
        isText          <- liftIO $ guessIfTextFile fullPath
        if rawRequested then do
            sendFile (guessContentType isText fullPath) fullPath
          else do
            maybeText <- liftIO $ readIfTextFile isText fullPath
            defaultLayout $ repos_file (name:path) maybeText
      else
        notFound


getAnnotatedContents :: Repository -> [String] -> IO [DirEntry]
getAnnotatedContents repo path = do
    let fullPath = joinPath (repoDir repo:path)

    contents <- getDirectoryContents fullPath

    let filtered    = [file | file <- contents, not $ file `elem` hiddenFiles]
        sorted      = sortOn (map toLower) filtered

    mapM (annotate repo path) sorted
  where
    hiddenFiles = "." : if null path then ["..","_darcs"] else []


annotate :: Repository -> [String] -> String -> IO DirEntry
annotate repo path name = do
    let prefix = joinPath (repoDir repo:path)

    isDir <- liftIO $ doesDirectoryExist $ prefix </> name

    if isDir then
        return $ Dir name
      else
        File name <$> lastChangeInfo repo (joinPath $ path ++ [name])


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
