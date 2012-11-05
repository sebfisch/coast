module Handler.Repo (

    getRepoR

    ) where


import           Import

import           Repository          (ChangeInfo, Repository, changeInfo,
                                      recentChanges, repoDir, (<$$>))
import           Template            (DirEntry (..), repos_dir, repos_file)

import           Control.Arrow       (first, second)
import           Data.Char           (ord)
import           Data.Conduit        (runResourceT, ($$))
import           Data.Conduit.Binary (sourceFile)
import           Data.List           (sort)
import           Data.Maybe          (isJust)
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      getDirectoryContents)
import           System.FilePath     (addTrailingPathSeparator,
                                      dropTrailingPathSeparator, joinPath,
                                      takeExtension, takeFileName, (</>))

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
        contents <- liftIO $ getAnnotatedContents repo (joinPath path)
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


getAnnotatedContents :: Repository -> FilePath -> IO [DirEntry]
getAnnotatedContents repo path = do
    contents <- getDirectoryContents fullPath

    let filtered = [file | file <- contents, not $ file `elem` hiddenFiles]

    (flags,files)   <- unzip <$> mapM (extend basePath path) filtered
    changeInfos     <- second changeInfo <$$> recentChanges repo files

    return $ sort $ zipWith (makeDirEntry changeInfos) flags filtered
  where
    basePath    = repoDir repo
    fullPath    = basePath </> path
    hiddenFiles = "." : if null path then ["..","_darcs"] else []


extend :: FilePath -> FilePath -> String -> IO (Bool,FilePath)
extend base prefix name = do
    isFile <- doesFileExist $ base </> relativeName
    if isFile then
        return (True, relativeName)
      else
        return (False, addTrailingPathSeparator relativeName)
  where
    relativeName = prefix </> name


makeDirEntry :: [(String, ChangeInfo)] -> Bool -> String -> DirEntry
makeDirEntry changeInfos isFile name =
    DirEntry isFile name $ lookupOn baseName name changeInfos
  where
    baseName = takeFileName . dropTrailingPathSeparator


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


lookupOn :: Eq k => (a -> k) -> k -> [(a,b)] -> Maybe b
lookupOn f k = lookup k . map (first f)
