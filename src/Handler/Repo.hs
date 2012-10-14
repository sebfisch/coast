module Handler.Repo where

import Import
import System.Directory
import Data.List
import Data.Char
import Data.Maybe
import Data.Function
import Data.String
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.ByteString as B
import qualified Data.Text.IO as T

getRepoR :: [String] -> Handler RepHtml
getRepoR [] = redirect HomeR
getRepoR names@(name:others) = do
    let links = [(MsgHome, HomeR)]
        top_navigation  = $(widgetFile "top-navigation")
        repos_header    = $(widgetFile "repos-header")
        fullPath        = makePath (reposPath:names)

    isDir   <- liftIO $ doesDirectoryExist fullPath
    isFile  <- liftIO $ doesFileExist fullPath

    if isDir then do
        contents <- liftIO $ getAnnotatedContents (null others) fullPath
        let makeRoute file = RepoR $ names ++ [file]
        defaultLayout $ do
            setTitle $ fromString $ makePath names
            $(widgetFile "repos-dir")
      else if isFile then do
        rawRequested <- isJust <$> lookupGetParam "raw"
        isText <- liftIO $ guessIfTextFile fullPath
        if rawRequested then do
            sendFile (guessContentType isText fullPath) fullPath
          else do
            maybeText <- liftIO $ readIfTextFile isText fullPath
            defaultLayout $ do
                setTitle $ fromString $ makePath names
                $(widgetFile "repos-file")
      else notFound

markDirectory :: FilePath -> FilePath -> IO (Bool,FilePath)
markDirectory prefix file = do 
    isDir <- liftIO $ doesDirectoryExist $ makePath [prefix,file]
    return (isDir,file)

getAnnotatedContents :: Bool -> FilePath -> IO [(Bool,FilePath)]
getAnnotatedContents isTopLevel fullPath = do
    contents <- getDirectoryContents fullPath
    let filtered    = [file | file <- contents, not $ file `elem` hiddenFiles]
        sorted      = sortOn (map toLower) filtered
    mapM (markDirectory fullPath) sorted
  where
    hiddenFiles = "." : if isTopLevel then ["..","_darcs"] else []

readIfTextFile :: Bool -> FilePath -> IO (Maybe Text)
readIfTextFile isText fullPath
    | isText    = Just <$> T.readFile fullPath
    | otherwise = return Nothing

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

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

peek :: Monad m => GLSink a m (Maybe a)
peek = await >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))
