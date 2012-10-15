module Template (

    module Template.Data,

    homepage, repos_dir, repos_file

    ) where


import Import                   hiding (fileName)
import Template.Solarized       -- CSS color names
import Template.Data            -- custom data types used in templates

import Data.String              (fromString)
import Data.List                (inits, tail, last)
import System.FilePath          (joinPath)
import Text.Lucius              (luciusFile)


homepage :: [String] -> (String -> Route App) -> GWidget sub App ()
homepage repos makeRoute = do
    toWidget $(luciusFile "templates/repos-dir.lucius")
    setTitle "Coast - Code Host"
    $(widgetFile "homepage")


repos_dir   :: [String] -> [DirEntry] -> (String -> Route App)
            -> GWidget sub App ()
repos_dir names contents makeRoute = do
    setTitle $ fromString $ joinPath names
    $(widgetFile "repos-dir")


repos_file :: [String] -> Maybe Text -> GWidget sub App ()
repos_file names maybeText = do
    setTitle $ fromString $ joinPath names
    $(widgetFile "repos-file")


top_navigation :: GWidget sub App ()
top_navigation = $(widgetFile "top-navigation")
  where
    links = [(MsgHome, HomeR)]


repos_header :: [String] -> GWidget sub App ()
repos_header names = do
    toWidget $ [lucius|
        h3.repos {
            a, a:visited, a:active, a:hover {
                color: #{lightText};
            }
        }
    |]

    [whamlet|$newline always
        <h3 .repos>
            $forall subpath <- tail $ inits names
                / <a href="@{RepoR subpath}">#{last subpath}</a> #
    |]
