module Template (

    homepage, repos_dir, repos_file,

    makePath

    ) where


import Import
import Template.Solarized       -- CSS color names

import Data.String              (fromString)
import Data.List                (inits, tail, last, intercalate)
import Text.Lucius              (luciusFile)


homepage :: [String] -> (String -> Route App) -> GWidget sub App ()
homepage repos makeRoute = do
    toWidget $(luciusFile "templates/repos-dir.lucius")
    setTitle "Coast - Code Host"
    $(widgetFile "homepage")


repos_dir   :: [String] -> [(Bool,String)] -> (String -> Route App)
            -> GWidget sub App ()
repos_dir names contents makeRoute = do
    setTitle $ fromString $ makePath names
    $(widgetFile "repos-dir")


repos_file :: [String] -> Maybe Text -> GWidget sub App ()
repos_file names maybeText = do
    setTitle $ fromString $ makePath names
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


makePath :: [FilePath] -> FilePath
makePath names = intercalate "/" names
