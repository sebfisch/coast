module Template (

    module Template.Data,

    homepage, repos_dir, repos_file

    ) where


import Import                   hiding (fileName)
import Template.Solarized       -- CSS color names
import Template.Data            -- custom data types used in templates

import Repository               (changeTime, changeAuthor, changeSummary)

import Data.String              (fromString)
import Data.List                (inits, init, tail, last)
import System.FilePath          (joinPath)
import System.Time              (TimeDiff(..), getClockTime, toClockTime
                                ,diffClockTimes, normalizeTimeDiff)
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
    now <- liftIO $ getClockTime
    let timeAgo =
            timeDiffMsg . normalizeTimeDiff . diffClockTimes now . toClockTime
    $(widgetFile "repos-dir")


repos_file :: [String] -> Maybe Text -> GWidget sub App ()
repos_file names maybeText = do
    setTitle $ fromString $ joinPath names
    $(widgetFile "repos-file")


top_navigation :: GWidget sub App ()
top_navigation = $(widgetFile "top-navigation")
  where
    links = [(MsgHome, HomeR)]


repos_header :: [String] -> Bool -> GWidget sub App ()
repos_header names showRawLink = do
    toWidget $ [lucius|
        h3.repos {
            a, a:visited, a:active, a:hover {
                color: #{lightText};
            }
        }
    |]

    [whamlet|$newline always
        <h3 .repos>
            $forall subpath <- init $ tail $ inits names
                / <a href="@{RepoR subpath}">#{last subpath}</a> #
            $if showRawLink
                / <a href="?raw">#{last names}</a>
            $else
                / <a href="@{RepoR names}">#{last names}</a>
    |]


timeDiffMsg :: TimeDiff -> AppMessage
timeDiffMsg TimeDiff{..}
    | tdYear    > 0 = MsgYearsAgo tdYear
    | tdMonth   > 0 = MsgMonthsAgo tdMonth
    | tdDay     > 6 = MsgWeeksAgo (tdDay `div` 7)
    | tdDay     > 0 = MsgDaysAgo tdDay
    | tdHour    > 0 = MsgHoursAgo tdHour
    | tdMin     > 0 = MsgMinutesAgo tdMin
    | otherwise     = MsgJustNow
