module Template (

    module Template.Data,

    homepage, repos_dir, repos_file

    ) where


import           Import             hiding (fileName)
import           Template.Data
import           Template.Solarized

import           Repository         (changeAuthor, changeSummary, changeTime)

import           Data.List          (init, inits, last, tail)
import           Data.String        (fromString)
import           System.FilePath    (joinPath)
import           System.Time        (CalendarTime (..), ClockTime, Day (..),
                                     TimeDiff (..), diffClockTimes,
                                     getClockTime, normalizeTimeDiff,
                                     toClockTime, toUTCTime)
import           Text.Lucius        (luciusFile)


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
    let timeAgo = timeDiffMsg now . toClockTime
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


timeDiffMsg :: ClockTime -> ClockTime -> AppMessage
timeDiffMsg now before
    | years   > 0   = MsgYearsAgo years
    | months  > 0   = MsgMonthsAgo months
    | weeks   > 0   = MsgWeeksAgo weeks
    | days    > 0   = MsgDaysAgo days
    | hours   > 0   = MsgHoursAgo hours
    | minutes > 0   = MsgMinutesAgo minutes
    | otherwise     = MsgJustNow
  where
    CalendarTime{..}    = toUTCTime now
    TimeDiff{..}        = normalizeTimeDiff $ diffClockTimes now before

    years   = approx ctYear                     tdYear  12  tdMonth
    months  = approx (succ $ fromEnum ctMonth)  tdMonth 30  tdDay
    days    = approx ctDay                      tdDay   24  tdHour
    hours   = tdHour
    minutes = tdMin

    weeks
        | max 4 (fromWeekDay ctWDay) <= days && days <= 7   = 1
        | otherwise                                         = days `div` 7

    approx :: Int -> Int -> Double -> Int -> Int
    approx current unit parts sub
        | unit == 0 && sub >= current   = 1
        | otherwise                     = round $ fromIntegral unit
                                                + fromIntegral sub / parts

    fromWeekDay Sunday  = 7
    fromWeekDay weekDay = fromEnum weekDay


timeClass :: AppMessage -> Text
timeClass (MsgYearsAgo _)   = "old"
timeClass (MsgMonthsAgo _)  = "old"
timeClass (MsgWeeksAgo w)   = if w > 1 then "old" else "new"
timeClass _                 = "new"
