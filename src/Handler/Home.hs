module Handler.Home (

    getHomeR

    ) where


import           Import
import           Template (homepage)


getHomeR :: Handler RepHtml
getHomeR = do
    repos <- getRepositories
    defaultLayout $ homepage repos $ \name -> RepoR [name]
