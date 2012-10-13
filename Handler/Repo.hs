module Handler.Repo where

import Import

getRepoR :: String -> Handler RepHtml
getRepoR name = do
    let links = [(name, RepoR name)]
        top_navigation = $(widgetFile "top-navigation")
    defaultLayout $(widgetFile "homepage")
