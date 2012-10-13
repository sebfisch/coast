{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Coast - Code Host"
    let links = [(MsgHome,HomeR)]
        top_navigation = $(widgetFile "top-navigation")
    $(widgetFile "homepage")
