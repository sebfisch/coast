{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Coast - Code Host"
    $(widgetFile "homepage")
