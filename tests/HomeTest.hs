{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import TestImport

homeSpecs :: Specs
homeSpecs =
  describe "These are some tests" $
    it "loads the index page" $ do
      get_ "/"
      statusIs 200
