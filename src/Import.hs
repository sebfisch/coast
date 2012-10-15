module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
    , Text
    , sortOn, peek
    , reposPath
#if __GLASGOW_HASKELL__ < 704
    , (<>)
#endif
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
#if __GLASGOW_HASKELL__ < 704
import Data.Monoid (Monoid (mappend, mempty, mconcat))
#else
import Data.Monoid (Monoid (mappend, mempty, mconcat), (<>))
#endif
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Settings.StaticFiles
import Settings.Development

import Data.Conduit             (GLSink, await, leftover)
import Data.Function            (on)
import Data.List                (intercalate, sortBy)


#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif


sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)


peek :: Monad m => GLSink a m (Maybe a)
peek = await >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))


reposPath :: String
reposPath = "var/repos"
