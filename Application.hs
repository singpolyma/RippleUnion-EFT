{-# LANGUAGE CPP #-}
module Application (federationEndpoint, quoteEndpoint, showAccount) where

import Prelude ()
import BasicPrelude
import System.Random (randomRIO)
import Control.Error (eitherT, EitherT(..), fmapLT, throwT, noteT, hoistMaybe)
import Data.Base58Address (RippleAddress)
import Database.SQLite3 (SQLError(..), Error(ErrorConstraint))
import qualified Data.Text as T

import Network.Wai (Application, Response, queryString)
import Network.HTTP.Types (ok200, badRequest400, notFound404)
import Network.Wai.Util (stringHeaders, json, queryLookup)

import qualified Vogogo as Vgg
import qualified Vogogo.Customer as VggC

import Network.URI (URI(..), URIAuth(..))
import Network.URI.Partial (relativeTo)

import Database.SQLite.Simple (query, execute, Connection, Query)
import Database.SQLite.Simple.ToRow (ToRow)

import qualified Ripple.Amount as Ripple

import Records
import Federation
#include "PathHelpers.hs"

showAccount :: Action (Word64 -> Application)
showAccount = undefined
