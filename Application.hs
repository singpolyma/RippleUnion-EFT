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
import Network.Wai.Util (stringHeaders, textBuilder, queryLookup)

import qualified Vogogo as Vgg
import qualified Vogogo.Customer as VggC

import Network.URI (URI(..), URIAuth(..))
import Network.URI.Partial (relativeTo)

import Database.SQLite.Simple (query, execute, Connection, Query)
import Database.SQLite.Simple.ToRow (ToRow)

import qualified Ripple.Amount as Ripple

import Records
import Federation
import MustacheTemplates
#include "PathHelpers.hs"

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

Just [htmlCT] = stringHeaders [("Content-Type", "text/html; charset=utf-8")]

showAccount :: Action (Word32 -> Application)
showAccount _ db _ _ account _ = do
	txs <- query' [
			"SELECT UPPER(txhash),ledger_index,amount,",
			"CASE WHEN paid_out == 0 THEN 'pending'",
			"     WHEN paid_out >= amount THEN 'sent'",
			"     ELSE 'partial'",
			"END ",
			"FROM transactions WHERE dt=? AND currency='CAD' ",
			"ORDER BY ledger_index DESC"
		] [toInteger account]

	ledger <- fromInteger . head . head <$> query' [
			"SELECT ledger_index FROM transactions ",
			"ORDER BY ledger_index DESC LIMIT 1"
		] ([] :: [Int])

	textBuilder ok200 [htmlCT] $ viewShowAccount htmlEscape
		(ShowAccount [Header ledger] account txs)
	where
	query' sql = liftIO . query db (s $ concat sql)
