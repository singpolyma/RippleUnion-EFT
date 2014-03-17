{-# LANGUAGE CPP #-}
module Application (federationEndpoint, quoteEndpoint, showAccount, home, lookupAccount) where

import Prelude ()
import BasicPrelude
import Control.Error (eitherT, MaybeT(..), noteT)
import qualified Data.Text as T

import Network.Wai (Application, queryString)
import Network.HTTP.Types (ok200, badRequest400, seeOther303)
import Network.Wai.Util (stringHeaders, textBuilder, queryLookup, redirect', string)

import Network.URI (URI(..))
import Network.URI.Partial (relativeTo)

import Database.SQLite.Simple (query, Connection)

import Records
import Account
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

getHeader :: (MonadIO m) => Connection -> m Header
getHeader db = liftIO $
	Header . fromInteger . head . head <$> query db (s $ concat[
			"SELECT ledger_index FROM transactions ",
			"ORDER BY ledger_index DESC LIMIT 1"
		]) ([] :: [Int])

home :: Action Application
home root db _ _ _ = do
	header <- getHeader db
	textBuilder ok200 [htmlCT] $ viewHome htmlEscape $
		Home [header] (lookupAccountPath `relativeTo` root)

lookupAccount :: Action Application
lookupAccount root db vgg _ req = eitherT (string badRequest400 []) return $ do
	(t,i,a) <- (,,) <$> fromQ "transit" <*> fromQ "institution" <*> fromQ "account"

	dt <- noteT "Could not look up that account." $ MaybeT $
		fetchDT db vgg t i a (t++"-"++i++"-"++a)

	redirect' seeOther303 [] (showAccountPath dt `relativeTo` root)
	where
	fromQ k = noteT' ("No "++k++" provided.") $ fmap T.unpack $
		queryLookup k (queryString req)

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

	header <- getHeader db
	textBuilder ok200 [htmlCT] $ viewShowAccount htmlEscape
		(ShowAccount [header] account txs)
	where
	query' sql = liftIO . query db (s $ concat sql)
