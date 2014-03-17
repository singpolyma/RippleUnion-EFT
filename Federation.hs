{-# LANGUAGE CPP #-}
module Federation (federationEndpoint, quoteEndpoint) where

import Prelude ()
import BasicPrelude
import Control.Error (eitherT, MaybeT(..), throwT, noteT)
import qualified Data.Text as T

import Network.Wai (Application, Response, queryString)
import Network.HTTP.Types (ok200, badRequest400, notFound404)
import Network.Wai.Util (stringHeaders, json, queryLookup)

import Network.URI (URI(..), URIAuth(..))
import Network.URI.Partial (relativeTo)

import Database.SQLite.Simple (query)

import qualified Ripple.Amount as Ripple

import Records
import Account

#define NO_showAccount
#define NO_home
#define NO_lookupAccount
#include "PathHelpers.hs"

Just [cors] = stringHeaders [("Access-Control-Allow-Origin", "*")]

parseAccountNumbers :: Text -> Maybe (String,String,String)
parseAccountNumbers t
	| length pieces /= 3 = Nothing
	| T.length i /= 3 = Nothing
	| otherwise = let [t,i,a] = pieces in
		Just (T.unpack t, T.unpack i, T.unpack a)
	where
	pieces@(_:i:_) = T.splitOn (s"-") t

err :: (Monad m) => FederationError -> m Response
err e@(FederationError NoSuchUser _) = json notFound404 [cors] e
err e = json badRequest400 [cors] e

nodomain :: FederationError
nodomain = FederationError NoSuchDomain "That domain is not served here."

invalidAccount :: FederationError
invalidAccount = FederationError NoSuchUser "Invalid account numbers."

invalidCurrency :: FederationError
invalidCurrency = FederationError InvalidParams "Invalid currency"

federationEndpoint :: Action Application
federationEndpoint root _ _ _ req = eitherT err return $ do
	(domain,account) <- (,) <$> fromQ "domain" <*> fromQ "destination"
	when (domain /= rootDomain) $ throwT nodomain

	(_,_,_) <- noteT' invalidAccount $ parseAccountNumbers account

	json ok200 [cors] (ShouldQuote account domain (quoteEndpointPath `relativeTo` root))
	where
	Just rootDomain = T.pack . uriRegName <$> uriAuthority root
	fromQ k = noteT' (FederationError InvalidParams ("No "++k++" provided.")) $
		queryLookup k (queryString req)

quoteEndpoint :: Action Application
quoteEndpoint _ db vgg rAddr req = eitherT err return $ do
	account <- fromQ "destination"

	{- Current client does not send domain for quote request
	(domain,account) <- (,) <$> fromQ "domain" <*> fromQ "destination"
	when (domain /= rootDomain) $ throwT nodomain
	-}

	(t,i,a) <- noteT' invalidAccount $ parseAccountNumbers account

	(samnt:currency:_) <- T.splitOn (s"/") <$> fromQ "amount"
	when (currency /= s"CAD") $ throwT invalidCurrency
	amnt <- noteT' (FederationError InvalidParams "Invalid amount")
		(realToFrac <$> (readMay samnt :: Maybe Double))

	when (amnt > fromIntegral limit) $
		throwT $ FederationError InvalidParams "Over limit"

	dt <- noteT (FederationError InvalidParams "Invalid account") $ MaybeT $
		fetchDT db vgg t i a (T.unpack account)

	json ok200 [cors] (Quote rAddr dt
		(Ripple.Amount (amnt + fromIntegral fee) $ Ripple.Currency ('C','A','D') rAddr))

	where
	query' sql = liftIO . query db (s sql)

	fromQ k = noteT' (FederationError InvalidParams ("No "++k++" provided.")) $
		queryLookup k (queryString req)
