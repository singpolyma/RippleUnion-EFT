{-# LANGUAGE CPP #-}
module Federation (federationEndpoint, quoteEndpoint) where

import Prelude ()
import BasicPrelude
import Data.Fixed (Centi)
import Control.Error (eitherT, MaybeT(..), throwT, noteT, atMay)
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
#define NO_webhook
#include "PathHelpers.hs"

Just [cors] = stringHeaders [("Access-Control-Allow-Origin", "*")]

parseAccountNumbers :: Text -> Maybe (Text,Text,Text)
parseAccountNumbers t
	| length pieces /= 3 = Nothing
	| T.length i /= 3 = Nothing
	| otherwise = let [t,i,a] = pieces in
		Just (t, i, a)
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
	domain <- fromQ "domain"
	when (domain /= rootDomain) $ throwT nodomain

	json ok200 [cors] (ShouldQuote destination domain (quoteEndpointPath `relativeTo` root) (atMay pieces 0, atMay pieces 1, atMay pieces 2))
	where
	destination = fromMaybe T.empty $ fromQ' "destination"
	pieces = T.splitOn (s"-") destination
	Just rootDomain = T.pack . uriRegName <$> uriAuthority root
	fromQ' k = queryLookup k (queryString req)
	fromQ k = noteT' (FederationError InvalidParams ("No "++k++" provided.")) $
		fromQ' k

quoteEndpoint :: Action Application
quoteEndpoint _ db vgg rAddr req = eitherT err return $ do
	{- Current client does not send domain for quote request
	(domain,account) <- (,) <$> fromQ "domain" <*> fromQ "destination"
	when (domain /= rootDomain) $ throwT nodomain
	-}

	(t',i',a') <- case fromQ' "transit" of
		(Just t) -> (,,) <$> pure t <*> fromQ "institution" <*> fromQ "account"
		Nothing -> fromQ "destination" >>= noteT' invalidAccount . parseAccountNumbers
	let (t,i,a) = (T.unpack t', T.unpack i', T.unpack a')

	(samnt:currency:_) <- T.splitOn (s"/") <$> fromQ "amount"
	when (currency /= s"CAD") $ throwT invalidCurrency
	amnt <- noteT' (FederationError InvalidParams "Invalid amount")
		(realToFrac <$> (readMay samnt :: Maybe Centi))

	(dt,lim) <- noteT (FederationError InvalidParams "Invalid account") $ MaybeT$
		fetchDT db vgg t i a (t ++ "-" ++ i ++ "-" ++ a)

	when (amnt > fromIntegral lim) $
		throwT $ FederationError InvalidParams "Over limit"

	json ok200 [cors] (Quote rAddr dt
		(Ripple.Amount (amnt + fromIntegral fee) $ Ripple.Currency ('C','A','D') rAddr))

	where
	query' sql = liftIO . query db (s sql)

	fromQ' k = queryLookup k (queryString req)
	fromQ k = noteT' (FederationError InvalidParams ("No "++k++" provided.")) $
		fromQ' k
