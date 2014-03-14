{-# LANGUAGE CPP #-}
module Application (federationEndpoint, quoteEndpoint) where

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
#include "PathHelpers.hs"

fee :: Rational
fee = 5

limit :: Rational
limit = 500

type Action a = URI -> Connection -> Vgg.Auth -> RippleAddress -> a
Just [cors] = stringHeaders [("Access-Control-Allow-Origin", "*")]

noteT' :: (Monad m) => e -> Maybe a -> EitherT e m a
noteT' e = noteT e . hoistMaybe

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
quoteEndpoint root db vgg rAddr req = eitherT err return $ do
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

	when (amnt > limit) $ throwT $ FederationError InvalidParams "Over limit"

	Vgg.UUID uuid <- fmap Vgg.uuid $ fmapLT apiErr $ EitherT $ liftIO $
		VggC.createAccount vgg $
			VggC.BankAccount (T.unpack account) t i a (read $ s"CAD")

	fdt <- query' "SELECT id FROM accounts WHERE vogogo_uuid = ? LIMIT 1" [uuid]
	dt <- case fdt of
		[[dt]] -> return dt
		_ ->  do
			rdt <- liftIO $ randomRIO (99999,199999999)
			fst <$> insertSucc db (s"INSERT INTO accounts VALUES(?,?)")
				(first succ) (rdt, uuid)

	json ok200 [cors] (Quote rAddr (fromInteger dt)
		(Ripple.Amount (amnt+fee) $ Ripple.Currency ('C','A','D') rAddr))

	where
	query' sql = liftIO . query db (s sql)

	apiErr Vgg.APIParamError = FederationError InvalidParams "Invalid account"
	apiErr _ = FederationError Unavailable "Something went wrong"

	Just rootDomain = T.pack . uriRegName <$> uriAuthority root
	fromQ k = noteT' (FederationError InvalidParams ("No "++k++" provided.")) $
		queryLookup k (queryString req)

-- | Increments id until success
insertSucc :: (MonadIO m,ToRow a) => Connection -> Query -> (a -> a) -> a -> m a
insertSucc db q succ x = liftIO $ do
	r <- try $ execute db q x
	case r of
		Left (SQLError ErrorConstraint _ _) ->
			insertSucc db q succ (succ x)
		Left e -> throwIO e
		Right () -> return x
