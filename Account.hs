module Account (fetchDT) where

import Prelude ()
import BasicPrelude
import System.Random (randomRIO)
import Control.Error (eitherT, EitherT(..))

import Database.SQLite3 (SQLError(..), Error(ErrorConstraint))
import Database.SQLite.Simple (query, execute, Connection, Query)
import Database.SQLite.Simple.ToRow (ToRow)

import qualified Vogogo as Vgg
import qualified Vogogo.Customer as VggC

import Records

fetchDT :: (MonadIO m) => Connection -> Vgg.Auth -> String -> String -> String -> String -> m (Maybe Word32)
fetchDT db vgg t i a account =
	eitherT (const $ return Nothing) (return . Just) $ do
		Vgg.UUID uuid <- fmap Vgg.uuid $ EitherT $ liftIO $
			VggC.createAccount vgg $
				VggC.BankAccount account t i a (read $ s"CAD")

		fdt <- liftIO $ query db (s"SELECT id FROM accounts WHERE vogogo_uuid = ? LIMIT 1") [uuid]
		case fdt of
			[[dt]] -> return $ fromInteger dt
			_ ->  do
				rdt <- liftIO $ randomRIO (99999,199999999)
				fst <$> insertSucc db (s"INSERT INTO accounts VALUES(?,?)")
					(first succ) (rdt, uuid)

-- | Increments id until success
insertSucc :: (MonadIO m,ToRow a) => Connection -> Query -> (a -> a) -> a -> m a
insertSucc db q succ x = liftIO $ do
	r <- try $ execute db q x
	case r of
		Left (SQLError ErrorConstraint _ _) ->
			insertSucc db q succ (succ x)
		Left e -> throwIO e
		Right () -> return x