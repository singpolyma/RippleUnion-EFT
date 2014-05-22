module Main (main) where

import Prelude (read)
import BasicPrelude hiding (getArgs, read)
import Data.Fixed (Centi)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import OpenSSL (withOpenSSL)
import Database.SQLite.Simple (Connection, open, execute, execute_, query_)

import qualified Vogogo as Vgg
import qualified Vogogo.Account as Vgg
import qualified Vogogo.Transaction as Vgg

import Data.Map (Map)
import qualified Data.Map as Map

import Payout

fee :: Centi
fee = 5

s :: (IsString a) => String -> a
s = fromString

main :: IO ()
main = withOpenSSL $ do
	[dbpth,vuser,vkey,vtoken] <- getArgs
	db <- open dbpth

	let vogogo = Vgg.Auth vuser vkey vtoken
	Right wallet <- Vgg.getWallet vogogo

	let type_concat = "((',' || GROUP_CONCAT(verification_type) || ','))"
	transactions <- query_ db (s $ concat [
			"SELECT txhash,amount,paid_out,vogogo_uuid, ",

			"CASE ",
			"WHEN " ++ type_concat ++ " LIKE '%,InPersonVerification,%' THEN 5000 ",
			"ELSE 500 ",
			"END AS 'limit' ",

			"FROM ",
			"transactions ",
			"INNER JOIN accounts ON transactions.dt = accounts.id ",
			"LEFT JOIN verifications ON verifications.item_table='accounts' AND verifications.item_id=accounts.id ",

			"WHERE currency='CAD' AND dt AND paid_out < amount ",
			"GROUP BY txhash ",
			"ORDER BY ledger_index ASC "
		])

	let payouts = Map.toList $
		Map.mapWithKey (\(Account _ limit) -> computeOnePayout limit fee) $
		mkAccountMap transactions

	forM_ payouts $ \(account, (payout, partials)) -> do
		execute_ db (s"BEGIN TRANSACTION")

		forM_ partials $ \(txhash, amount) -> do
			execute db (s"UPDATE transactions SET paid_out = ? WHERE txhash = ?")
				(realToFrac amount :: Double, txhash)

		hPutStrLn stderr $ "SENDING PAYOUT OF " ++ textToString (show payout) ++ " TO " ++ textToString (show account)
		print =<< Vgg.sendEFT vogogo wallet (Vgg.UUID account) payout (read "CAD")

		execute_ db (s"END TRANSACTION")
