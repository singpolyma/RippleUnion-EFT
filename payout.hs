module Main (main) where

import Prelude ()
import BasicPrelude
import System.IO (hPutStrLn, stderr)
import Database.SQLite.Simple (Connection, open, execute, execute_, query_)

import Data.Map (Map)
import qualified Data.Map as Map

import Payout

s :: (IsString a) => String -> a
s = fromString

main :: IO ()
main = do
	[dbpth] <- getArgs
	db <- open $ textToString dbpth

	transactions <- query_ db (s"SELECT txhash,vogogo_uuid,amount,paid_out FROM transactions INNER JOIN accounts ON transactions.dt = accounts.id WHERE currency='CAD' AND dt AND paid_out < amount ORDER BY ledger_index ASC")

	let forAccounts = mkAccountMap transactions
	let payouts = Map.toList $ Map.map (computeOnePayout 100 2) forAccounts

	forM_ payouts $ \(account, (payout, partials)) -> do
		execute_ db (s"BEGIN TRANSACTION")

		forM_ partials $ \(txhash, amount) -> do
			execute db (s"UPDATE transactions SET paid_out = ? WHERE txhash = ?")
				(realToFrac amount :: Double, txhash)

		hPutStrLn stderr $ "SENDING PAYOUT OF " ++ textToString (show payout) ++ " TO " ++ textToString (show account)

		execute_ db (s"END TRANSACTION")
