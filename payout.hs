module Main (main) where

import Prelude (read)
import BasicPrelude hiding (getArgs, read)
import Data.Fixed (Centi)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Database.SQLite.Simple (Connection, open, execute, execute_, query_)

import qualified Vogogo as Vgg
import qualified Vogogo.Account as Vgg
import qualified Vogogo.Transaction as Vgg

import Data.Map (Map)
import qualified Data.Map as Map

import Payout

fee :: Centi
fee = 5

limit :: Centi
limit = 500

s :: (IsString a) => String -> a
s = fromString

main :: IO ()
main = do
	[dbpth,vuser,vkey,vtoken] <- getArgs
	db <- open dbpth

	let vogogo = Vgg.Auth vuser vkey vtoken
	Right wallet <- Vgg.getWallet vogogo

	transactions <- query_ db (s"SELECT txhash,vogogo_uuid,amount,paid_out FROM transactions INNER JOIN accounts ON transactions.dt = accounts.id WHERE currency='CAD' AND dt AND paid_out < amount ORDER BY ledger_index ASC")

	let forAccounts = mkAccountMap transactions
	let payouts = Map.toList $ Map.map (computeOnePayout limit fee) forAccounts

	forM_ payouts $ \(account, (payout, partials)) -> do
		execute_ db (s"BEGIN TRANSACTION")

		forM_ partials $ \(txhash, amount) -> do
			execute db (s"UPDATE transactions SET paid_out = ? WHERE txhash = ?")
				(realToFrac amount :: Double, txhash)

		hPutStrLn stderr $ "SENDING PAYOUT OF " ++ textToString (show payout) ++ " TO " ++ account
		print =<< Vgg.sendEFT vogogo wallet (Vgg.UUID account) payout (read "CAD")

		execute_ db (s"END TRANSACTION")
