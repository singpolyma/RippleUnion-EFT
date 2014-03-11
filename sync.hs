module Main (main) where

import Prelude ()
import BasicPrelude
import Numeric (showHex)
import System.IO (hPutStrLn, stderr)
import Database.SQLite.Simple (Connection, open, execute, execute_, query_)

import qualified Data.Set as Set

import Ripple.Transaction
import qualified Ripple.Amount as A

import GetTransactions

s :: (IsString a) => String -> a
s = fromString

getAmount :: Maybe Field -> Maybe Field -> Maybe (Double, String)
getAmount (Just (DeliveredAmount amount)) _ = case amount of
	A.Amount rat (A.Currency (x,y,z) _) -> Just (realToFrac rat,[x,y,z])
	A.Amount rat A.XRP -> Just (realToFrac rat,"XRP")
getAmount _ amnt@(Just (Amount amount)) =
	getAmount (Just (DeliveredAmount amount)) amnt
getAmount _ _ = Nothing

hex :: (Integral a, Show a) => a -> String
hex i = showHex (toInteger i) ""

doInsert :: Connection -> Integer -> Maybe Field -> Maybe (Double, String) -> Maybe Field -> Maybe Field -> IO ()
doInsert db ledger (Just (TransactionHash txhash)) (Just (amount, curr))
	(Just (DestinationTag dt)) (Just (InvoiceID invoiceid)) =
		execute db (s"INSERT INTO transactions (txhash,ledger_index,amount,currency,dt,invoiceid) VALUES (?,?,?,?,?,?)")
			(hex txhash, ledger, amount, curr, dt, hex invoiceid)
doInsert db ledger (Just (TransactionHash txhash)) (Just (amount,curr))
	(Just (DestinationTag dt)) _ =
		execute db (s"INSERT INTO transactions (txhash,ledger_index,amount,currency,dt) VALUES (?,?,?,?,?)")
			(hex txhash, ledger, amount, curr, dt)
doInsert db ledger (Just (TransactionHash txhash)) (Just (amount, curr))
	_ (Just (InvoiceID invoiceid)) =
		execute db (s"INSERT INTO transactions (txhash,ledger_index,amount,currency,invoiceid) VALUES (?,?,?,?,?,?)")
			(hex txhash, ledger, amount, curr, hex invoiceid)
doInsert db ledger (Just (TransactionHash txhash)) (Just (amount,curr)) _ _ =
		execute db (s"INSERT INTO transactions (txhash,ledger_index,amount,currency) VALUES (?,?,?,?)")
			(showHex txhash "", ledger, amount, curr)
doInsert _ _ txhash _ _ _ =
	hPutStrLn stderr $ "Invalid transaction: " ++ textToString (show txhash)

main :: IO ()
main = do
	[dbpth,srAddr] <- getArgs
	let addr = read srAddr
	db <- open $ textToString dbpth

	lastSeen <- query_ db (s"SELECT ledger_index FROM transactions ORDER BY ledger_index DESC LIMIT 1")
	let start = case lastSeen of
		[[idx]] -> idx
		_ -> error "Please insert a ledger bookmark into the database."

	result <- catchUpTransactions addr start $ \ledger (Transaction fields) -> do
		let fs = Set.fromList fields
		let headers = (,,) <$>
			Set.lookupLE (TransactionType {}) fs <*>
			Set.lookupLE (Destination {}) fs <*>
			Set.lookupLE (TransactionResult {}) fs
		case headers of
			Just (TransactionType Payment, Destination dst, TransactionResult 0)
				| dst == addr -> doInsert db ledger
					(Set.lookupLE (TransactionHash {}) fs)
					(getAmount
						(Set.lookupLE (DeliveredAmount {}) fs)
						(Set.lookupLE (Amount {}) fs)
					)
					(Set.lookupLE (DestinationTag {}) fs)
					(Set.lookupLE (InvoiceID {}) fs)
			_ -> return ()

	case result of
		Right e -> do
			execute_ db (s"DELETE FROM transactions WHERE txhash='bookmark'")
			execute db (s"INSERT INTO transactions (txhash,ledger_index,amount,currency) VALUES (?,?,?,?)")
				("bookmark", e, 0::Integer, "XXX")
		Left err -> hPutStrLn stderr $ textToString $ show err
