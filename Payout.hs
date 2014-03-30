module Payout where

import Control.Applicative
import Control.Exception (assert)
import Data.Fixed (Centi)
import Data.Map (Map)
import qualified Data.Map as Map

import Database.SQLite.Simple (SQLData(..))
import Database.SQLite.Simple.Ok (Ok(..))
import Database.SQLite.Simple.FromRow (FromRow(..), field, fieldWith)
import Database.SQLite.Simple.FromField (fieldData)

type Account = String
type TxHash = String

data Transaction = Transaction {
		txhash  :: TxHash,
		account :: Account,
		amount  :: Centi,
		paidOut :: Centi
	} deriving (Show, Eq)

instance FromRow Transaction where
	fromRow = Transaction <$> field <*> field <*> fieldWith dbl <*> fieldWith dbl
		where
		dbl f = case fieldData f of
			SQLInteger i -> Ok $ fromIntegral i
			SQLFloat d -> Ok $ realToFrac d
			_ -> Errors []

mkAccountMap :: [Transaction] -> Map Account [Transaction]
mkAccountMap = Map.fromListWith (++) . map (\t -> (account t, [t]))

-- All transactions should be from the same account
-- They should also be in reverse ledger/date order
computeOnePayout :: Centi -> Centi -> [Transaction] -> (Centi, [(TxHash, Centi)])
computeOnePayout limit fee =
	assert (limit > 0) $
	assert (fee >= 0) $
	foldr (\(Transaction txhash _ amount paidOut) (sum,partials) ->
		assert (sum >= 0) $
		assert (paidOut < amount) $
		assert (limit >= sum) $
		assert (amount >= 0) $
		assert (paidOut >= 0) $
		if sum >= limit then
			(sum, partials) -- No more payout in this calculation
		else
			let
				-- Pay a fee if this is the first payout to include this transaction
				-- or the transaction total is greater than the (limit+fee)
				pay = if paidOut == 0 || amount > (limit+fee) then fee else 0
				-- Don't send already-paid-out or fee amounts
				amountToSend = max ((amount - paidOut) - pay) 0
				spaceLeft = limit - sum
			in if amountToSend > spaceLeft then
				-- Send the full limit
				-- This transaction is partially paid out at whatever space was left
				(limit, (txhash, paidOut + spaceLeft + pay):partials)
			else
				-- Send the full amount, this transaction is fully paid out
				(sum+amountToSend, (txhash, amount):partials)
	) (0,[])
