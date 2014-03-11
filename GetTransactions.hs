module GetTransactions (catchUpTransactions) where

import Control.Applicative ((<$>))
import Control.Error (EitherT(..), runEitherT)
import Control.Monad.IO.Class (liftIO)
import Data.Base58Address (RippleAddress)
import qualified Network.WebSockets as WS

import Ripple.Transaction
import Ripple.WebSockets

-- | This function may return transactions earlier than the lower bound passed.
--   It will never return transactions later than the latest ledger when called,
--   (not even ledgers that happen after it is called but before it is done)
catchUpTransactions ::
	RippleAddress -- ^ Transactions to this address
	-> Integer    -- ^ Since this ledger (up to the current ledger)
	-> (Integer -> Transaction -> IO ())
	-> IO (Either RippleError Integer)
catchUpTransactions addr lastSeen k =
	WS.runClient "s_east.ripple.com" 443 "/" $ \conn -> runEitherT $ do
		sendJSON conn CommandLedgerClosed
		ResultLedgerClosed _ idx <- EitherT (getRippleResult <$> receiveJSON conn)
		byPages conn addr lastSeen idx k
		return idx

byPages :: WS.Connection -> RippleAddress -> Integer -> Integer -> (Integer -> Transaction -> IO ()) -> EitherT RippleError IO ()
byPages conn addr startLedger endLedger k = go 0
	where
	go offset = do
		sendJSON conn (CommandAccountTX addr pageSize (Just offset)
			(Just (startLedger-1)) (Just (endLedger+1)) False False)
		ResultAccountTX ts <- EitherT (getRippleResult <$> receiveJSON conn)

		case ts of
			[] -> return ()
			_ | any ((==endLedger).fst) ts ->
				liftIO $ mapM_ (uncurry k) $ filter ((/=endLedger).fst) ts
			  | otherwise -> do
				liftIO $ mapM_ (uncurry k) ts
				go (offset + pageSize)

pageSize :: Int
pageSize = 100
