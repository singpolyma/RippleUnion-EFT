import Control.Applicative
import Data.List
import Data.Function
import Data.Fixed (Centi)
import qualified Data.Map as Map

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2

import Payout

instance Arbitrary Transaction where
	arbitrary = do
		amount <- getPositive <$> arbitrary
		paidOut <- realToFrac <$> choose (0::Double, realToFrac amount) `suchThat` (< realToFrac amount)
		Transaction <$> vectorOf 64 arbitrary <*> pure amount <*> pure paidOut <*> (Account <$> arbitrary <*> arbitrary)

prop_mkAccountMap_byAccount :: [Transaction] -> Bool
prop_mkAccountMap_byAccount ts =
	all (\(acct,bucket) -> all (\t -> account t == acct) bucket) $
		Map.toList $ mkAccountMap ts

prop_mkAccountMap_keepsAll :: [Transaction] -> Bool
prop_mkAccountMap_keepsAll ts =
	length (concat $ Map.elems (mkAccountMap ts)) == length ts

prop_mkAccountMap_reversesOrder :: [Transaction] -> Bool
prop_mkAccountMap_reversesOrder ts =
	all (\(_,bucket) -> reverse bucket == sortByAmount bucket) $
		Map.toList $ mkAccountMap (sortByAmount ts)
	where
	sortByAmount = sortBy (compare `on` amount)

prop_computeOnePayout_respectLimit :: Positive Centi -> Positive Centi -> [Transaction] -> Bool
prop_computeOnePayout_respectLimit (Positive limit) (Positive fee) ts =
	fst (computeOnePayout limit fee ts) <= limit

prop_computeOnePayout_partialsIncludePast :: Positive Centi -> Positive Centi -> [Transaction] -> Bool
prop_computeOnePayout_partialsIncludePast (Positive limit) (Positive fee) ts =
	all (\(tx,partial) -> fmap paidOut (find ((==tx).txhash) ts)<Just partial) $
	snd (computeOnePayout limit fee ts)

prop_computeOnePayout_positivePayout :: Positive Centi -> Positive Centi -> [Transaction] -> Bool
prop_computeOnePayout_positivePayout (Positive limit) (Positive fee) ts =
	fst (computeOnePayout limit fee ts) >= 0

prop_computeOnePayout_correctFees :: Positive Centi -> Positive Centi -> [Transaction] -> Bool
prop_computeOnePayout_correctFees (Positive limit) (Positive fee) ts =
	realToFrac total == totalNewRecorded - totalFees
	where
	totalFees = sum $
		map (\(tx,partial) -> case find ((==tx).txhash) ts of
			Just (Transaction _ amnt pO _)
				-- Can't charge full fee on microtransaction
				| pO == 0 -> realToFrac (if amnt <= fee then amnt else fee)
				| amnt > (limit+fee) -> realToFrac $
					if (amnt-pO) <= fee then
						amnt - pO
					else
						fee
			_ -> 0 :: Rational
		) partials
	totalNewRecorded = sum $
		map (\(tx,partial) -> case find ((==tx).txhash) ts of
			Just (Transaction _ _ pO _) ->
				(realToFrac partial - realToFrac pO) :: Rational
			_ -> error "TEST WRITER ERROR: Transaction missing"
		) partials
	(total,partials) = computeOnePayout limit fee ts

tests :: [Test]
tests =
	[
		testGroup "mkAccountMap" [
			testProperty "byAccount" prop_mkAccountMap_byAccount,
			testProperty "keepsAll" prop_mkAccountMap_keepsAll,
			testProperty "reversesOrder" prop_mkAccountMap_reversesOrder
		],
		testGroup "computeOnePayout" [
			testProperty "respectLimit" prop_computeOnePayout_respectLimit,
			testProperty "partialsIncludePast" prop_computeOnePayout_partialsIncludePast,
			testProperty "positivePayout" prop_computeOnePayout_positivePayout,
			testProperty "correctFees" prop_computeOnePayout_correctFees
		]
	]

main :: IO ()
main = defaultMain tests
