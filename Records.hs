module Records where

import Prelude ()
import BasicPrelude

import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Network.URI (URI)
import Text.Email.Validate (EmailAddress, validate)
import Data.Base58Address (RippleAddress)
import Data.Aeson (ToJSON(..), object, (.=))

import Data.Text.Buildable
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromRow (FromRow(..), field, fieldWith)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (ToField(..), toField)
import Database.SQLite.Simple.FromField (fieldData, ResultError(ConversionFailed))
import Database.SQLite.Simple.Ok (Ok(Ok, Errors))
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

serviceLimit :: Int
serviceLimit = 100

serviceFee :: Int
serviceFee = 2

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

instance Buildable URI where
	build = build . show

instance ToRow Deposit where
	toRow (Deposit rid fn email tel ripple amnt complete) =
		[toField rid, toField fn, toField (show email), toField tel, toField (show ripple), toField amnt, toField complete]

instance FromRow Deposit where
	fromRow = Deposit <$> field <*> field <*> fieldWith emailF <*> field <*> fieldWith rippleF <*> field <*> field
		where
		emailF f = case fieldData f of
			(SQLText t) -> case validate (encodeUtf8 t) of
				Left e -> Errors [toException $ ConversionFailed "TEXT" "EmailAddress" e]
				Right email -> Ok email
			_ -> Errors [toException $ ConversionFailed "TEXT" "EmailAddress" "need a text"]

		rippleF f = case fieldData f of
			(SQLText t) -> case readMay t of
				Nothing -> Errors [toException $ ConversionFailed "TEXT" "RippleAddress" "invalid"]
				Just ripple -> Ok ripple
			_ -> Errors [toException $ ConversionFailed "TEXT" "RippleAddress" "need a text"]

instance (CanVerify a) => ToRow (Verification a) where
	toRow (Verification item typ notes token) = [
			toField itemId,
			toField itemTable,
			toField typ,
			toField notes,
			toField token
		]
		where
		(itemId, itemTable) = verifyItemData item

instance ToField VerificationType where
	toField = toField . show

class CanVerify a where
	verifyItemData :: a -> (Int64, String)

instance CanVerify Deposit where
	verifyItemData d = (depositId d, "deposits")

instance Eq Form where
	(Form _ a1) == (Form _ a2) = a1 == a2

data Home = Home {
		renderedDepositForm :: [Form],
		renderedQuoteForm   :: [Form]
	}

data Form = Form {
		formHtml   :: Html,
		formAction :: URI
	}

data DepositSuccess = DepositSuccess {
		successfulDeposit :: [Deposit],
		higherVerificationNeeded :: Bool,
		renderedStripeVerifyForm :: [Form],
		homeLink :: URI
	}

data Deposit = Deposit {
		depositId       :: Int64,
		depositorFN     :: Text,
		depositorEmail  :: EmailAddress,
		depositorTel    :: Text,
		depositorRipple :: RippleAddress,
		depositAmount   :: Double,
		depositComplete :: Bool
	}

data VerificationType = AutomatedPhoneVerification | ManualPhoneVerification | StripeVerification
	deriving (Show, Read, Enum)

data Verification a = Verification {
		verificationItem :: a,
		verificationType :: VerificationType,
		verificationNotes :: Maybe Text,
		verificationAddrToken :: Maybe Text
	}

data PlivoDeposit = PlivoDeposit {
		plivoCode :: String
	}

data PlivoConfig = PlivoConfig {
		plivoAuthId    :: String,
		plivoAuthToken :: String,
		plivoTel       :: String
	}

data Header = Header {
	}

instance Monoid Header where
	mempty = Header
	mappend _ _ = Header

instance Eq Header where
	_ == _ = False

header :: Header
header = Header

s :: (IsString s) => String -> s
s = fromString


data Alias = Alias {
		alias :: Text,
		domain :: Text,
		ripple :: RippleAddress,
		dt :: Maybe Word32
	}

instance ToJSON Alias where
	toJSON (Alias alias domain ripple dt) = object [
		s"federation_json" .= (object $ [
				s"type" .= "federation_record",
				s"destination" .= alias,
				s"domain" .= domain,
				s"destination_address" .= show ripple,
				s"currencies" .= [object [s"currency" .= "CAD"]]
			] ++ maybe [] (\x -> [s"dt" .= x]) dt)
		]

data ShouldQuote = ShouldQuote Text Text URI

instance ToJSON ShouldQuote where
	toJSON (ShouldQuote alias domain quoteURI) = object [
		s"federation_json" .= object [
				s"type" .= "federation_record",
				s"destination" .= alias,
				s"domain" .= domain,
				s"quote_url" .= show quoteURI,
				s"currencies" .= [object [s"currency" .= "CAD"]]
			]
		]

data Quote = Quote {
		quoteRipple :: RippleAddress,
		quoteDT     :: Word32,
		quoteAmount :: (Double, String)
	}

instance ToJSON Quote where
	toJSON (Quote ripple dt (amount,currency)) = object [
		s"federation_json" .= object [
			s"result" .= "success",
			s"quote" .= object [
				s"address" .= show ripple,
				s"destination_tag" .= dt,
				s"send" .= [object [
					s"currency" .= currency,
					s"value" .= show amount
				]]
			]
		]]

instance ToJSON FederationError where
	toJSON (FederationError typ message) = object [
			s"result" .= "error",
			s"error" .= typ,
			s"error_message" .= message
		]

instance ToJSON FederationErrorType where
	toJSON NoSuchUser = toJSON "noSuchUser"
	toJSON NoSupported = toJSON "noSupported"
	toJSON NoSuchDomain = toJSON "noSuchDomain"
	toJSON InvalidParams = toJSON "invalidParams"
	toJSON Unavailable = toJSON "unavailable"

data FederationErrorType = NoSuchUser | NoSupported | NoSuchDomain | InvalidParams | Unavailable

data FederationError = FederationError {
		federationErrorType :: FederationErrorType,
		federationErrorMessage :: String
	}
