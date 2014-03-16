module Records where

import Prelude ()
import BasicPrelude
import Data.Fixed (Centi)

import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Network.URI (URI)
import Data.Base58Address (RippleAddress)
import Data.Aeson (ToJSON(..), object, (.=))

import Data.Text.Buildable
import Database.SQLite.Simple (SQLData(..), Connection)
import Database.SQLite.Simple.Ok (Ok(..))
import Database.SQLite.Simple.FromRow (FromRow(..), field, fieldWith)
import Database.SQLite.Simple.FromField (fieldData)
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

import qualified Ripple.Amount as Ripple
import qualified Vogogo as Vgg

fee :: Rational
fee = 5

limit :: Rational
limit = 500

s :: (IsString s) => String -> s
s = fromString

type Action a = URI -> Connection -> Vgg.Auth -> RippleAddress -> a

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

instance Buildable URI where
	build = build . show

data ShowAccount = ShowAccount {
		header :: [Header],
		showAccountDT :: Word32,
		transactions :: [Transaction]
	}

data Form = Form {
		formHtml   :: Html,
		formAction :: URI
	}

instance Eq Form where
	(Form _ a1) == (Form _ a2) = a1 == a2

data Header = Header {
		lastSeenLedger :: Word32
	}

data Transaction = Transaction {
		txhash :: String,
		ledger_index :: Word32,
		amount :: Centi,
		status :: String
	}

instance FromRow Transaction where
	fromRow = Transaction <$> field <*> fmap fromInteger field <*> fieldWith dbl <*> field
		where
		dbl f = case fieldData f of
			SQLInteger i -> Ok $ fromIntegral i
			SQLFloat d -> Ok $ realToFrac d
			_ -> Errors []

-- Federation stuff

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
		quoteAmount :: Ripple.Amount
	}

instance ToJSON Quote where
	toJSON (Quote ripple dt amount) = object [
			s"result" .= "success",
			s"quote" .= object [
				s"address" .= show ripple,
				s"destination_tag" .= dt,
				s"send" .= [amount]
			]
		]

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
