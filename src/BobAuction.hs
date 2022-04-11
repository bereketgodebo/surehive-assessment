{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module BobAuction 
     (

     ) where


import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P
import           Schema               (ToSchema)
import           Text.Printf          (printf)

-- an NFT must contain a minimum amount ada
minLovelace :: Integer
minLovelace = 2000000


-- The auction must be parametrized by the owner of the token,
-- the token itself, mininum bid amount and the deadline.
-- A token is represented by CurrencySymbol and TokenName

data Auction = Auction
    { aSeller   :: !PaymentPubKeyHash
    , aDeadline :: !POSIXTime
    , aMinBid   :: !Integer
    , aCurrency :: !CurrencySymbol
    , aToken    :: !TokenName
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

-- defining equality for Auction type
instance Eq Auction where 
    {-# INLINABLE (==) #-}
    a == b = (aSeller   a == aSeller   b) &&
             (aDeadline a == aDeadline b) &&
             (aMinBid   a == aMinBid   b) &&
             (aCurrency a == aCurrency b) &&
             (aToken    a == aToken    b) 

-- Generate FromData and ToData instance for Auction type
PlutusTx.unstableMakeIsData ''Auction
-- Class for types which can be lifted into Plutus IR
PlutusTx.makeLift ''Auction 

data Bid = Bid 
    { bBidder :: !PaymentPubKeyHash
    , bBid    :: !Integer 
    } deriving P.Show 

instance Eq Bid where 
    {-# INLINABLE (==) #-}
    b == c = (bBidder b == bBidder c) &&
             (bBid    b == bBid    c)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

-- bidders can either bid or close the auction
-- it does not matter if anyone invokes the close action
-- since the contract only closes an auction after the deadline 
data AuctionAction = MkBid Bid | Close 

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction

data AuctionDatum = AuctionDatum 
   { adAuction     :: !Auction 
   , adHighestBid :: !(Maybe Bid)
   } deriving P.Show 

PlutusTx.unstableMakeIsData ''AuctionDatum 
PlutusTx.makeLift ''AuctionDatum 

data Auctioning
instance Scripts.ValidatorTypes Auctioning where 
    type instance RedeemerType Auctioning = AuctionAction 
    type instance DatumType    Auctioning = AuctionDatum 


{-# INLINABLE minBid #-}
minBid :: AuctionDatum -> Integer 
minBid AuctionDatum{..} = case adHighestBid of 
    Nothing      -> aMinBid adAuction
    Just Bid{..} -> bBid + 1

{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx = 
    traceIfFalse "wrong input value" correctInputValue &&
    case redeemer of 
        MkBid b@Bid{..} ->
            traceIfFalse "bid too low"  (sufficientBid bBid)               &&
            traceIfFalse "Wrong output datum" (correctBidOutputDatum b)    &&
            traceIfFalse "wrong output value" (correctBidOutputValue bBid) &&
            traceIfFalse "wrong refund" correctBidRefund                   &&
            traceIfFalse "too late"      correctBidSlotRange
        Close          ->
            traceIfFalse "too early" correctCloseSlotRange &&
            case adHighestBid ad of 
                Nothing      ->
                    traceIfFalse "expected seller to get token" (getsValue (aSeller auction) $ tokenValue <> Ada.lovelaceValueOf minLovelace)
                Just Bid{..} ->
                    traceIfFalse "expected highest bidder to get token" (getsValue bBidder $ tokenValue <> Ada.lovelaceValueOf minLovelace) &&
                    traceIfFalse "expected seller to get highest bid" (getsValue (aSeller auction) $ Ada.lovelaceValueOf bBid)
    where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _  -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
      in
        case xs of
            [i] -> i
            _   -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    auction :: Auction
    auction = adAuction ad

    tokenValue :: Value
    tokenValue = Value.singleton (aCurrency auction) (aToken auction) 1

    correctInputValue :: Bool
    correctInputValue = inVal == case adHighestBid ad of
        Nothing      -> tokenValue <> Ada.lovelaceValueOf minLovelace
        Just Bid{..} -> tokenValue <> Ada.lovelaceValueOf (minLovelace + bBid)

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid ad

    ownOutput   :: TxOut
    outputDatum :: AuctionDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b = (adAuction outputDatum == auction)   &&
                              (adHighestBid outputDatum == Just b)

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
        txOutValue ownOutput == tokenValue <> Ada.lovelaceValueOf (minLovelace + amount)

    correctBidRefund :: Bool
    correctBidRefund = case adHighestBid ad of
        Nothing      -> True
        Just Bid{..} ->
          let
            os = [ o
                 | o <- txInfoOutputs info
                 , txOutAddress o == pubKeyHashAddress bBidder Nothing
                 ]
          in
            case os of
                [o] -> txOutValue o == Ada.lovelaceValueOf bBid
                _   -> traceError "expected exactly one refund output"

    correctBidSlotRange :: Bool
    correctBidSlotRange = to (aDeadline auction) `contains` txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = from (aDeadline auction) `contains` txInfoValidRange info

    getsValue :: PaymentPubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h Nothing

typedAuctionValidator :: Scripts.TypedValidator Auctioning 
typedAuctionValidator = Scripts.mkTypedValidator @Auctioning 
    $$(PlutusTx.compile [|| mkAuctionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where 
      wrap = Scripts.wrapValidator @AuctionDatum @AuctionAction 
    
auctionValidator :: Validator 
auctionValidator = Scripts.validatorScript typedAuctionValidator 

auctionHash :: Ledger.ValidatorHash 
auctionHash = Scripts.validatorHash typedAuctionValidator 

auctionAddress :: Ledger.Address 
auctionAddress = scriptHashAddress auctionHash 

---------------------- off chain code begins ---------------------------

----------------------ADTs start
data StartParams = StartParams 
    { spDeadline :: !POSIXTime 
    , spMinBid   :: !Integer 
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName 
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data BidParams = BidParams 
    { bpCurrency :: !CurrencySymbol
    , bpToken    :: !TokenName 
    , bpBid      :: !Integer 
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data CloseParams = CloseParams 
    { cpCurrency :: !CurrencySymbol
    , cpToken    :: !TokenName 
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type AuctionSchema =
        Endpoint "start" StartParams
    .\/ Endpoint "bid"   BidParams
    .\/ Endpoint "close" CloseParams

-------------------- ADTs end ----------------------------------

---------------------- bid start logic -----------------------------
start :: AsContractError e => StartParams -> Contract w s e ()
start StartParams{..} = do
    pkh <- ownPaymentPubKeyHash 
    let a = Auction
            { aSeller   = pkh 
            , aDeadline = spDeadline
            , aMinBid   = spMinBid
            , aCurrency = spCurrency
            , aToken    = spToken
            }
        d = AuctionDatum 
        -- utxo used to start auction; contains an NFT (within the Auction type) and no bider
            { adAuction    = a 
            , adHighestBid = Nothing 
            }
        -- creates of type Value to contain tokens; from the provided CurrencySymbol and TokenName as input
        -- together with a minmum amount of Ada.
        v = Value.singleton spCurrency spToken 1 <> Ada.lovelaceValueOf minLovelace
        tx = Constraints.mustPayToTheScript d v 
    ledgerTx <- submitTxConstraints typedAuctionValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "started auction %s for token %s" (P.show a) (P.show v)

------ start logic end --------------------


    