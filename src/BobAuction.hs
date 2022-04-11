{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

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
    { aBidder :: !PaymentPubKeyHash
    , aBid    :: !Integer 
    } deriving P.Show 

instance Eq Bid where 
    {-# INLINABLE (==) #-}
    b == c = (aBidder b == aBidder c) &&
             (aBid    b == aBid    c)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

-- bidders can either bid or close the auction
-- it does not matter if anyone invokes the close action
-- since the contract only closes an auction after the deadline 
data AuctionAction = MkBid Bid | Close 

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction




