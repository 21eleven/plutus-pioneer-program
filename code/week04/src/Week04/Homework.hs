{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Functor           (void)
import           Data.Text              (Text, unpack)
import           GHC.Generics           (Generic)
import           Ledger
import           Ledger.Ada             as Ada
import           Ledger.Constraints     as Constraints
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

-- Helper function to convert ada to lovelace
ada :: Integer -> Integer
ada amt = amt * 1_000_000

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    handleError (\err -> logError $ "Hit Error: " ++ unpack err) $ void $ submitTx tx
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace amt1 amt2 = do
    h <- activateContractWallet (knownWallet 1) payContract
    let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
    callEndpoint @"pay" h PayParams
        { ppRecipient=pkh
        , ppLovelace=amt1
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" h PayParams
        { ppRecipient=pkh
        , ppLovelace=amt2
        }
    void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace (ada 10) (ada 20)

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace (ada 1000) (ada 20)

