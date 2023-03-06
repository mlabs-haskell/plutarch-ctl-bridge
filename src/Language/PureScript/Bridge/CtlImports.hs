module Language.PureScript.Bridge.CtlImports (ctlBridge) where

import Control.Applicative (asum)
import Data.Text (Text)
import Language.PureScript.Bridge.Builder (BridgePart, psTypeParameters, (^==))
import Language.PureScript.Bridge.TypeInfo (
  PSType,
  TypeInfo (TypeInfo, _typeModule, _typeName, _typePackage, _typeParameters),
  typeModule,
  typeName,
 )

{- | Types that needs to be overriten by CTL ones instead of being bridiged
 from haskell land
-}
ctlBridge :: BridgePart
ctlBridge =
  asum
    [ overrideModuleFor
        "PlutusLedgerApi.V1.Address"
        "Contract.Address"
        [ "Address"
        ]
    , overrideModuleFor
        "PlutusLedgerApi.V1.Credential"
        "Contract.Credential"
        [ "Credential"
        ]
    , overrideModuleFor
        "PlutusLedgerApi.V1.Time"
        "Contract.Time"
        [ "POSIXTime"
        ]
    , overrideModuleFor
        "PlutusLedgerApi.V1.Crypto"
        "Contract.Address"
        [ "PubKeyHash"
        ]
    , overrideModuleFor
        "PlutusLedgerApi.V2.Tx"
        "Contract.PlutusData"
        [ "OutputDatum"
        ]
    , overrideModuleAndTypeName
        "PlutusLedgerApi.V1.Scripts"
        "DatumHash"
        "Contract.PlutusData"
        "DataHash"
    , overrideModuleFor
        "PlutusTx.Ratio"
        "Contract.Numeric.Rational"
        [ "Rational"
        ]
    , overrideModuleAndTypeName
        "PlutusLedgerApi.V1.Tx"
        "TxOutRef"
        "Ctl.Internal.Types.Transaction"
        "TransactionInput"
    , overrideModuleFor
        "PlutusLedgerApi.V1.Value"
        "Contract.Value"
        [ "Value"
        , "TokenName"
        , "CurrencySymbol"
        ]
    , overrideModuleFor
        "PlutusLedgerApi.V1.Scripts"
        "Contract.Scripts"
        [ "ScriptHash"
        ]
    , overrideModuleFor
        "Data.Tagged"
        "Ctl.Extra.Tagged"
        [ "Tagged"
        ]
    , -- Override for 'Rational'. Type synonyms must be expanded by hand
      overrideModuleFor'
        "GHC.Real"
        "Data.Ratio"
        "Ratio"
    ]

-- | Override module and type name
overrideModuleAndTypeName :: Text -> Text -> Text -> Text -> BridgePart
overrideModuleAndTypeName mOld tOld mNew tNew = do
  typeName ^== tOld
  typeModule ^== mOld
  pure $
    TypeInfo
      { _typePackage = "cardano-transaction-library"
      , _typeModule = mNew
      , _typeName = tNew
      , _typeParameters = []
      }

-- | Override module for multiple data types
overrideModuleFor :: Text -> Text -> [Text] -> BridgePart
overrideModuleFor mOld mNew = do
  asum . fmap (overrideModuleForOneWithParams mOld mNew [])

-- | Override parametrized datatype
overrideModuleFor' :: Text -> Text -> Text -> BridgePart
overrideModuleFor' mOld mNew tName = do
  params <- psTypeParameters
  overrideModuleForOneWithParams mOld mNew params tName

-- | Internal use only, generalization of above functions
overrideModuleForOneWithParams :: Text -> Text -> [PSType] -> Text -> BridgePart
overrideModuleForOneWithParams mOld mNew params tName = do
  typeName ^== tName
  typeModule ^== mOld
  pure $
    TypeInfo
      { _typePackage = "cardano-transaction-library"
      , _typeModule = mNew
      , _typeName = tName
      , _typeParameters = params
      }
