module Main (main) where

import Plutarch.Prelude

import Data.Proxy (Proxy (Proxy))
import Generics.SOP qualified as SOP
import Language.PureScript.Bridge (
  Language (Haskell),
  SumType,
  buildBridge,
  defaultBridge,
  mkSumType,
  writePSTypes,
 )
import Language.PureScript.Bridge.Script (SScriptType (SMinting), writeScriptModule)
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.Api.V2 (PMintingPolicy)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (DerivePConstantViaDataList),
  PlutusTypeDataList,
  ProductIsData (ProductIsData),
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (type PLifted))
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusTx qualified

data Foo = Foo
  { foo :: Integer
  , bar :: CurrencySymbol
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusTx.ToData, PlutusTx.FromData) via (ProductIsData Foo)

newtype PFoo (s :: S)
  = PFoo
      ( Term
          s
          ( PDataRecord
              '[ "foo" ':= PInteger
               , "bar" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow, PDataFields)

deriving via
  (DerivePConstantViaDataList Foo PFoo)
  instance
    (PConstantDecl Foo)

instance DerivePlutusType PFoo where
  type DPTStrat _ = PlutusTypeDataList

instance PUnsafeLiftDecl PFoo where
  type PLifted _ = Foo

instance PTryFrom PData (PAsData PFoo)

examplePolicy :: ClosedTerm (PAsData PFoo :--> PMintingPolicy)
examplePolicy = plam $ \_ _ _ -> popaque (pconstant ())

typesToBridge :: [SumType 'Haskell]
typesToBridge =
  [ mkSumType (Proxy @Foo)
  ]

main :: IO ()
main = do
  writePSTypes "./out" (buildBridge defaultBridge) typesToBridge
  writeScriptModule "./out/Example/Scripts" "ExamplePolicy" "examplePolicy" SMinting examplePolicy
