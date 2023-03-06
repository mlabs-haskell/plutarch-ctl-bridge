module Main (main) where

import Plutarch.Prelude

import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged)
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
import Plutarch.Api.V2 (PMintingPolicy)
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import System.Directory (createDirectoryIfMissing)

data Foo = Foo
  { foo :: Tagged "foo" Integer
  , bar :: CurrencySymbol
  , baz :: Tagged 42 CurrencySymbol
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

examplePolicy :: ClosedTerm (PAsData PInteger :--> PMintingPolicy)
examplePolicy = plam $ \_ _ _ -> popaque (pconstant ())

typesToBridge :: [SumType 'Haskell]
typesToBridge =
  [ mkSumType (Proxy @Foo)
  ]

main :: IO ()
main = do
  writePSTypes "./out" (buildBridge defaultBridge) typesToBridge
  createDirectoryIfMissing True "./out/Example/Scripts"
  writeScriptModule "./out/Example/Scripts" "ExamplePolicy" "examplePolicy" SMinting examplePolicy
