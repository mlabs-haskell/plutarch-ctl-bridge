{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.Bridge.Script (
  ExportableScript,
  ScriptType,
  SScriptType (..),
  writeScriptModule,
  generateModule,
) where

import Plutarch.Prelude

import Cardano.Binary qualified as CBOR
import Data.ByteString.Base16 qualified as Base16
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Singletons (fromSing)
import Data.Singletons.TH (singletons)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (Typeable)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:)), TypeError)
import Language.PureScript.Bridge (defaultBridge)
import Language.PureScript.Bridge.Builder (buildBridge)
import Language.PureScript.Bridge.Printer (
  ImportLine (ImportLine),
  importLineToText,
  importsFromList,
  typeInfoToText,
  typesToImportLines,
 )
import Language.PureScript.Bridge.TypeInfo (HaskellType, PSType, mkTypeInfo)
import Plutarch (Config (Config), TracingMode (DetTracing, NoTracing), compile)
import Plutarch.Api.V2 (PMintingPolicy, PStakeValidator, PValidator)
import Plutarch.Lift (PLifted)
import Plutarch.Script (serialiseScript)
import System.FilePath ((</>))

{- ORMOLU_DISABLE -}
$(singletons [d|
  data ScriptType = Minting | Spending | Certifying
  |])
{- ORMOLU_ENABLE -}

deriving stock instance Show ScriptType

type ExportableScript (script :: PType) (scriptType :: ScriptType) =
  ReifyHaskTypes (GetHaskParams script scriptType)

-- | Create a module @<parent-module>.<script-name>@ with embedded Plutarch script
writeScriptModule ::
  forall (script :: PType) (scriptType :: ScriptType).
  ExportableScript script scriptType =>
  -- | Output file
  FilePath ->
  -- | Parent module name
  Text ->
  -- | Script name
  Text ->
  SScriptType scriptType ->
  ClosedTerm script ->
  IO ()
writeScriptModule fp parentModule scriptName scriptType script =
  let
    moduleName = firstToUpper scriptName
    code = generateModule (parentModule <> "." <> moduleName) scriptName script scriptType
   in
    Text.writeFile (fp </> (Text.unpack moduleName <> ".purs")) code

-- | Generate full PureScript module with embedded Plutarch script
generateModule ::
  forall (script :: PType) (scriptType :: ScriptType).
  ExportableScript script scriptType =>
  -- | Module name
  Text ->
  -- | Script name
  Text ->
  -- | Script
  ClosedTerm script ->
  SScriptType scriptType ->
  Text
generateModule moduleName scriptName script scriptType =
  let
    baselineImports =
      importsFromList
        [ ImportLine "Ctl.Internal.Types.ByteArray" Nothing $ Set.fromList ["hexToByteArrayUnsafe"]
        , ImportLine "Ctl.Internal.Types.Scripts" Nothing $ Set.fromList ["PlutusScript (PlutusScript)", "Language (PlutusV2)"]
        , ImportLine "Ctl.Internal.ApplyArgs" Nothing $ Set.fromList ["ApplyArgsError", "applyArgs"]
        , ImportLine "Ctl.Internal.ToData" Nothing $ Set.fromList ["class ToData", "toData"]
        , ImportLine "Data.Tuple.Nested" Nothing $ Set.fromList ["(/\\)"]
        , ImportLine "Data.Either" Nothing $ Set.fromList ["Either"]
        , ImportLine "Contract.Scripts" Nothing $
            Set.fromList
              [ "MintingPolicy (PlutusMintingPolicy)"
              , "Validator (Validator)"
              , "PlutusScriptStakeValidator(PlutusScriptStakeValidator)"
              ]
        , ImportLine "Contract.Prelude" Nothing $ Set.fromList ["($)", "map"]
        ]
    haskTypes = reifyHaskTypes $ Proxy @(GetHaskParams script scriptType)
    psScriptArgumentsTypes = fmap bridgeHaskType haskTypes
    paramsImports =
      map importLineToText
        . Map.elems
        . typesToImportLines baselineImports
        . Set.fromList
        $ psScriptArgumentsTypes
   in
    mconcat
      [ "module " <> moduleName <> " where\n\n"
      , Text.unlines paramsImports <> "\n"
      , exportScriptWith (Config NoTracing) scriptName script <> "\n"
      , exportScriptWith (Config DetTracing) (scriptName <> "Debug") script <> "\n"
      , wrapTypedScript script scriptType psScriptArgumentsTypes scriptName <> "\n"
      , wrapTypedScript script scriptType psScriptArgumentsTypes (scriptName <> "Debug") <> "\n"
      ]

-- | Compile a script with the given config and embed the compiled script
exportScriptWith :: forall (script :: PType). Config -> Text -> ClosedTerm script -> Text
exportScriptWith config scriptName script =
  let
    -- See: https://www.rfc-editor.org/rfc/rfc8949.html#section-appendix.b
    decodeByteString :: Text -> Text
    decodeByteString inp
      | "58" `Text.isPrefixOf` inp = Text.drop (2 + 2) inp
      | "59" `Text.isPrefixOf` inp = Text.drop (2 + 4) inp
      | "5a" `Text.isPrefixOf` inp = Text.drop (2 + 8) inp
      | "5b" `Text.isPrefixOf` inp = Text.drop (2 + 16) inp
      | "5" `Text.isPrefixOf` inp = Text.drop 2 inp
      | "4" `Text.isPrefixOf` inp = Text.drop 2 inp
      | otherwise = error $ "decodeByteString: Invalid CBOR encoding: " <> show inp

    -- Why do we need this roundtrip? I don't know, but it doesn't work without it.
    scriptBytes :: Text
    scriptBytes =
      decodeByteString
        . Base16.encodeBase16
        . CBOR.serialize'
        . serialiseScript
        . either (error . show) id
        $ compile config script
   in
    mconcat
      [ scriptName
      , " :: PlutusScript\n"
      , scriptName
      , " = PlutusScript (hexToByteArrayUnsafe \""
      , scriptBytes
      , "\" /\\ PlutusV2)\n"
      ]

-- | Create typed script wrapper for applying arguments
wrapTypedScript ::
  forall (script :: PType) (scriptType :: ScriptType).
  ExportableScript script scriptType =>
  ClosedTerm script ->
  SScriptType scriptType ->
  [PSType] ->
  Text ->
  Text
wrapTypedScript _ scriptType psTypes scriptName =
  let typesCount = length psTypes
   in mconcat
        [ "apply"
        , firstToUpper scriptName
        , " :: "
        , Text.unwords $ fmap (\t -> typeInfoToText True t <> " ->") psTypes
        , " Either ApplyArgsError "
        , getWrapperType $ fromSing scriptType
        , "\n"
        , "apply"
        , firstToUpper scriptName
        , " "
        , Text.pack $ unwords $ fmap (\idx -> "p" <> show idx) [1 .. typesCount]
        , " = map "
        , getWrapperConstr $ fromSing scriptType
        , " $ applyArgs "
        , scriptName
        , " ["
        , Text.pack $ intercalate ", " $ fmap (\idx -> "toData p" <> show idx) [1 .. typesCount]
        , "]"
        , "\n"
        ]

-- | Convert a Haskell type to a PureScript type
bridgeHaskType :: HaskellType -> PSType
bridgeHaskType = buildBridge defaultBridge

-- | Capitalize the first letter of a 'Text'
firstToUpper :: Text -> Text
firstToUpper t = case Text.uncons t of
  Nothing -> Text.toUpper t
  Just (c, cs) -> Text.cons (toUpper c) cs

-- | Get runtime representation of a Haskell types
class ReifyHaskTypes (ts :: [Type]) where
  reifyHaskTypes :: Proxy ts -> [HaskellType]

instance ReifyHaskTypes '[] where
  reifyHaskTypes _ = []

instance (Typeable t, ReifyHaskTypes ts) => ReifyHaskTypes (t ': ts) where
  reifyHaskTypes _ = (mkTypeInfo (Proxy @t)) : reifyHaskTypes (Proxy @ts)

-- | Get Haskell level parameters of Plutarch script
type family GetHaskParams (ptype :: PType) (scriptType :: ScriptType) where
  GetHaskParams PMintingPolicy 'Minting = '[]
  GetHaskParams PStakeValidator 'Certifying = '[]
  GetHaskParams PValidator 'Spending = '[]
  GetHaskParams (PAsData a :--> b) ty = PLifted a ': GetHaskParams b ty
  GetHaskParams (a :--> _) _ =
    TypeError
      ( 'Text "Unsupported script argument type:"
          ':$$: 'ShowType a
          ':$$: 'Text "Only PAsData types are supported"
      )

-- | Get PureScript constructor to wrap raw script
getWrapperConstr :: ScriptType -> Text
getWrapperConstr Minting = "PlutusMintingPolicy"
getWrapperConstr Spending = "Validator"
getWrapperConstr Certifying = "PlutusScriptStakeValidator"

-- | Get PureScript script wrapper type name
getWrapperType :: ScriptType -> Text
getWrapperType Minting = "MintingPolicy"
getWrapperType Spending = "Validator"
getWrapperType Certifying = "PlutusScriptStakeValidator"
