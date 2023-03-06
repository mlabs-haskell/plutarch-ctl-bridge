{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Bridge.Primitives (
  boolBridge,
  eitherBridge,
  strMapBridge,
  dummyBridge,
  intBridge,
  integerBridge,
  doubleBridge,
  listBridge,
  maybeBridge,
  stringBridge,
  textBridge,
  unitBridge,
  noContentBridge,
) where

import Control.Monad.Reader.Class (MonadReader)
import Data.Proxy (Proxy (Proxy))
import Language.PureScript.Bridge.Builder (
  BridgeData,
  BridgePart,
  clearPackageFixUp,
  (<|>),
  (^==),
 )
import Language.PureScript.Bridge.PSTypes (
  psArray,
  psBool,
  psEither,
  psInt,
  psInteger,
  psMaybe,
  psNumber,
  psObject,
  psString,
  psUnit,
 )
import Language.PureScript.Bridge.TypeInfo (
  PSType,
  haskType,
  mkTypeInfo,
  typeModule,
  typeName,
 )

boolBridge :: BridgePart
boolBridge = typeName ^== "Bool" >> return psBool

eitherBridge :: BridgePart
eitherBridge = typeName ^== "Either" >> psEither

strMapBridge :: BridgePart
strMapBridge = typeName ^== "Map" >> psObject

-- | Dummy bridge, translates every type with 'clearPackageFixUp'
dummyBridge :: MonadReader BridgeData m => m PSType
dummyBridge = clearPackageFixUp

intBridge :: BridgePart
intBridge = typeName ^== "Int" >> return psInt

integerBridge :: BridgePart
integerBridge = typeName ^== "Integer" >> return psInteger

doubleBridge :: BridgePart
doubleBridge = typeName ^== "Double" >> return psNumber

listBridge :: BridgePart
listBridge = typeName ^== "[]" >> psArray

maybeBridge :: BridgePart
maybeBridge = typeName ^== "Maybe" >> psMaybe

stringBridge :: BridgePart
stringBridge = haskType ^== mkTypeInfo (Proxy :: Proxy String) >> return psString

textBridge :: BridgePart
textBridge = do
  typeName ^== "Text"
  typeModule ^== "Data.Text.Internal" <|> typeModule ^== "Data.Text.Internal.Lazy"
  return psString

unitBridge :: BridgePart
unitBridge = typeName ^== "()" >> return psUnit

noContentBridge :: BridgePart
noContentBridge = typeName ^== "NoContent" >> return psUnit
