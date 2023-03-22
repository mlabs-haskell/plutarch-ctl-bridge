{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | PureScript types to be used for bridges, e.g. in "Language.PureScript.Bridge.Primitives".
module Language.PureScript.Bridge.PSTypes (
  psArray,
  psBool,
  psEither,
  psObject,
  psInt,
  psInteger,
  psNumber,
  psMaybe,
  psString,
  psTuple,
  psUnit,
) where

import Control.Lens (views)
import Control.Monad.Reader.Class (MonadReader)
import Data.Text qualified as Text

import Language.PureScript.Bridge.Builder (BridgeData, psTypeParameters)
import Language.PureScript.Bridge.TypeInfo (
  PSType,
  TypeInfo (TypeInfo, _typeModule, _typeName, _typePackage, _typeParameters),
  haskType,
  typeParameters,
 )

-- | Uses  type parameters from 'haskType' (bridged).
psArray :: MonadReader BridgeData m => m PSType
psArray = TypeInfo "" "Prim" "Array" <$> psTypeParameters

psBool :: PSType
psBool =
  TypeInfo
    { _typePackage = ""
    , _typeModule = "Prim"
    , _typeName = "Boolean"
    , _typeParameters = []
    }

-- | Uses  type parameters from 'haskType' (bridged).
psEither :: MonadReader BridgeData m => m PSType
psEither = TypeInfo "purescript-either" "Data.Either" "Either" <$> psTypeParameters

psObject :: MonadReader BridgeData m => m PSType
psObject = do
  valueTypes <- tail <$> psTypeParameters
  return $ TypeInfo "purescript-foreign-object" "Foreign.Object" "Object" valueTypes

psInt :: PSType
psInt =
  TypeInfo
    { _typePackage = ""
    , _typeModule = "Prim"
    , _typeName = "Int"
    , _typeParameters = []
    }

psInteger :: PSType
psInteger =
  TypeInfo
    { _typePackage = "bigints"
    , _typeModule = "Data.BigInt"
    , _typeName = "BigInt"
    , _typeParameters = []
    }

psNumber :: PSType
psNumber =
  TypeInfo
    { _typePackage = ""
    , _typeModule = "Prim"
    , _typeName = "Number"
    , _typeParameters = []
    }

-- | Uses  type parameters from 'haskType' (bridged).
psMaybe :: MonadReader BridgeData m => m PSType
psMaybe = TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" <$> psTypeParameters

psString :: PSType
psString =
  TypeInfo
    { _typePackage = ""
    , _typeModule = "Prim"
    , _typeName = "String"
    , _typeParameters = []
    }

-- | Uses  type parameters from 'haskType' (bridged).
psTuple :: MonadReader BridgeData m => m PSType
psTuple = do
  size <- views (haskType . typeParameters) length
  let tupleModule = if size == 2 then "Data.Tuple" else "Data.Tuple.Nested"
      tupleName = "Tuple" <> if size == 2 then "" else Text.pack (show size)
  TypeInfo "purescript-tuples" tupleModule tupleName <$> psTypeParameters

psUnit :: PSType
psUnit =
  TypeInfo
    { _typePackage = "purescript-prelude"
    , _typeModule = "Prelude"
    , _typeName = "Unit"
    , _typeParameters = []
    }
