{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.PureScript.Bridge.SumType (
  SumType (..),
  mkSumType,
  mkSumTypeWithEncoding,
  DataEncodingFlavor (..),
  defaultDataEncodingFlavor,
  equal,
  order,
  DataConstructor (..),
  RecordEntry (..),
  Instance (..),
  nootype,
  getUsedTypes,
  constructorToTypes,
  sigConstructor,
  sigValues,
  sumTypeInfo,
  sumTypeConstructors,
  recLabel,
  recValue,
) where

import Control.Lens (makeLenses)
import Data.List (nub)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Generics.Deriving (
  C1,
  Constructor,
  D1,
  Datatype,
  Generic,
  K1,
  M1 (M1),
  R,
  Rep,
  S1,
  Selector,
  U1,
  V1,
  conIsRecord,
  conName,
  from,
  selName,
  (:*:),
  (:+:),
 )

import Language.PureScript.Bridge.TypeInfo (
  Language (Haskell),
  TypeInfo,
  flattenTypeInfo,
  mkTypeInfo,
 )

-- | Generic representation of your Haskell types.
data SumType (lang :: Language) = SumType (TypeInfo lang) [DataConstructor lang] [Instance]
  deriving stock (Show, Eq)

-- | TypInfo lens for 'SumType'.
sumTypeInfo :: Functor f => (TypeInfo lang -> f (TypeInfo lang)) -> SumType lang -> f (SumType lang)
sumTypeInfo inj (SumType info constrs is) = (\ti -> SumType ti constrs is) <$> inj info

-- | DataConstructor lens for 'SumType'.
sumTypeConstructors :: Functor f => ([DataConstructor lang] -> f [DataConstructor lang]) -> SumType lang -> f (SumType lang)
sumTypeConstructors inj (SumType info constrs is) = (\cs -> SumType info cs is) <$> inj constrs

{- | Create a representation of your sum (and product) types,
   for doing type translations and writing it out to your PureScript modules.
   In order to get the type information we use a dummy variable of type 'Proxy' (YourType).
-}
mkSumType ::
  forall t.
  (Generic t, Typeable t, GDataConstructor (Rep t)) =>
  Proxy t ->
  SumType 'Haskell
mkSumType p = mkSumTypeWithEncoding encodingFlavor p
  where
    constructors = gToConstructors (from (undefined :: t))
    encodingFlavor = defaultDataEncodingFlavor constructors

{- | Does not guarantee correct PS code, but it's more flexible. If your type follows
| our encoding standards, you should use 'mkSumType' instead.
-}
mkSumTypeWithEncoding ::
  forall t.
  (Generic t, Typeable t, GDataConstructor (Rep t)) =>
  DataEncodingFlavor ->
  Proxy t ->
  SumType 'Haskell
mkSumTypeWithEncoding encodingFlavor p = SumType (mkTypeInfo p) constructors instances
  where
    constructors = gToConstructors (from (undefined :: t))
    plutusInstances = case encodingFlavor of
      DataEncodingSoP -> [HasPlutusSchema]
      DataEncodingProduct -> [HasPlutusSchema, FieldOrder]
      _ -> []
    instances = case constructors of
      [] -> []
      _ -> [Generic, Show, ToData encodingFlavor, FromData encodingFlavor] <> plutusInstances <> maybeToList (nootype constructors)

data DataEncodingFlavor
  = DataEncodingSoP
  | DataEncodingProduct
  | DataEncodingNewtype
  | DataEncodingEnum
  deriving stock (Eq, Show)

-- | Get optimal encoding for a SoP
defaultDataEncodingFlavor :: [DataConstructor lang] -> DataEncodingFlavor
defaultDataEncodingFlavor constructors
  | [DataConstructor _ (Right [_])] <- constructors = DataEncodingNewtype
  | [DataConstructor _ (Left [_])] <- constructors = DataEncodingNewtype
  | [DataConstructor _ (Right (_ : _))] <- constructors = DataEncodingProduct
  | isEnum constructors = DataEncodingEnum
  | otherwise = DataEncodingSoP
  where
    isEnum :: [DataConstructor lang] -> Bool
    isEnum = all isEnumConstr

    isEnumConstr :: DataConstructor lang -> Bool
    isEnumConstr constr = case _sigValues constr of
      Left [] -> True
      _ -> False

-- | Purescript typeclass instances that can be generated for your Haskell types.
data Instance
  = Generic
  | Newtype
  | Eq
  | Ord
  | Show
  | ToData DataEncodingFlavor
  | FromData DataEncodingFlavor
  | HasPlutusSchema
  | FieldOrder
  deriving stock (Eq, Show)

{- | The Purescript typeclass `Newtype` might be derivable if the original
 Haskell type was a simple type wrapper.
-}
nootype :: [DataConstructor lang] -> Maybe Instance
nootype cs = case cs of
  [constr]
    | either isSingletonList (const True) (_sigValues constr) -> Just Newtype
    | otherwise -> Nothing
  _ -> Nothing
  where
    isSingletonList [_] = True
    isSingletonList _ = False

-- | Ensure that an `Eq` instance is generated for your type.
equal :: Eq a => Proxy a -> SumType t -> SumType t
equal _ (SumType ti dc is) = SumType ti dc . nub $ Eq : is

-- | Ensure that both `Eq` and `Ord` instances are generated for your type.
order :: Ord a => Proxy a -> SumType t -> SumType t
order _ (SumType ti dc is) = SumType ti dc . nub $ Eq : Ord : is

data DataConstructor (lang :: Language) = DataConstructor
  { _sigConstructor :: !Text
  -- ^ e.g. `Left`/`Right` for `Either`
  , _sigValues :: !(Either [TypeInfo lang] [RecordEntry lang])
  }
  deriving stock (Show, Eq)

data RecordEntry (lang :: Language) = RecordEntry
  { _recLabel :: !Text
  -- ^ e.g. `runState` for `State`
  , _recValue :: !(TypeInfo lang)
  }
  deriving stock (Show, Eq)

class GDataConstructor f where
  gToConstructors :: f a -> [DataConstructor 'Haskell]

class GRecordEntry f where
  gToRecordEntries :: f a -> [RecordEntry 'Haskell]

instance (Datatype a, GDataConstructor c) => GDataConstructor (D1 a c) where
  gToConstructors (M1 c) = gToConstructors c

instance (GDataConstructor a, GDataConstructor b) => GDataConstructor (a :+: b) where
  gToConstructors (_ :: (a :+: b) f) =
    gToConstructors (undefined :: a f)
      ++ gToConstructors (undefined :: b f)

instance (Constructor a, GRecordEntry b) => GDataConstructor (C1 a b) where
  gToConstructors c@(M1 r) =
    [ DataConstructor
        { _sigConstructor = constructor
        , _sigValues = values
        }
    ]
    where
      constructor = Text.pack $ conName c
      values =
        if conIsRecord c
          then Right $ gToRecordEntries r
          else Left $ map _recValue $ gToRecordEntries r

instance (GRecordEntry a, GRecordEntry b) => GRecordEntry (a :*: b) where
  gToRecordEntries (_ :: (a :*: b) f) =
    gToRecordEntries (undefined :: a f)
      ++ gToRecordEntries (undefined :: b f)

instance GRecordEntry U1 where
  gToRecordEntries _ = []

instance (Selector a, Typeable t) => GRecordEntry (S1 a (K1 R t)) where
  gToRecordEntries e =
    [ RecordEntry
        { _recLabel = Text.pack (selName e)
        , _recValue = mkTypeInfo (Proxy :: Proxy t)
        }
    ]

instance GDataConstructor V1 where
  gToConstructors _ = []

{- | Get all used types in a sum type.

   This includes all types found at the right hand side of a sum type
   definition, not the type parameters of the sum type itself
-}
getUsedTypes :: SumType lang -> Set (TypeInfo lang)
getUsedTypes (SumType _ cs _) = foldr constructorToTypes Set.empty cs

constructorToTypes :: DataConstructor lang -> Set (TypeInfo lang) -> Set (TypeInfo lang)
constructorToTypes (DataConstructor _ (Left myTs)) ts =
  Set.fromList (concatMap flattenTypeInfo myTs) `Set.union` ts
constructorToTypes (DataConstructor _ (Right rs)) ts =
  Set.fromList (concatMap (flattenTypeInfo . _recValue) rs) `Set.union` ts

-- Lenses:
makeLenses ''DataConstructor
makeLenses ''RecordEntry
