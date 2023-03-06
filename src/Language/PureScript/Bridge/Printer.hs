module Language.PureScript.Bridge.Printer (
  ImportLine (ImportLine),
  printModule,
  sumTypesToModules,
  sumTypesToNeededPackages,
  typeInfoToText,
  typesToImportLines,
  importLineToText,
  importsFromList,
) where

import Control.Lens (view)
import Control.Monad (unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath (joinPath, takeDirectory, (</>))

import Language.PureScript.Bridge.SumType (
  DataConstructor (DataConstructor),
  DataEncodingFlavor (
    DataEncodingEnum,
    DataEncodingNewtype,
    DataEncodingProduct,
    DataEncodingSoP
  ),
  Instance (
    FieldOrder,
    FromData,
    Generic,
    HasPlutusSchema,
    Newtype,
    Show,
    ToData
  ),
  RecordEntry (RecordEntry),
  SumType (SumType),
  getUsedTypes,
  nootype,
  recValue,
  sigConstructor,
  sigValues,
  _recLabel,
  _recValue,
  _sigConstructor,
 )
import Language.PureScript.Bridge.TypeInfo (
  Language (PureScript),
  PSType,
  TypeInfo (_typeModule, _typeName, _typePackage, _typeParameters),
  typeParameters,
 )

data Module (lang :: Language) = PSModule
  { psModuleName :: !Text
  , psImportLines :: !(Map Text ImportLine)
  , psTypes :: ![SumType lang]
  }
  deriving stock (Show)

type PSModule = Module 'PureScript

data ImportLine = ImportLine
  { importModule :: !Text
  , importAlias :: !(Maybe Text)
  , importTypes :: !(Set Text)
  }
  deriving stock (Show)

type Modules = Map Text PSModule
type ImportLines = Map Text ImportLine

printModule :: FilePath -> PSModule -> IO ()
printModule root m = do
  unlessM (doesDirectoryExist mDir) $ createDirectoryIfMissing True mDir
  Text.writeFile mPath . moduleToText $ m
  where
    mFile = (joinPath . map Text.unpack . Text.splitOn "." $ psModuleName m) <> ".purs"
    mPath = root </> mFile
    mDir = takeDirectory mPath

sumTypesToNeededPackages :: [SumType lang] -> Set Text
sumTypesToNeededPackages = Set.unions . map sumTypeToNeededPackages

sumTypeToNeededPackages :: SumType lang -> Set Text
sumTypeToNeededPackages st =
  Set.filter (not . Text.null) . Set.map _typePackage $ getUsedTypes st

moduleToText :: Module 'PureScript -> Text
moduleToText m =
  Text.unlines $
    "-- !!! DO NOT MODIFY !!! File auto generated by plutarch-ctl-bridge! --"
      : "module " <> psModuleName m <> " where\n"
      : map importLineToText allImports
        <> [ ""
           , "import Prelude"
           , ""
           ]
        <> map sumTypeToText (psTypes m)
  where
    otherImports = importsFromList baselineImports
    allImports = Map.elems $ mergeImportLines otherImports (psImportLines m)

baselineImports :: [ImportLine]
baselineImports =
  [ ImportLine "Data.Maybe" Nothing $ Set.fromList ["Maybe(Just, Nothing)"]
  , ImportLine "Data.Newtype" Nothing $ Set.fromList ["class Newtype"]
  , ImportLine "Data.Generic.Rep" Nothing $ Set.fromList ["class Generic"]
  , ImportLine "Contract.PlutusData" Nothing $ Set.fromList ["class FromData", "class ToData", "genericFromData", "genericToData"]
  , ImportLine "Data.BigInt" (Just "BigInt") $ Set.fromList []
  , ImportLine "Ctl.Internal.Types.PlutusData" Nothing $ Set.fromList ["PlutusData(Integer)"]
  , ImportLine "Ctl.Internal.Plutus.Types.DataSchema" Nothing $ Set.fromList ["class HasPlutusSchema", "type (:+)", "type (:=)", "type (@@)", "I", "PNil"]
  , ImportLine "Ctl.Internal.TypeLevel.Nat" Nothing $ Set.fromList ["S", "Z"]
  , -- From agora-purescript-bridge
    ImportLine "Utils.IsData" Nothing $ Set.fromList ["productFromData", "productToData"]
  , ImportLine "Utils.FieldOrder" Nothing $ Set.fromList ["class FieldOrder"]
  , ImportLine "Prim.RowList" Nothing $ Set.fromList ["Cons", "Nil"]
  ]

importLineToText :: ImportLine -> Text
importLineToText = \case
  ImportLine importModule Nothing importTypes ->
    "import " <> importModule <> " (" <> typeList importTypes <> ")"
  ImportLine importModule (Just importAlias) _ ->
    "import " <> importModule <> " as " <> importAlias
  where
    typeList s = Text.intercalate ", " (Set.toList s)

sumTypeToText :: SumType 'PureScript -> Text
sumTypeToText st = sumTypeToTypeDecls st

sumTypeToTypeDecls :: SumType 'PureScript -> Text
sumTypeToTypeDecls (SumType t cs is) =
  Text.unlines $
    dataOrNewtype <> " " <> typeInfoToText True t <> (if null cs then "" else " =")
      : "    " <> Text.intercalate "\n  | " (map (constructorToText 4) cs) <> "\n"
      : instances (SumType t cs is)
  where
    dataOrNewtype = if isJust (nootype cs) then "newtype" else "data"

{- | Given a Purescript type, generate instances for typeclass
 instances it claims to have.
-}
instances :: SumType 'PureScript -> [Text]
instances st@(SumType t cs is) = map go is
  where
    constraintAllTyVars :: Text -> Text
    constraintAllTyVars constraint =
      case view typeParameters t of
        [] -> ""
        tyVars ->
          mconcat
            [ "("
            , Text.intercalate ", " $ map (((constraint <> " ") <>) . typeInfoToText False) $ tyVars
            , ") => "
            ]

    go Show =
      mconcat
        [ "instance show"
        , _typeName t
        , " :: "
        , constraintAllTyVars "Show"
        , "Show "
        , typeInfoToText False t
        , " where\n"
        , showConstrs
        ]
      where
        showConstrs :: Text
        showConstrs = Text.unlines $ map showConstr cs

        showConstr :: DataConstructor lang -> Text
        showConstr c =
          mconcat
            [ "  show "
            , either (showProducts c) (showRecords c) (view sigValues c)
            ]

        showRecords :: DataConstructor lang -> [RecordEntry lang] -> Text
        showRecords c rs =
          mconcat
            [ "("
            , view sigConstructor c
            , " uniqueRecordName) = \""
            , view sigConstructor c
            , " {\" <> "
            , Text.intercalate " <> \",\" <> " $ map showRecord rs
            , " <> \"}\""
            ]

        showRecord :: RecordEntry lang -> Text
        showRecord (RecordEntry fieldName _) =
          mconcat
            [ "\""
            , fieldName
            , ": \" <> \"(\" <> show uniqueRecordName."
            , fieldName
            , " <> \")\""
            ]

        showProducts :: DataConstructor lang -> [TypeInfo lang] -> Text
        showProducts c tis =
          mconcat
            [ "("
            , view sigConstructor c
            , " "
            , Text.unwords $ zipWith (\i _ -> Text.singleton i) ['a' ..] tis
            , ") = \""
            , view sigConstructor c
            , if null tis
                then "\""
                else
                  mconcat
                    [ "(\" <> "
                    , Text.intercalate " <> \" \" <> " $
                        zipWith (\i _ -> "\"(\" <> show " <> Text.singleton i <> " <> \")\"") ['a' ..] tis
                    , " <> \")\""
                    ]
            ]
    go (ToData DataEncodingSoP) =
      mconcat
        [ "instance toData"
        , _typeName t
        , " :: "
        , constraintAllTyVars "ToData"
        , "ToData "
        , typeInfoToText False t
        , " where\n"
        , "  toData = genericToData\n"
        ]
    go (ToData DataEncodingProduct) =
      mconcat
        [ "instance toData"
        , _typeName t
        , " :: "
        , constraintAllTyVars "ToData"
        , "ToData "
        , typeInfoToText False t
        , " where\n"
        , "  toData = productToData\n"
        ]
    go (ToData DataEncodingNewtype) =
      mconcat
        [ "derive newtype instance ToData "
        , typeInfoToText False t
        , "\n"
        ]
    go (ToData DataEncodingEnum) =
      mconcat
        [ "instance toData"
        , _typeName t
        , " :: "
        , constraintAllTyVars "ToData"
        , "ToData "
        , typeInfoToText False t
        , " where\n"
        , enumToData cs 0
        ]
      where
        enumToData :: [DataConstructor lang] -> Int -> Text
        enumToData [] _ = ""
        enumToData (constr : constrs) idx = do
          let thisCondition :: Text =
                mconcat
                  [ "  toData "
                  , _sigConstructor constr
                  , " = Integer $ BigInt.fromInt "
                  , Text.pack (show idx)
                  , "\n"
                  ]
              restOfConditions :: Text = enumToData constrs (idx + 1)
           in thisCondition <> restOfConditions
    go (FromData DataEncodingSoP) =
      mconcat
        [ "instance fromData"
        , _typeName t
        , " :: "
        , constraintAllTyVars "FromData"
        , "FromData "
        , typeInfoToText False t
        , " where\n"
        , "  fromData = genericFromData\n"
        ]
    go (FromData DataEncodingProduct) =
      mconcat
        [ "instance fromData"
        , _typeName t
        , " :: "
        , constraintAllTyVars "FromData"
        , "FromData "
        , typeInfoToText False t
        , " where\n"
        , "  fromData = productFromData\n"
        ]
    go (FromData DataEncodingNewtype) =
      mconcat
        [ "derive newtype instance FromData "
        , typeInfoToText False t
        , "\n"
        ]
    go (FromData DataEncodingEnum) =
      mconcat
        [ "instance fromData"
        , _typeName t
        , " :: "
        , "FromData "
        , typeInfoToText False t
        , " where\n"
        , "  fromData x = case x of"
        , "\n    (Integer y) ->\n"
        , enumFromData cs 0
        , "\n    _ -> Nothing\n"
        ]
      where
        enumFromData :: [DataConstructor lang] -> Int -> Text
        enumFromData [] _ = "      else Nothing"
        enumFromData (constr : constrs) idx = do
          let thisCondition =
                mconcat
                  [ "      "
                  , if idx /= 0 then "else " else ""
                  , "if y == BigInt.fromInt "
                  , Text.pack (show idx)
                  , " then Just "
                  , _sigConstructor constr
                  , "\n"
                  ]
              restofConditions :: Text = enumFromData constrs (idx + 1)
           in thisCondition <> restofConditions
    go HasPlutusSchema = fromMaybe "" hasPlutusSchemaInstance
      where
        hasPlutusSchemaInstance = do
          pure $
            mconcat
              [ "instance HasPlutusSchema "
              , typeInfoToText False t
              , " ( "
              , goContr cs 0
              , " )\n"
              ]

        goContr [] _ = "PNil"
        goContr (constr : rest) idx =
          mconcat
            [ "\""
            , _sigConstructor constr
            , "\" := PNil @@ "
            , intToPeano idx
            , " :+ "
            , goContr rest (idx + 1)
            ]

        intToPeano :: Int -> Text
        intToPeano i
          | i <= 0 = "Z"
          | otherwise =
              mconcat
                [ "(S "
                , intToPeano (i - 1)
                , ")"
                ]
    go FieldOrder = fromMaybe "" hasFieldOrderInstance
      where
        hasFieldOrderInstance = do
          fields <- case cs of
            [DataConstructor _ (Right fs)] -> pure fs
            _ -> Nothing
          pure $
            mconcat
              [ "instance FieldOrder "
              , typeInfoToText False t
              , " "
              , goFields fields
              , "\n"
              ]

        goFields [] = "Nil"
        goFields (f : fs) =
          mconcat
            [ "(Cons \""
            , _recLabel f
            , "\" "
            , typeInfoToText False (_recValue f)
            , " "
            , goFields fs
            , ")"
            ]
    go i = "derive instance " <> Text.toLower c <> _typeName t <> " :: " <> extras i <> c <> " " <> typeInfoToText False t <> postfix i <> "\n"
      where
        c = Text.pack $ show i
        extras Generic
          | stpLength == 0 = mempty
          | stpLength == 1 = genericConstraintsInner <> " => "
          | otherwise = bracketWrap genericConstraintsInner <> " => "
        extras _ = ""
        postfix Newtype = " _"
        postfix Generic = " _"
        postfix _ = ""
        stpLength = length sumTypeParameters
        sumTypeParameters = filter (isTypeParam t) . Set.toList $ getUsedTypes st
        genericConstraintsInner = Text.intercalate ", " $ map genericInstance sumTypeParameters
        bracketWrap x = "(" <> x <> ")"

isTypeParam :: PSType -> PSType -> Bool
isTypeParam t typ = _typeName typ `elem` map _typeName (_typeParameters t)

genericInstance :: PSType -> Text
genericInstance params = "Generic " <> typeInfoToText False params <> " r" <> mergedTypeInfoToText params

constructorToText :: Int -> DataConstructor 'PureScript -> Text
constructorToText _ (DataConstructor n (Left [])) = n
constructorToText _ (DataConstructor n (Left ts)) = n <> " " <> Text.intercalate " " (map (typeInfoToText False) ts)
constructorToText indentation (DataConstructor n (Right rs)) =
  n
    <> " {\n"
    <> spaces (indentation + 2)
    <> Text.intercalate intercalation (map recordEntryToText rs)
    <> "\n"
    <> spaces indentation
    <> "}"
  where
    intercalation = "\n" <> spaces indentation <> "," <> " "

spaces :: Int -> Text
spaces c = Text.replicate c " "

recordEntryToText :: RecordEntry 'PureScript -> Text
recordEntryToText e = _recLabel e <> " :: " <> typeInfoToText True (view recValue e)

typeInfoToText :: Bool -> PSType -> Text
typeInfoToText topLevel t = if needParens then "(" <> inner <> ")" else inner
  where
    inner =
      _typeName t
        <> if pLength > 0
          then " " <> Text.intercalate " " textParameters
          else ""
    params = _typeParameters t
    pLength = length params
    needParens = not topLevel && pLength > 0
    textParameters = map (typeInfoToText False) params

mergedTypeInfoToText :: PSType -> Text
mergedTypeInfoToText t =
  _typeName t <> Text.concat textParameters
  where
    params = _typeParameters t
    textParameters = map mergedTypeInfoToText params

sumTypesToModules :: Modules -> [SumType 'PureScript] -> Modules
sumTypesToModules = foldr sumTypeToModule

sumTypeToModule :: SumType 'PureScript -> Modules -> Modules
sumTypeToModule st@(SumType t _ _) = Map.alter (Just . updateModule) (_typeModule t)
  where
    updateModule Nothing =
      PSModule
        { psModuleName = _typeModule t
        , psImportLines = dropSelf $ typesToImportLines Map.empty (getUsedTypes st)
        , psTypes = [st]
        }
    updateModule (Just m) =
      m
        { psImportLines = dropSelf $ typesToImportLines (psImportLines m) (getUsedTypes st)
        , psTypes = st : psTypes m
        }
    dropSelf = Map.delete (_typeModule t)

typesToImportLines :: ImportLines -> Set PSType -> ImportLines
typesToImportLines = foldr typeToImportLines

typeToImportLines :: PSType -> ImportLines -> ImportLines
typeToImportLines t ls = typesToImportLines (update ls) (Set.fromList (_typeParameters t))
  where
    update =
      if not (Text.null (_typeModule t))
        then Map.alter (Just . updateLine) (_typeModule t)
        else id

    updateLine Nothing = ImportLine (_typeModule t) Nothing (Set.singleton (_typeName t))
    updateLine (Just (ImportLine m alias types)) =
      ImportLine m alias (Set.insert (_typeName t) types)

importsFromList :: [ImportLine] -> Map Text ImportLine
importsFromList ls =
  let pairs = zip (map importModule ls) ls
      merge a b = ImportLine (importModule a) (importAlias a) (importTypes a `Set.union` importTypes b)
   in Map.fromListWith merge pairs

mergeImportLines :: ImportLines -> ImportLines -> ImportLines
mergeImportLines = Map.unionWith mergeLines
  where
    mergeLines a b = ImportLine (importModule a) (importAlias a) (importTypes a `Set.union` importTypes b)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action