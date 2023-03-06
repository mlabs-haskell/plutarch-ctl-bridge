module Language.PureScript.Bridge (
  bridgeSumType,
  defaultBridge,
  module Bridge,
  writePSTypes,
) where

import Control.Applicative ((<|>))
import Data.Map qualified as Map
import Data.Text.IO qualified as Text

import Language.PureScript.Bridge.Builder (BridgePart, FullBridge, buildBridge)
import Language.PureScript.Bridge.CtlImports (ctlBridge)
import Language.PureScript.Bridge.Primitives (
  boolBridge,
  doubleBridge,
  dummyBridge,
  eitherBridge,
  intBridge,
  integerBridge,
  listBridge,
  maybeBridge,
  noContentBridge,
  strMapBridge,
  stringBridge,
  textBridge,
  unitBridge,
 )
import Language.PureScript.Bridge.Printer (printModule, sumTypesToModules, sumTypesToNeededPackages)
import Language.PureScript.Bridge.SumType (
  DataConstructor (DataConstructor),
  RecordEntry (RecordEntry),
  SumType (SumType),
  mkSumType,
 )
import Language.PureScript.Bridge.Tuple (tupleBridge)
import Language.PureScript.Bridge.TypeInfo (Language (Haskell, PureScript))

import Language.PureScript.Bridge.Builder qualified as Bridge
import Language.PureScript.Bridge.Primitives qualified as Bridge
import Language.PureScript.Bridge.Printer qualified as Bridge
import Language.PureScript.Bridge.SumType qualified as Bridge
import Language.PureScript.Bridge.Tuple qualified as Bridge
import Language.PureScript.Bridge.TypeInfo qualified as Bridge

{- | Your entry point to this library and quite likely all you will need.
   Make sure all your types derive `Generic`.

   Then list all your types you want to use in PureScript and call 'writePSTypes':

   > data Foo = Foo { ... } deriving (Eq, Generic)
   > data Bar = A | B | C deriving (Eq, Ord, Generic)
   > data Baz = ... deriving (Generic)
   >
   > -- | All types will have a `Generic` instance produced in Purescript.
   > myTypes :: [SumType 'Haskell]
   > myTypes =
   >   [ let p = (Proxy :: Proxy Foo) in equal p (mkSumType p)  -- Also produce a `Eq` instance.
   >   , let p = (Proxy :: Proxy Bar) in order p (mkSumType p)  -- Produce both `Eq` and `Ord`.
   >   , mkSumType (Proxy :: Proxy Baz)  -- Just produce a `Generic` instance.
   >   ]
   >
   >  writePSTypes "path/to/your/purescript/project" (buildBridge defaultBridge) myTypes

   You can define your own type bridges based on 'defaultBridge':


  >  myBridge = defaultBridge <|> mySpecialTypeBridge

  and use it with 'writePSTypes':

  >  writePSTypes "path/to/your/purescript/project" (buildBridge myBridge) myTypes

   Find examples for implementing your own bridges in: "Language.PureScript.Bridge.Primitives".

  == Result:
   'writePSTypes' will write out PureScript modules to the given path, mirroring the hierarchy of the Haskell modules
   the types came from. In addition a list of needed PS packages is printed to the console.

   The list of needed packages is retrieved from the bridged 'TypeInfo' data, so make sure you set '_typePackage' correctly
   in your own bridges, in order for this feature to be useful.

  == Real world usage example (at time of this writing outdated, at time of reading hopefully fixed):
   A real world use case of this library can be found <https://github.com/gonimo/gonimo-back/blob/master/app/PSGenerator.hs here>.

   With custom bridges defined <https://github.com/gonimo/gonimo-back/blob/master/src/Gonimo/CodeGen/TypeBridges.hs here> and
   custom PS types defined <https://github.com/gonimo/gonimo-back/blob/master/src/Gonimo/CodeGen/PSTypes.hs here>.

   Parts of the generated output can be found <https://github.com/gonimo/gonimo-front/blob/master/src/Gonimo/Types.purs here>.

   Note how 'Secret' and 'Key'
   get translated according to our custom rules, with correct imports and everything.
   Also the formatting is quite nice, would you have guessed that this code was generated?

  == /WARNING/:
   This function overwrites files - make backups or use version control!
-}
writePSTypes :: FilePath -> FullBridge -> [SumType 'Haskell] -> IO ()
writePSTypes root bridge sts = do
  mapM_ (printModule root) modules
  Text.putStrLn "The following purescript packages are needed by the generated code:\n"
  mapM_ (Text.putStrLn . mappend "  - ") packages
  Text.putStrLn "\nSuccessfully created your PureScript modules!"
  where
    bridged = map (bridgeSumType bridge) sts
    modules = Map.elems $ sumTypesToModules Map.empty bridged
    packages = sumTypesToNeededPackages bridged

{- | Translate all 'TypeInfo' values in a 'SumType' to PureScript types.

   Example usage, with defaultBridge:

 > data Foo = Foo | Bar Int | FooBar Int Text deriving (Generic, Typeable, Show)

 > bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy Foo))
-}
bridgeSumType :: FullBridge -> SumType 'Haskell -> SumType 'PureScript
bridgeSumType br (SumType t cs is) = SumType (br t) (map (bridgeConstructor br) cs) is

{- | Default bridge for mapping primitive/common types:
   You can append your own bridges like this:

 >  defaultBridge <|> myBridge1 <|> myBridge2

   Find examples for bridge definitions in "Language.PureScript.Bridge.Primitives" and
   "Language.PureScript.Bridge.Tuple".
-}
defaultBridge :: BridgePart
defaultBridge =
  textBridge
    <|> stringBridge
    <|> listBridge
    <|> maybeBridge
    <|> eitherBridge
    <|> strMapBridge
    <|> boolBridge
    <|> intBridge
    <|> integerBridge
    <|> doubleBridge
    <|> tupleBridge
    <|> unitBridge
    <|> ctlBridge
    <|> noContentBridge

-- | Translate types in a constructor.
bridgeConstructor :: FullBridge -> DataConstructor 'Haskell -> DataConstructor 'PureScript
bridgeConstructor br (DataConstructor name (Left infos)) =
  DataConstructor name . Left $ map br infos
bridgeConstructor br (DataConstructor name (Right record)) =
  DataConstructor name . Right $ map (bridgeRecordEntry br) record

-- | Translate types in a record entry.
bridgeRecordEntry :: FullBridge -> RecordEntry 'Haskell -> RecordEntry 'PureScript
bridgeRecordEntry br (RecordEntry label value) = RecordEntry label $ br value
