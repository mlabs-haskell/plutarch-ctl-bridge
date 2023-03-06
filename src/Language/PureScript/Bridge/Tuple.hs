{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Bridge.Tuple (tupleBridge) where

import Data.Text qualified as Text

import Language.PureScript.Bridge.Builder (BridgePart, doCheck)
import Language.PureScript.Bridge.PSTypes (psTuple)
import Language.PureScript.Bridge.TypeInfo (HaskellType, haskType, _typeName)

tupleBridge :: BridgePart
tupleBridge = doCheck haskType isTuple >> psTuple

data TupleParserState
  = Start
  | OpenFound
  | ColonFound
  | Tuple
  | NoTuple
  deriving stock (Eq, Show)

step :: TupleParserState -> Char -> TupleParserState
step Start '(' = OpenFound
step Start _ = NoTuple
step OpenFound ',' = ColonFound
step OpenFound _ = NoTuple
step ColonFound ',' = ColonFound
step ColonFound ')' = Tuple
step ColonFound _ = NoTuple
step Tuple _ = NoTuple
step NoTuple _ = NoTuple

isTuple :: HaskellType -> Bool
isTuple = (== Tuple) . Text.foldl' step Start . _typeName
