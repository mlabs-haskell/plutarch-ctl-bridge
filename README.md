# `plutarch-ctl-bridge`

Project is based on great [purescript-bridge](https://github.com/eskimor/purescript-bridge) library.

## Usage

```haskell
import Data.Proxy (Proxy (Proxy))
import Language.PureScript.Bridge (defaultBridge, writePSTypes)
import Language.PureScript.Bridge.SumType (mkSumType)

typesToBridge =
  [ mkSumType (Proxy @MyTypeFoo)
  , mkSumType (Proxy @MyTypeBar)
  ]

main =
  writePSTypes "/path/to/save/modules" (buildBridge defaultBridge) typesToBridge
```

## Defatult encoding rules

Types bridged with `mkSumType` are serialized by the following rules.

### Newtypes (`DataEncodingNewtype`/`PlutusTypeNewtype`)

If type is a newtype, it'll be encoded as internal type

```haskell
newtype MyNewtype
  = MkMyNewtype Bool -- Encoded in the same way `Bool` is encoded
```

### Enums (`DataEncodingEnum`/`PlutusTypeEnumData`)

If type is a sum of constructors without fields, it'll be encoded as `I <idx>` where `idx` is an index of a constructor starting from 0

```haskell
data MyEnum
  = MyEnumFoo -- Encoded as `I 0`
  | MyEnumBar -- Encoded as `I 1`
```

### Products/Records (`DataEncodingProduct`/`PlutusTypeDataList`)

If type is a record with only one constructor, it'll be encoded as `List [field0, field1, ..., fieldN]`

```haskell
-- Encoded as `List [I field1, B field2]`
data MyProduct = MyProduct
  { field1 :: Integer
  , field2 :: ByteString
  }
```

### General sums (`DataEncodingSoP`/`PlutusTypeData`)

Otherwise, type is a general sum of products, it's encoded as `Constr <idx> [fields]` where `idx` is an index of a constructor starting from 0 (like in enums) and `[fields]` is a list of fields (like in records).

```haskell
data MySop
  = MySopFoo Integer Integer -- Encoded as `Constr 0 [I field0, I field1]`
  | MySopBar ByteString Integer -- Encoded as `Constr 1 [B field0, I field1]`
```

## Override encoding rules

If your types are not serialized using above set of rules, use `mkSumTypeWithEncoding` to bridge it to specify encoding flavor. Types bridged with `mkSumTypeWithEncoding` are NOT guaranteed to be correct PureScript code, so make sure you know what you're doing. If you have control over bridge types, make them follow above rules, to guarantee optimal encoding and bridge correctness.

## Knwon issues

Some PlutusTx types may not be overridden yet to match with CTL equivalents. If you notice some type missing please add it to `Language.PureScript.Bridge.CtlImports` and open a PR.
