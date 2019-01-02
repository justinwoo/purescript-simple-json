# Quickstart

## Decoding / Reading JSON

Simple-JSON can be used to easily decode from types that have JSON representations, such as numbers, booleans, strings, arrays, and records.

Let's look at an example using a record alias:

```hs
type MyRecordAlias =
  { apple :: String
  , banana :: Array Int
  }
```

Now we can try decoding some JSON:

```hs
import Simple.JSON as JSON

testJSON1 :: String
testJSON1 = """
{ "apple": "Hello"
, "banana": [ 1, 2, 3 ]
}
"""

main = do
  case JSON.readJSON testJSON1 of
    Right (r :: MyRecordAlias) -> do
      assertEqual { expected: r.apple, actual: "Hello"}
      assertEqual { expected: r.banana, actual: [ 1, 2, 3 ] }
    Left e -> do
      assertEqual { expected: "failed", actual: show e }
```

Since `JSON.readJSON` returns `Either MultipleErrors a`, we need to provide the compiler information on what type the `a` should be. We accomplish this by establishing a concrete type for `a` with the type annotation `r :: MyRecordAlias`, so the return type is now `Either MultipleErrors MyRecordAlias`, which is the same as `Either MultipleErrors  { apple :: String, banana :: Array Int }`.

And that's it!

## Encoding / Writing JSON

Encoding JSON is a failure-proof operation, since we know what we want to encode at compile time.

```hs
main = do
  let
    myValue =
      { apple: "Hi"
      , banana: [ 1, 2, 3 ]
      } :: MyRecordAlias

  log (JSON.writeJSON myValue) -- {"banana":[1,2,3],"apple":"Hi"}
```

And that's all we need to do to encode JSON!

## Working with Optional values

For most cases, the instance for `Maybe` will do what you want by decoding `undefined` and `null` to `Nothing` and writing `undefined` from `Nothing` (meaning that the JSON output will not contain the field).

```hs
type WithMaybe =
  { cherry :: Maybe Boolean
  }

testJSON3 :: String
testJSON3 = """
{ "cherry": true
}
"""

testJSON4 :: String
testJSON4 = """
{}
"""
```

```hs
main = do
  case JSON.readJSON testJSON3 of
    Right (r :: WithMaybe) -> do
      assertEqual { expected: Just true, actual: r.cherry }
    Left e -> do
      assertEqual { expected: "failed", actual: show e }

  case JSON.readJSON testJSON4 of
    Right (r :: WithMaybe) -> do
      assertEqual { expected: Nothing, actual: r.cherry }
    Left e -> do
      assertEqual { expected: "failed", actual: show e }

  let
    withJust =
      { cherry: Just true
      } :: WithMaybe
    withNothing =
      { cherry: Nothing
      } :: WithMaybe

  log (JSON.writeJSON withJust) -- {"cherry":true}
  log (JSON.writeJSON withNothing) -- {}
```

If you explicitly need `null` and not `undefined`, use the `Nullable` type.

```hs
main =
  case JSON.readJSON testJSON3 of
    Right (r :: WithNullable) -> do
      assertEqual { expected: toNullable (Just true), actual: r.cherry }
    Left e -> do
      assertEqual { expected: "failed", actual: show e }

  case JSON.readJSON testJSON4 of
    Right (r :: WithNullable) -> do
      assertEqual { expected: "failed", actual: show r }
    Left e -> do
      let errors = Array.fromFoldable e
      assertEqual { expected: [ErrorAtProperty "cherry" (TypeMismatch "Nullable Boolean" "Undefined")], actual: errors }

  let
    withNullable =
      { cherry: toNullable Nothing
      } :: WithNullable
  log (JSON.writeJSON withNullable) -- {"cherry":null}
```
