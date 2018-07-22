# Quickstart

Simple-JSON was designed to be the simplest way possible for PureScript users to safely consume untyped, JSON structured data.

## Reading and Writing Simple Types

Simple-JSON can be used to read and write simple types, such as numbers, ints, strings, booleans, etc.

For example, the snippet below attempts to parse an integer from the string `"1"` and print it to the console:
```hs
import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console
import Simple.JSON as JSON

main :: Effect Unit
main =
  case (JSON.readJSON "1") of
    Right (int :: Int) -> 
      log ("Parsed the integer " <> show int)
    Left errs -> 
      log ("Failed to parse the input as an integer: " <> show errs)
```

Note that, because `JSON.readJSON` returns `Either MultipleErrors a`, the `Right` pattern match must be explicitly annotated in this case, since there's no other way to infer that we _actually_ want to parse out an `Int` here.

Writing simple types out as JSON strings is even easier:
```hs
import Prelude
import Effect (Effect)
import Effect.Console
import Simple.JSON as JSON

main :: Effect Unit
main =
  let stringifiedInt = JSON.writeJSON (1 :: Int)
  in log ("Writing the integer: " <> stringifiedInt)
```

Reading and writing non-integer simple types is just as easy, just keep in mind that there might not always be enough information available for the compiler to infer _precisely_ what you want to read and write.

When in doubt, annotate the arguments and results of functions as appropriate!

## Handling Optionality

Simple-JSON provides functions that can deal with the two types of optionality one might encounter when dealing with JSON data: `null` and `undefined`.

To disambiguate between these two cases, Simple-JSON relies on the the `Nullable` newtype to represent fields that might be `null`, and uses the standard `Maybe` type tp represent fields that might be `undefined`.

```hs
import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Console
import Simple.JSON as JSON

main :: Effect Unit
main = do
  let nullInt      :: JSON.E (Nullable Int) = JSON.readJSON "null"
  let undefinedInt :: JSON.E (Maybe Int)    = JSON.readJSON "undefined"
  let actualInt    :: JSON.E (Nullable Int) = JSON.readJSON "1"
  
  printOptionalInt "Parsed a 'null'"       (toMaybe nullInt)
  printOptionalInt "Parsed an 'undefined'" undefinedInt
  printOptionalInt "Parsed a 'null'"       (toMaybe actualInt)
  
  where
    printOptionalInt :: String -> Maybe Int -> Effect Unit
    printOptionalInt msg = case _ of
      Right maybeInt -> case maybeInt of
        Nothing -> msg
        Just int -> log ("Parsed the integer " <> show int)
      Left errs -> 
        log ("Failed to parse the input as an integer: " <> show errs)
```

Like before, we had to supply explicit type annotations to ensure that the compiler can properly parse out these fields correctly. 

In the case where we want to successfully parse out `null` values, the result needs to be clearly annotated as a `Nullable` type when using `readJSON`. After that, the `toMaybe` function from `Data.Nullable` can unwrap it so it can be handled like any other `Maybe` value.

It's important to understand ahead of time which type of optional types you'll be expecting. Any time that Simple-JSON parses out an `undefined` field when it expects a `null` (or vice versa), the parser will fail and return `MultipleErrors`.

Writing is, as before, quite a bit simpler:
```hs
import Prelude
import Effect (Effect)
import Effect.Console
import Simple.JSON as JSON

main :: Effect Unit
main = do
  let nullInt      :: Nullable Int = toNullable Nothing
  let undefinedInt :: Maybe Int    = Nothing
  let actualInt    :: Nullable Int = toNullable (Just 1)

  log ("Writing out a 'null' integer: "      <> (JSON.writeJSON nullInt))
  log ("Writing out an 'undefined integer: " <> (JSON.writeJSON undefinedInt))
  log ("Writing out an actual ineger: "      <> (JSON.writeJSON actualInt))
```

Again, type annotation is very important to make everything work, even though the `null` and `undefined` values will have these types erased when written out as strings.

## Dealing with Arrays and PureScript Objects

## Reading and Writing PureScript Records
