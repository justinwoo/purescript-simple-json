# Usage with Affjax

There is an issue that discusses how usage with Affjax goes here: <https://github.com/justinwoo/purescript-simple-json/issues/51>

## Manually

In short, you can use the `string` response format for the request:

```hs
import Prelude
import Affjax (get, printError)
import Affjax.ResponseFormat (string)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Simple.JSON (readJSON)

type MyRecordAlias
  = { userId :: Int }

main :: Effect Unit
main =
  launchAff_
    $ do
        res <- get string "https://jsonplaceholder.typicode.com/todos/1"
        case res of
          Left err -> do
            log $ "GET /api response failed to decode: " <> printError err
          Right response -> do
            case readJSON response.body of
              Right (r :: MyRecordAlias) -> do
                log $ "userID is: " <> show r.userId
              Left e -> do
                log $ "Can't parse JSON. " <> show e
```

## With Simple-Ajax

You can use Dario's library for making requests with Affjax and handling errors with Variant here: <https://github.com/dariooddenino/purescript-simple-ajax>
