# Usage with Affjax

There is an issue that discusses how usage with Affjax goes here: <https://github.com/justinwoo/purescript-simple-json/issues/51>

## Manually

In short, you can use the `string` response format for the request:

```hs
import Prelude

import Affjax (get)
import Affjax.ResponseFormat (ResponseFormatError(..), string)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Simple.JSON (readJSON)

type MyRecordAlias = { userId :: Int }

main = void $ launchAff_ $ do
  res <- get string "https://jsonplaceholder.typicode.com/todos/1"
  case lmap transformError res.body >>= readJSON of
    Right (r :: MyRecordAlias) -> do
      log "all good"
    Left e -> do
      log "all bad"

transformError (ResponseFormatError e _) = singleton e
```

## With Simple-Ajax

You can use Dario's library for making requests with Affjax and handling errors with Variant here: <https://github.com/dariooddenino/purescript-simple-ajax>
