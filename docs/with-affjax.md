# Usage with Affjax

There is an issue that discusses how usage with Affjax goes here: <https://github.com/justinwoo/purescript-simple-json/issues/51>

## Manually

In short, you can use the String response format for the request:

```purs
-- from https://github.com/justinwoo/purescript-simple-json/issues/51#issuecomment-421457861

-- (your regular code need not be this long)
main = void $ launchAff_ $ do
  res <- AX.request (
    AX.defaultRequest
    {
      url = "https://jsonplaceholder.typicode.com/todos/1",
      method = Left GET,
      responseFormat = ResponseFormat.string -- String ResponseFormat specified here
    }
  )
 let body = bimap transfomError identity res.body
  case res.body >>= JSON.readJSON  of -- then String JSON can be used with readJSON
    Right (r :: MyRecordAlias) -> do
      log "all good"
    Left e -> do
      log "all bad"

transformError (ResponseFormat.ResponseFormatError e _) = cons' e Nil
```

## With Simple-Ajax

You can use Dario's library for making requests with Affjax and handling errors with Variant here: <https://github.com/dariooddenino/purescript-simple-ajax>
