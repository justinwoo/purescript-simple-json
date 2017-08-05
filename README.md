# Purescript-Simple-JSON

[![Build Status](https://travis-ci.org/justinwoo/purescript-simple-json.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-simple-json)

A simple Foreign/JSON library based on the Purescript's RowToList feature.

Requires compiler version 0.11.6 or greater.

## Usage

See the [API Docs](https://pursuit.purescript.org/packages/purescript-simple-json/) or the [tests](test/Main.purs) for usage.

## Usage Example

I use this in my [simple-rpc-telegram-bot](https://github.com/justinwoo/simple-rpc-telegram-bot/blob/7ebdce679eba0eb4462d14d3a6e51d1ba245aa6f/src/Main.purs#L50-L72) project to read configs quickly and easily:

```purs
newtype FilePath = FilePath String
derive instance ntFP :: Newtype FilePath _
derive newtype instance rfFP :: ReadForeign FilePath

newtype Token = Token String
derive instance ntT :: Newtype Token _
derive newtype instance rfT :: ReadForeign Token

newtype Id = Id Int
derive instance ntI :: Newtype Id _
derive newtype instance rfI :: ReadForeign Id

type Config =
  { token :: Token
  , torscraperPath :: FilePath
  , master :: Id
  }

getConfig :: IO (F Config)
getConfig = liftAff $ readJSON <$> readTextFile UTF8 "./config.json"
```
