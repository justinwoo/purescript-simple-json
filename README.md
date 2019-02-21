# Purescript-Simple-JSON

[![Build Status](https://travis-ci.org/justinwoo/purescript-simple-json.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-simple-json) [![Documentation Status](https://readthedocs.org/projects/purescript-simple-json/badge/?version=latest)](https://purescript-simple-json.readthedocs.io/en/latest)

A simple Foreign/JSON library based on the Purescript's RowToList feature.

## Quickstart

Get going quickly with the Quickstart section of the guide: <https://purescript-simple-json.readthedocs.io/en/latest/quickstart.html>

You may also be interested in this presentation about how Simple-JSON works well with PureScript-Record: <https://speakerdeck.com/justinwoo/easy-json-deserialization-with-simple-json-and-record>. Note that the slides are based on an older version of the library and on PureScript 0.11.6, and it is not necessary to understand these slides to get started.

## Usage

In brief:

```purs
type MyJSON =
  { apple :: String
  , banana :: Int
  , cherry :: Maybe Boolean
  }
  
decodeToMyJSON :: String -> Either (NonEmptyList ForeignError) MyJSON
decodeToMyJSON = SimpleJSON.readJSON
```

See the [API Docs](https://pursuit.purescript.org/packages/purescript-simple-json/) or the [tests](test/Main.purs) for usage.

There is also a guide for how to use this library on [Read the Docs](https://purescript-simple-json.readthedocs.io/en/latest/).

## Warning: `Maybe`

This library will decode `undefined` and `null` as `Nothing` and write `Nothing` as `undefined`. Please use the `Nullable` type if you'd like to read and write `null` instead. Please take caution when using `Maybe` as this default may not be what you want.

## FAQ

### How do I use this with Affjax?

Please see this page in the guide: <https://purescript-simple-json.readthedocs.io/en/latest/with-affjax.html>

### How do I change how some fields of my JSON objects are read?

Please see this page in the guide: <https://purescript-simple-json.readthedocs.io/en/latest/inferred-record-types.html>

### How do I work with `data` Types?

Please see this page in the guide: <https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html>
