# FAQ

## How do I get instances of ReadForeign/WriteForeign for my newtypes?

See the post about PureScript newtype deriving here: <https://github.com/paf31/24-days-of-purescript-2016/blob/master/4.markdown>

So you can do everything given some definition of a newtype and its instances:

```purs
-- from test/Quickstart.purs

newtype FancyInt = FancyInt Int

derive newtype instance eqFancyInt :: Eq FancyInt
derive newtype instance showFancyInt :: Show FancyInt
derive newtype instance readForeignFancyInt :: JSON.ReadForeign FancyInt
derive newtype instance writeForeignFancyInt :: JSON.WriteForeign FancyInt
```

## Why isn't this library Aeson-compatible?

There are a few factors involved here:

1. I (Justin) don't use Aeson instances.

2. Many Aeson instances revolve around using Sum and Product types (or Haskell Records, which are not structurally similar to PureScript Records).

3. I would rather give you the tools to write your own so that you have instances that match what you are using by having docs/guides like in this page: <https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html>

4. There doesn't seem to be anyone else making a general solution library and publishing it.

## I just want some random encoding for my Sum types!

If you really are sure you don't want to use the existing instances for [Variant](https://pursuit.purescript.org/packages/purescript-variant/5.0.0/docs/Data.Variant#t:Variant) (from [purescript-variant](https://github.com/natefaubion/purescript-variant)), you can use the code from here: <https://github.com/justinwoo/purescript-simple-json-generics>

You might also choose to use this library: <https://github.com/justinwoo/purescript-kishimen>

## How do I handle keys that aren't lower case?

PureScript record labels can be quoted.

```purs
type MyRecord =
  { "Apple" :: String }
  
fn :: MyRecord -> String
fn myRecordValue =
  myRecordValue."Apple"
```
