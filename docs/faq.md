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
