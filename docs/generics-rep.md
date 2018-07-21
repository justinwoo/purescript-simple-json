# Usage with Generics-Rep

## Motivation

If you really want to work with sum types using Simple-JSON, you will have to define instances for your types accordingly. Normally, this would mean that you would have to define a bunch of instances manually. For example,

```hs
data IntOrBoolean
  = Int Int
  | Boolean Boolean

instance readForeign :: JSON.ReadForeign IntOrBoolean where
  readImpl f
      = Int <$> Foreign.readInt f
    <|> Boolean <$> Foreign.readBoolean f
```

But this ends up with us needing to maintain a mountain of error-prone boilerplate, where we might forget to include a constructor or accidentally have duplicate cases. We should be able to work more generically to write how instances should be created once, and then have all of these instances created for us for free.

This is the idea of using datatype generics, which are provided by the [Generics-Rep](https://pursuit.purescript.org/packages/purescript-generics-rep) library in PureScript.

## Generics-Rep in short

Since what makes Generics-Rep work is in the PureScript compiler as a built-in derivation, you can read through its source to get the gist of it: [Link](https://github.com/purescript/purescript-generics-rep/blob/49ba119b315ff782293e6f59625d6b5e87099812/src/Data/Generic/Rep.purs)

So once you've skimmed through that, let's first look at `class Generic`:

```hs
class Generic a rep | a -> rep where
  to :: rep -> a
  from :: a -> rep
```

The functional dependencies here declare that instances of `Generic` are determined by the type given, so only `a` needs to be known to get `rep`. Then we have a method for turning the representation into our type with `to` and our type into a representation with `from`. This means that if we define a function that can produce a `F rep` from decoding `Foreign` in our `JSON.ReadForeign` instances, we can map the `to` function to it to get `F a`. We'll see how that works later.

*If some of this isn't familiar to you, you should read about type classes from some source like [PureScript By Example](https://leanpub.com/purescript/read#leanpub-auto-type-classes)*

Then, let's look at some of the most relevant representation types:

```hs
-- | A representation for types with multiple constructors.
data Sum a b = Inl a | Inr b

-- | A representation for constructors which includes the data constructor name
-- | as a type-level string.
newtype Constructor (name :: Symbol) a = Constructor a

-- | A representation for an argument in a data constructor.
newtype Argument a = Argument a
```

These will be the main types that will need to write instances for when we define a type class to do some generic decoding. These correspond to the following parts of a definition:

```hs
data Things = Apple Int |   Banana String
--            a         Sum b
-- e.g. Sum (Inl a) (Inr b)

data Things = Apple             Int | Banana String
--            Constructor(name) a
-- e.g. Constructor "Apple" a

data Things = Apple Int | Banana String
--                  Argument(a)
-- e.g. Argument Int
```

This diagram probably won't be that useful the first time you read it, but you may find it to be nice to return to.

*You can read more coherent explanations like in the documentation for GHC Generics in [generics-deriving](http://hackage.haskell.org/package/generic-deriving-1.12.1/docs/Generics-Deriving-Base.html)*

## Applying Generics-Rep to decoding untagged JSON values

Let's revisit the `IntOrBoolean` example, but this time by using Generics-Rep.

```hs
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show (genericShow)

data IntOrBoolean2
  = Int2 Int
  | Boolean2 Boolean

-- note the underscore at the end for the `rep` parameter of class Generic
derive instance genericIntOrBoolean2 :: GR.Generic IntOrBoolean2 _

instance showIntOrBoolean2 :: Show IntOrBoolean2 where
  show = genericShow
  -- now we get a Show based on Generic

instance readForeignIntOrBoolean2 :: JSON.ReadForeign IntOrBoolean2 where
  readImpl f = GR.to <$> untaggedSumRep f
  -- as noted above, mapping to so that we go from F rep to F IntOrBoolean

class UntaggedSumRep rep where
  untaggedSumRep :: Foreign -> Foreign.F rep
```

So with our class `UntaggedSumRep`, we have our method `untaggedSumRep` for decoding `Foreign` to `rep`.

Once we have this code, we'll get some errors about missing instances for `Sum`, `Constructor`, and `Argument` as expected.

First, we define our `Sum` instance so we take the alternative of a `Inl` construction and `Inr` construction:

```hs
instance untaggedSumRepSum ::
  ( UntaggedSumRep a
  , UntaggedSumRep b
  ) => UntaggedSumRep (GR.Sum a b) where
  untaggedSumRep f
      = GR.Inl <$> untaggedSumRep f
    <|> GR.Inr <$> untaggedSumRep f
```

And in our instance we have clearly constrained `a` and `b` for having instances of `UntaggedSumRep`, so that we can use `untaggedSumRep` on the members.

Then we define our `Constructor` instance:

```hs
instance untaggedSumRepConstructor ::
  ( UntaggedSumRep a
  ) => UntaggedSumRep (GR.Constructor name a) where
  untaggedSumRep f = GR.Constructor <$> untaggedSumRep f
```

This definition similar to above, but just with our single constructor case.

*This is where you would try reading `f` into a record by doing something like `record :: { tag :: String, value :: Foreign } <- f` in a do block, if you wanted to represent sum types in that way. Sky's the limit!*

Then let's define the argument instance that will call `readImpl` on the `Foreign` value.

```hs
instance untaggedSumRepArgument ::
  ( JSON.ReadForeign a
  ) => UntaggedSumRep (GR.Argument a) where
  untaggedSumRep f = GR.Argument <$> JSON.readImpl f
```

And so at this level, we try to decode the `Foreign` value directly to the type of the argument.

With just these few lines of code, we now have generic decoding for our untagged sum type encoding that we can apply to any sum type where `Generic` is derived and the generic representation contains `Sum`, `Constructor`, and `Argument`. To get started with your own instances, check out the example in [test/Generic.purs](https://github.com/justinwoo/purescript-simple-json/blob/master/test/Generic.purs) in the Simple-JSON repo.

## Working with "Enum" sum types

If you have sum types where all of the constructors are nullary, you may want to work with them as string literals. For example:

```hs
data Fruit
  = Abogado
  | Boat
  | Candy
derive instance genericFruit :: Generic Fruit _
```

Like the above, we should write a function that can work with the generic representation of sum types, so that we can apply this to all enum-like sum types that derive `Generic` and use it like so:

```hs
instance fruitReadForeign :: JSON.ReadForeign Fruit where
  readImpl = enumReadForeign

enumReadForeign :: forall a rep
   . Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> Foreign.F a
enumReadForeign f =
  to <$> enumReadForeignImpl f
```

First, we define our class which is take the rep and return a `Foreign.F rep`:

```hs
class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> Foreign.F rep
```

Then we only need two instance for this class. First, the instance for the `Sum` type to split cases:

```hs
instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (Sum a b) where
  enumReadForeignImpl f
      = Inl <$> enumReadForeignImpl f
    <|> Inr <$> enumReadForeignImpl f
```

Then we need to match on `Constructor`, but only when its second argument is `NoArguments`, as we want only to work with enum sum types.

```hs
instance constructorEnumReadForeign ::
  ( IsSymbol name
  ) => EnumReadForeign (Constructor name NoArguments) where
  enumReadForeignImpl f = do
    s <- JSON.readImpl f
    if s == name
       then pure $ Constructor NoArguments
       else throwError <<< pure <<< Foreign.ForeignError $
            "Enum string " <> s <> " did not match expected string " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)
```

We put a `IsSymbol` constraint on `name` so that can reflect it to a string and check if it is equal to the string that is taken from the foreign value. In the success branch, we construct the `Constructor` value with the `NoArguments` value.

With just this, we can now decode all enum-like sums:

```hs
readFruit :: String -> Either Foreign.MultipleErrors Fruit
readFruit = JSON.readJSON

main = do
  logShow $ readFruit "\"Abogado\""
  logShow $ readFruit "\"Boat\""
  logShow $ readFruit "\"Candy\""
```
