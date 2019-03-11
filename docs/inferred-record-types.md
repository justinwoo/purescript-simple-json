# Working with Inferred Record Types

## How records work in PureScript

In PureScript, a `Record` type is parameterized by `# Type`

```hs
data Record :: # Type -> Type
```

As seen on [Pursuit](https://pursuit.purescript.org/builtins/docs/Prim#t:Record), this means that records are an application of row types of `Type`, such that the two definitions are equivalent:

```hs
type Person = { name :: String, age :: Number }

type Person = Record ( name :: String, age :: Number )
```

With this knowledge, we can work with records in a generic way where any operation with the correct row type constraints is valid.

This is unlike other languages where records are often simply product types with selector information. Let's look at some examples of this at work.

## Modifying a field's type

Say that we wanted to read in JSON into this type:

```hs
type RecordWithEither =
  { apple :: Int
  , banana :: Either Int String
  }
```

We know that there's no representation of this `Either Int String` in JavaScript, but it would be convenient to read some value into it. First, let's define a function to read in any `Either`:

```hs
readEitherImpl
  :: forall a b
   . JSON.ReadForeign a
  => JSON.ReadForeign b
  => Foreign
  -> Foreign.F (Either a b)
readEitherImpl f
    = Left <$> JSON.readImpl f
  <|> Right <$> JSON.readImpl f
```

Now we can read in to an `Either` any `a` and `b` that have instances for `ReadForeign`. We can then use this to modify a field in an inferred context:

```hs
readRecordWithEitherJSON :: String -> Either Foreign.MultipleErrors RecordWithEither
readRecordWithEitherJSON s = runExcept do
  inter <- JSON.readJSON' s
  banana <- readEitherImpl inter.banana
  pure $ inter { banana = banana }
```

So what goes on here is that since the result of the function is our `RecordWithEither` with a field of `banana :: Either Int String`, the type is inferred "going backwards", so with the application of our function that is now concretely typed in this context as `readEitherImpl :: Foreign -> Foreign.F (Either Int String)`, the `inter` is read in as `{ apple :: Int, banana :: Foreign }`.

In this case, we used record update syntax to modify our inferred record, but we also could have done this generically using `Record.modify` from the [Record](https://pursuit.purescript.org/packages/purescript-record) library.

## PureScript-Record in a nutshell

Most of PureScript-Record revolves around usages of two row type classes from [Prim.Row](https://justinwoo.github.io/generated-docs-12/generated-docs/Prim.Row.html):

```hs
class Cons
  (label :: Symbol) (a :: Type) (tail :: # Type) (row :: # Type)
  | label a tail -> row, label row -> a tail

class Lacks
  (label :: Symbol) (row :: # Type)
```

`class Cons` is a relation of a field of a given `Symbol` label (think type-level `String`), its value `Type`, a row type `tail`, and a row type `row` which is made of the `tail` and the field put together. This is very much like your normal `List` of `Cons a` and `Nil`, but with the unordered row type structure at the type level (that `(a :: String, b :: Int)` is equivalent to `(b :: Int, a :: String)`).

`class Lacks` is a relation of a given `Symbol` label not existing in any of the fields of `row`.

With this bit of knowledge, we can go ahead and look at the docs of the [Record](https://pursuit.purescript.org/packages/purescript-record) library.

Let's go through a few of these. First, `get`:

```hs
get
  :: forall r r' l a
   . IsSymbol l
  => Cons l a r' r
  => SProxy l
  -> { | r }
  -> a
```

So here right away we can see that the `Cons` constraint is used to declare that the label `l` provided by the `SProxy` argument must exist in the row type `r`, and that there exists a `r'`, a complementary row type, which is `r` but without the field `l, a`. With this, this function is able to get out the value of type `a` at label `l`. This function doesn't know what concrete label is going to be used, but it uses this constraint to ensure that the field exists in the record.

```hs
insert
  :: forall r1 r2 l a
   . IsSymbol l
  => Lacks l r1
  => Cons l a r1 r2
  => SProxy l
  -> a
  -> { | r1 }
  -> { | r2 }
```

With `insert`, we work with the input row type `r1` and the output row type `r2`. The constraints here work that the `r1` row should not contain a field with label `l`, and that the result of adding a field of `l, a` to `r1` yields `r2`.

Now, the most involved example:

```hs
rename
  :: forall prev next ty input inter output
   . IsSymbol prev
  => IsSymbol next
  => Cons prev ty inter input
  => Lacks prev inter
  => Cons next ty inter output
  => Lacks next inter
  => SProxy prev
  -> SProxy next
  -> { | input }
  -> { | output }
```

Because PureScript does not solve multiple constraints simultaneously, we work with three row types here: `input`, `inter` (intermediate), and `output`. This function takes two `Symbol` types: one for the current label of the field and one for the next label. Then the constraints work such that `inter` is `input` without the field `prev, ty` and lacks any additional fields of `prev`, as row types can have duplicate labels as they are not only for records. Then `output` is constructured by adding the field `next, ty` to `inter` and checking that the `inter` does not already contain a field with the label `next`. While this seems complicated at first, slowly reading through the constraints will show that this is a series of piecewise operations instead of being a multiple-constraint system.

## Application of generic Record functions

Say we have a type where we know the JSON will have the wrong name:

```hs
type RecordMisnamedField =
  { cherry :: Int
  }
```

If the JSON we receive has this field but with the name "grape", what should we do?

We can apply the same inferred record type method as above but with `Record.rename`:

```hs
readRecordMisnamedField :: String -> Either Foreign.MultipleErrors RecordMisnamedField
readRecordMisnamedField s = do
  inter <- JSON.readJSON s
  pure $ Record.rename grapeP cherryP inter
  where
    grapeP = SProxy :: SProxy "grape"
    cherryP = SProxy :: SProxy "cherry"
```

So again, by applying a function that renames `grape, Int` to `cherry, Int`, the inferred record type of the `inter` is `{ grape :: Int }` and that is the type used to decode the JSON.

Hopefully this page has shown you how powerful row type based Records are in PureScript and the generic operations they allow.

*You might be interested in reading through [slides](https://speakerdeck.com/justinwoo/easy-json-deserialization-with-simple-json-and-record) for further illustrations of how generic record operations work and how they can be used with Simple-JSON.*
