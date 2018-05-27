module Test.Util where

import Prelude

import Record (get)
import Type.Prelude (class IsSymbol, SProxy(..))
import Type.Row (RLProxy(..))
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.Row as Row

-- | Check two records of the same type for equality.
equal
  :: forall r rs
   . RowToList r rs
  => EqualFields rs r
  => Record r
  -> Record r
  -> Boolean
equal a b = equalFields (RLProxy :: RLProxy rs) a b

class EqualFields (rs :: RowList) (row :: # Type) | rs -> row where
  equalFields :: RLProxy rs -> Record row -> Record row -> Boolean

instance equalFieldsCons
  ::
  ( IsSymbol name
  , Eq ty
  , Row.Cons name ty tailRow row
  , EqualFields tail row
  ) => EqualFields (Cons name ty tail) row where
  equalFields _ a b = get' a == get' b && rest
    where
      get' = get (SProxy :: SProxy name)
      rest = equalFields (RLProxy :: RLProxy tail) a b

instance equalFieldsNil :: EqualFields Nil row where
  equalFields _ _ _ = true
