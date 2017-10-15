module BananaKick where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Foreign (MultipleErrors, toForeign)
import Data.List (List, zipWith, (:))
import Data.Monoid (mempty)
import Data.Record (get)
import SQLite3 (DBConnection, DBEffects, queryDB)
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeJSON)
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
import Type.Row (Cons, Nil, kind RowList)

newtype TableName = TableName String

data DateTimeParameter = DateTimeParameter
instance writeForeignDateTimeParameter :: WriteForeign DateTimeParameter where
  writeImpl _ = toForeign "datetime()"

newtype Parameter a = Parameter a

selectQuery :: forall e cols colsRL f
   . RowToList cols colsRL
  => Keys colsRL
  => ReadForeign (Record cols)
  => DBConnection
  -> TableName
  -> f cols
  -> Aff (db :: DBEffects | e) (Either MultipleErrors (Record cols))
selectQuery db tableName proxy =
    runExcept <<< read <$> queryDB db (prepareSelectQuery tableName proxy) []

prepareSelectQuery :: forall e cols colsRL f
   . RowToList cols colsRL
  => Keys colsRL
  => ReadForeign (Record cols)
  => TableName
  -> f cols
  -> String
prepareSelectQuery (TableName table) _ =
    "SELECT " <> cols <> " FROM " <> table <>  ";"
  where
    cols = intercalate ", " $ keys (RLProxy :: RLProxy colsRL)

insertOrReplaceQuery :: forall e params paramsRL
   . RowToList params paramsRL
  => Keys paramsRL
  => Values paramsRL params
  => DBConnection
  -> TableName
  -> Record params
  -> Aff (db :: DBEffects | e) Unit
insertOrReplaceQuery db tableName record =
    void $ queryDB db prep.query prep.vals
  where
    prep = prepareInsertOrReplaceQuery tableName record

prepareInsertOrReplaceQuery :: forall e params paramsRL
   . RowToList params paramsRL
  => Keys paramsRL
  => Values paramsRL params
  => TableName
  -> Record params
  -> {query :: String, vals :: Array String}
prepareInsertOrReplaceQuery (TableName table) rec =
    { query: "INSERT OR REPLACE INTO " <> table <> " " <> keys' <> " VALUES " <> cols <> ";"
    , vals: fromFoldable values'.args
    }
  where
    values' = values (RLProxy :: RLProxy paramsRL) rec 1
    keys' = "(" <> (intercalate ", " $ keys (RLProxy :: RLProxy paramsRL)) <> ")"
    cols = "(" <> (intercalate ", " $ values'.cols) <> ")"

deleteQuery :: forall e params paramsRL
   . RowToList params paramsRL
  => Keys paramsRL
  => Values paramsRL params
  => DBConnection
  -> TableName
  -> Record params
  -> Aff (db :: DBEffects | e) Unit
deleteQuery db tableName record =
    void $ queryDB db prep.query prep.vals
  where
    prep = prepareDeleteQuery tableName record

prepareDeleteQuery :: forall e params paramsRL
   . RowToList params paramsRL
  => Keys paramsRL
  => Values paramsRL params
  => TableName
  -> Record params
  -> {query :: String, vals :: Array String}
prepareDeleteQuery (TableName table) rec =
    { query: "DELETE FROM " <> table <> " WHERE " <> conds <> ";"
    , vals: fromFoldable values'.args
    }
  where
    values' = values (RLProxy :: RLProxy paramsRL) rec 1
    keys' = keys (RLProxy :: RLProxy paramsRL)
    conds = intercalate " AND " $ zipWith mkCond keys' values'.cols
    mkCond key col = key <> " = " <> col

class Keys (rl :: RowList) where
  keys :: RLProxy rl -> List String

instance nilKeys :: Keys Nil where
  keys _ = mempty

instance consKeys ::
  ( Keys tail
  , IsSymbol name
  ) => Keys (Cons name trash tail) where
  keys _ = head : tail
    where
      head = reflectSymbol (SProxy :: SProxy name)
      tail = keys (RLProxy :: RLProxy tail)

class Values (rl :: RowList) (row :: # Type) | rl -> row where
  -- int: number for counting up params if inserted
  -- cols: strings used as columns in the query
  -- args: strings used as array args for parameters in the query
  values :: RLProxy rl -> Record row -> Int -> {cols :: List String, args :: List String}

instance nilValues :: Values Nil row where
  values _ _ _ =
    { cols: mempty
    , args: mempty
    }

instance consValuesDateTimeParameter ::
  ( Values tail row
  ) => Values (Cons name DateTimeParameter tail) row where
  values _ rec i =
      tail
        { cols = "datetime()" : tail.cols
        }
    where
      tail = values (RLProxy :: RLProxy tail) rec i

instance consValuesParameter ::
  ( Values tail row
  , RowCons name (Parameter ty) trash row
  , IsSymbol name
  , WriteForeign ty
  ) => Values (Cons name (Parameter ty) tail) row where
  values _ rec i =
      { cols: ("$" <> show i) : tail.cols
      , args: (writeJSON param) : tail.args
      }
    where
      tail = values (RLProxy :: RLProxy tail) rec (i + 1)
      param
        | Parameter x <- get (SProxy :: SProxy name) rec
        = x :: ty
