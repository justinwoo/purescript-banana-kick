module Test.Main where

import Prelude

import BananaKick (DateTimeParameter(DateTimeParameter), Parameter(Parameter), TableName(TableName), prepareDeleteQuery, prepareInsertOrReplaceQuery, prepareSelectQuery)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Type.Prelude (RProxy(..))

-- "SELECT path, created FROM watched;" []

-- "INSERT OR REPLACE INTO watched (path, created) VALUES ($1, datetime());" [unwrap ur.path]

-- "DELETE FROM watched WHERE path = $1;" [unwrap ur.path]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let
    selectQuery =
      prepareSelectQuery
        (TableName "watched")
        (RProxy :: RProxy (path :: String, created :: String))
    insertOrReplaceQuery =
      prepareInsertOrReplaceQuery
        (TableName "watched")
        {path: Parameter "asdf", created: DateTimeParameter}
    deleteQuery =
      prepareDeleteQuery
        (TableName "watched")
        {path: Parameter "asdf"}

  log selectQuery
  log <<< show $ selectQuery == "SELECT created, path FROM watched;"

  log insertOrReplaceQuery.query
  log $ show insertOrReplaceQuery.vals
  log <<< show $ insertOrReplaceQuery.query == "INSERT OR REPLACE INTO watched (created, path) VALUES (datetime(), $1);"
  log <<< show $ insertOrReplaceQuery.vals == ["\"asdf\""]

  log deleteQuery.query
  log $ show deleteQuery.vals
  log <<< show $ deleteQuery.query == "DELETE FROM watched WHERE path = $1;"
  log <<< show $ deleteQuery.vals == ["\"asdf\""]
