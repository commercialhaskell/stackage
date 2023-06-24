# Haskell PostgreSQL-typed

A Haskell PostgreSQL interface that provides type-safety through compile-time (template Haskell) database access.
See the [Haddock](http://hackage.haskell.org/package/postgresql-typed) documentation in [Database.PostgreSQL.Typed](http://hackage.haskell.org/package/postgresql-typed/docs/Database-PostgreSQL-Typed.html) or the [test cases](test/Main.hs) for simple examples.

## Getting started

### Installation

Use your preferred package manager to install or add to your package dependencies:

- `stack install postgresql-typed` or
- `cabal install postgresql-typed`

You'll also likely need to add `network` as a dependency.

### Enable ghc extensions

Make sure you enable `TemplateHaskell`, `QuasiQuotes`, and `DataKinds` language extensions, either in your cabal `default-extensions` or in a `{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}` pragma in your source.

### Setup compile-time database connection

Either set the following environment variables:

- `TPG_DB` the database name to use (default: same as user)
- `TPG_USER` the username to connect as (default: `$USER` or `postgres`)
- `TPG_PASS` the password to use (default: *empty*)
- `TPG_HOST` the host to connect to (default: `localhost`)
- `TPG_PORT` or `TPG_SOCK` the port number or local socket path to connect on (default port: `5432`)

*Or* in your code call `Database.PostgreSQL.Typed.useTPGDatabase` with a database config as a top-level quote in each code file where you have SQL queries.
It's often helpful to make your own utility function to do this:

```haskell
-- |Call this at top-level at the beginning of every file (rather than 'useTPGDatabase')
useMyTPGConfig :: Language.Haskell.TH.DecsQ
useMyTPGConfig = useTPGDatabase PGDatabase{ ... } -- or load config from file
```

### Setup your database schema

Your tables and other schema need to be created in your development (compile-time) database before you compile your code.
No queries will actually be executed, so there does not need to be any data, but it will do query parsing with the database (prepare queries) so any referenced objects must exist.

### Setup run-time database connection

Use `pgConnect` to connect to your database using a `PGDatabase` configuration.
The run-time database does not need to be the same as the build-time database (though it can be), but it *must* have the same schema.
It's recommended to use `bracket (pgConnect PGDatabase{..}) pgDisconnect`.
If you need a pool of connections, consider `resource-pool` (while `PGConnection`s are mostly thread-safe, they can't be used for multiple queries simultaneously).

### Complete example

schema.sql:
```sql
CREATE TABLE thing (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
```

DBConfig.hs:
```haskell
{-# LANGUAGE OverloadedStrings #-}
module DBConfig where

import qualified Database.PostgreSQL.Typed as PG
import           Network.Socket (SockAddr(SockAddrUnix))

myPGDatabase :: PG.PGDatabase
myPGDatabase = PG.defaultPGDatabase
  { PG.pgDBAddr = if tcp then Left ("localhost", "5432") else Right (SockAddrUnix "/run/postgresql/.s.PGSQL.5432")
  , PG.pgDBUser = "user"
  , PG.pgDBName = "db"
  } where tcp = False
```

Main.hs:
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Exception (bracket)
import           Control.Monad (void, unless)
import           Data.Int (Int32)
import           Data.Maybe (listToMaybe)
import qualified Database.PostgreSQL.Typed as PG

import DBConfig

PG.useTPGDatabase myPGDatabase

data Thing = Thing Int32 String
  deriving (Eq)

createThing :: PG.PGConnection -> Thing -> IO ()
createThing pg (Thing tid tname) =
  void $ PG.pgExecute pg [PG.pgSQL|INSERT INTO thing (id, name) VALUES (${tid}, ${tname})|]

lookupThing :: PG.PGConnection -> Int32 -> IO (Maybe Thing)
lookupThing pg tid = fmap (uncurry Thing) . listToMaybe <$>
  PG.pgQuery pg [PG.pgSQL|SELECT id, name FROM thing WHERE id = ${tid}|]

main = bracket (PG.pgConnect myPGDatabase) PG.pgDisconnect $ \pg -> do
  let myt = Thing 1 "cat"
  createThing pg myt
  t <- lookupThing pg 1
  unless (t == Just myt) $ fail "wrong thing!"
```
