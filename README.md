# [configurator-ng](https://github.com/robx/configurator-pg)

## What is this?

This is a simplified version of the resting
[configurator-ng](https://github.com/lpsmith/configurator-ng),
aimed particularly to offer users of configurator-ng such as
[PostgREST](postgrest.org) an easy path to migrate to
a package the compiles with modern GHC versions and that
continues to read existing configuration files.

## Changes

configurator-pg skips some of configurator-ng's features, and
changes the API in other places. In particular, there is not
configuration file reloading. Furthermore

  * No configuration file reloading.
  * Simplified parsing API:
    - There is no type-class based value parsing; you need
      to supply explicit value parsers.
    - There's only `load` to read and evaluate a configuration file,
      and `runParser` to extract configuration values.
  * Simplified handling of configuration subsets. There's `subassocs`
    and the unit tests pass, but the author didn't attempt to
    understand the original implementation fully.

## Credits

The original configurator-ng is due to MailRank, Inc., Bryan
O'Sullivan and Leon P Smith.

The low-level parser (Data.Configurator.Syntax) is mostly unchanged,
evaluation (Data.Configurator.Load) is also close to the original.
The high-level parser (Data.Configurator.Parser) is original.

## File format

In short, the file format supports:

* A simple, but flexible, configuration language, supporting several
  of the most commonly needed types of data, along with
  interpolation of strings from the configuration or the system
  environment (e.g. `$(HOME)`).

* An `import` directive allows the configuration of a complex
  application to be split across several smaller files, or common
  configuration data to be shared across several applications.

The format is more fully documented in the packages
[configurator](https://hackage.haskell.org/package/configurator) and
[configurator-ng](https://hackage.haskell.org/package/configurator-ng).

Here's an example:

```
# listen address
hostname = "localhost"
port = 8000

logdir = "$(HOME)/logs"
logfile = "$(logdir)/log.txt"
loglevels = [1, 4, 5]

users {
  alice = "alice@example.com"
  bob   = "bob@example.com"
}

# passwords.txt might contain
#   alice = "12345"
#   bob   = "sesame"
passwords {
  import "$(HOME)/secrets/passwords.txt"
}
```

## Usage

The following code can be used to parse the example above.

```
import Data.Configurator

data Settings = Settings
  { hostname  :: Text
  , port      :: Int
  , logfile   :: Maybe FilePath
  , loglevels :: Maybe [Int]
  , users     :: [(Text, Text)]
  , passwords :: [(Text, Text)]
  }

settingsParser :: Parser Config Settings
settingsParser =
  Settings
    <$> required "hostname" string
    <*> (Maybe.withDefault 1234 <$> optional "port" int)
    <*> optional "logfile" (pack <$> string)
    <*> optional "loglevels" (list int)
    <*> subassocs "users" string
    <*> subassocs "passwords" string

loadSettings :: IO Settings
loadSettings = do
  cfg <- load "settings.cfg"
  case runParser settingsParser cfg of
    Left err       -> die $ "reading config: " <> err
    Right settings -> return settings
```

Though note that for no apparent reason, `subassocs`
returns the full key, whence the parsed list of users
will be

```
    [ ("users.alice", "alice@example.com")
    , ("users.bob", "bob@example.com") ]
```
