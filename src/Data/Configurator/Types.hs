module Data.Configurator.Types
    ( Value(..)
    , Directive(..)
    , ParseError(..)
    , Key
    , Path
    , Config
    , Interpolate (..)
    ) where

import Protolude

import Data.Map.Strict (Map)
import Data.Scientific (Scientific)

-- | An error that occurred during the low-level parsing of a configuration file.
newtype ParseError = ParseError Text
  deriving (Show)

instance Exception ParseError

-- | An evaluated configuation.
type Config = Map Key Value

-- | The left-hand side of a configuration binding.
type Key = Text

-- | A packed 'FilePath'.
type Path = Text

-- | A key-value binding.
--type Binding = (Key, Value)

-- | A directive in a configuration file.
data Directive = Import Path
               | Bind Key Value
               | Group Key [Directive]
               | DirectiveComment Directive
                 deriving (Eq, Show)

-- | A general right-hand side value of a configuration binding.
data Value = Bool Bool
           -- ^ A Boolean. Represented in a configuration file as @on@
           -- or @off@, @true@ or @false@ (case sensitive).
           | String Text
           -- ^ A Unicode string.  Represented in a configuration file
           -- as text surrounded by double quotes.
           --
           -- Escape sequences:
           --
           -- * @\\n@ - newline
           --
           -- * @\\r@ - carriage return
           --
           -- * @\\t@ - horizontal tab
           --
           -- * @\\\\@ - backslash
           --
           -- * @\\\"@ - quotes
           --
           -- * @\\u@/xxxx/ - Unicode character, encoded as four
           --   hexadecimal digits
           --
           -- * @\\u@/xxxx/@\\u@/xxxx/ - Unicode character (as two
           --   UTF-16 surrogates)
           | Number Scientific
           -- ^ A number.
           | List [Value]
           -- ^ A heterogeneous list.  Represented in a configuration
           -- file as an opening square bracket \"@[@\", followed by a
           -- comma-separated series of values, ending with a closing
           -- square bracket \"@]@\".
             deriving (Eq, Show)

-- | An interpolation directive.
data Interpolate = Literal Text
                 | Interpolate Text
                   deriving (Eq, Show)
