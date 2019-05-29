-- |
-- Module:      Data.Configurator
-- Description: A configuration file parser
--
-- A simplified library for reading configuration files
-- in the format of [configurator-ng](https://hackage.haskell.org/package/configurator-ng).

module Data.Configurator
    (
    -- * Types
      Key
    , Value(..)
    , Config
    -- * Low-level parsing
    , load
    , ParseError(..)
    -- * High-level parsing
    , Parser
    , runParser
    -- ** Value parsers
    , bool
    , int
    , string
    , value
    , list
    -- ** Configuration parsers
    , optional
    , required
    , subassocs
    ) where

import Data.Configurator.Load   (load)
import Data.Configurator.Parser
import Data.Configurator.Types
