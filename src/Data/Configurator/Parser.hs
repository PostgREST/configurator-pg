{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Data.Configurator.Parser
    ( Parser
    , runParser
    , bool
    , int
    , string
    , value
    , list
    , optional
    , required
    , subassocs
    ) where

import Protolude hiding (bool, list, optional)

import           Control.Monad.Fail
import           Data.Functor.Compose
import qualified Data.Map.Strict      as M
import qualified Data.Scientific      as Scientific
import qualified Data.Text            as T

import Data.Configurator.Types

-- | A generic parser.
--
-- A @'Parser' a b@ knows how to extract a @b@ from an @a@. Typical
-- instances are @'Parser' 'Value' a@, which handles the parsing of
-- individual configuration values, and @'Parser' 'Config' a@, which
-- handles extracting data from a full keyed configuration file.
newtype Parser a b = Parser { getParser :: Compose ((->) a) (Either Text) b }
  deriving (Functor, Applicative)

makeParser :: (a -> Either Text b) -> Parser a b
makeParser = Parser . Compose

-- | Run a parser.
--
-- @'runParser' p x@ runs the parser @p@ on the input @x@, returning
-- a value @'Right' v@ on success, or @'Left' err@ on error.
runParser :: Parser a b -> a -> Either Text b
runParser = getCompose . getParser

instance Monad (Parser a) where
  p >>= f = makeParser $ \v -> runParser p v >>= \w -> runParser (f w) v

instance MonadFail (Parser a) where
  fail s = makeParser (const (Left (T.pack s)))

-- | Parse a required configuration field.
--
-- @'required' key p@ expects the field @key@ to be present, and parses
-- its value with @p@.
required :: Key -> Parser Value a -> Parser Config a
required key pv = makeParser $ \cfg ->
                case M.lookup key cfg of
                       Nothing -> Left $ "missing key: " <> key
                       Just v  -> runParser pv v

-- | Parse an optional configuration field.
--
-- @'optional' key p@ returns 'Nothing' if the field @key@ is not present.
-- Otherwise it returns @'Just' v@, where @v@ is the result of parsing the
-- field value with @p@.
optional :: Key -> Parser Value a -> Parser Config (Maybe a)
optional key pv = makeParser $ \cfg ->
                case M.lookup key cfg of
                       Nothing -> Right Nothing
                       Just v  -> Just <$> runParser pv v

-- | Parse a set of fields with a shared prefix.
--
-- @'subassocs' prefix p@ extracts all configuration keys one level
-- below @prefix@, and collects pairs of the full keys and the
-- corresponding field values parsed with @p@.
subassocs :: Key -> Parser Value a -> Parser Config [(Key, a)]
subassocs prefix pv = makeParser $ \cfg ->
  M.toList <$> mapM (runParser pv) (M.filterWithKey match cfg)
 where
  match k _ = if T.null prefix
                then not (T.isInfixOf "." k)
                else case T.stripPrefix (prefix <> ".") k of
                       Nothing   -> False
                       Just suff -> not (T.isInfixOf "." suff)

-- | Parse a list of values.
--
-- @'list' p@ expects a list value, and parses each entry with @p@.
list :: Parser Value a -> Parser Value [a]
list p = makeParser $ \case
  List vs -> mapM (runParser p) vs
  _       -> Left "expected a list"

-- | Extract a raw value.
--
-- 'value' returns a configuration value in its raw form.
value :: Parser Value Value
value = makeParser pure

-- | Extract a string value.
--
-- 'string' expects the given value to be a string.
string :: Parser Value Text
string = makeParser $ \case
  String s -> Right s
  _        -> Left "expected a string"

-- | Extract an integer value.
--
-- 'int' expects the given value to be an 'Int'.
int :: Parser Value Int
int = makeParser $ \case
  Number n -> if Scientific.isInteger n
                then case Scientific.toBoundedInteger n of
                      Just x  -> Right x
                      Nothing -> Left "int out of bounds"
                else Left "expected an integer"
  _        -> Left "expected an integer"

-- | Extract a boolean value.
--
-- 'bool' expects the given value to be boolean.
bool :: Parser Value Bool
bool = makeParser $ \case
  Bool b -> Right b
  _      -> Left "expected a boolean"
