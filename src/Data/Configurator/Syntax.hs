{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Data.Configurator.Syntax
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2015-2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- A parser for configuration files.

module Data.Configurator.Syntax
    (
      topLevel
    , interp
    ) where

import Protolude hiding (First, try)

import           Control.Monad           (fail)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Char                  as Char
import           Data.Configurator.Types
import qualified Data.Text               as T

type Parser = Parsec Void Text

topLevel :: Parser [Directive]
topLevel = skipLWS *> directives <* skipLWS <* eof

directive :: Parser Directive
directive =
  choice
   [ do try (keyword "import") <* skipLWS
        Import <$> string_
   , do ident <- identifier <* skipLWS
        choice
          [ Bind ident <$> (char '=' *> skipLWS *> value)
          , Group ident <$> brackets '{' '}' directives
          ]
   , do string "#;" *> skipHWS
        DirectiveComment <$> directive
   ]

directives :: Parser [Directive]
directives = (directive <* skipHWS) `sepEndBy` (eol *> skipLWS) <* skipLWS

-- | Skip lines, comments, or horizontal white space.
skipLWS :: Parser ()
skipLWS = Lexer.space space1 comment empty
  where
    beginComment = char '#' *> notFollowedBy (char ';')
    comment = try beginComment <* takeWhileP Nothing (\c -> c /= '\r' && c /= '\n')

-- | Skip comments or horizontal white space.
skipHWS :: Parser ()
skipHWS = Lexer.space
            (satisfy (\c -> c == ' ' || c == '\t') >> return ())
            (Lexer.skipLineComment "#")
            empty

isIdentifier :: Char -> Bool
isIdentifier c = Char.isAlphaNum c || c == '_' || c == '-'

keyword :: Text -> Parser ()
keyword kw = string kw *> notFollowedBy (satisfy isAnyIdentifier)
  where
    isAnyIdentifier c = c == '.' || isIdentifier c

identifier :: Parser Key
identifier = fst <$> match (word `sepBy1` char '.')
 where
  word = T.cons <$> letterChar <*> takeWhileP (Just "alphanumeric character") isIdentifier

value :: Parser Value
value = choice [
          string "on" *> pure (Bool True)
        , string "off" *> pure (Bool False)
        , string "true" *> pure (Bool True)
        , string "false" *> pure (Bool False)
        , String <$> string_
        , Number <$> Lexer.scientific
        , List <$> brackets '[' ']'
                   ((value <* skipLWS) `sepBy` (char ',' <* skipLWS))
        ]

string_ :: Parser Text
string_ = T.pack <$> str
 where
  str = char '"' *> manyTill charLiteral (char '"')

brackets :: Char -> Char -> Parser a -> Parser a
brackets open close p = between (char open *> skipLWS) (char close) p

charLiteral :: Parser Char
charLiteral = choice
  [ char '\\' *> parseEscape
  , anySingle
  ]
 where
  parseEscape = do
    c <- oneOf ("ntru\"\\" :: [Char])
    case c of
      'n'  -> pure '\n'
      't'  -> pure '\t'
      'r'  -> pure '\r'
      '"'  -> pure '"'
      '\\' -> pure '\\'
      _    -> hexQuad

hexQuad :: Parser Char
hexQuad = do
  a <- quad
  if a < 0xd800 || a > 0xdfff
    then return (chr a)
    else do
      b <- string "\\u" *> quad
      if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
        then return $! chr (((a - 0xd800) `shiftL` 10) + (b - 0xdc00) + 0x10000)
        else fail "invalid UTF-16 surrogates"
 where
  quad     = mkNum <$> count 4 (satisfy Char.isHexDigit <?> "hexadecimal digit")
  mkNum    = foldl' step 0
  step a c = a * 16 + fromIntegral (Char.digitToInt c)

-- | Parse a string interpolation spec.
--
-- The sequence @$$@ is treated as a single @$@ character.  The
-- sequence @$(@ begins a section to be interpolated, and @)@ ends it.
interp :: Parser [Interpolate]
interp = reverse <$> p []
 where
  p acc = do
    h <- Literal <$> takeWhileP Nothing (/='$')
    let rest = do
          let cont x = p (x : h : acc)
          c <- char '$' *> satisfy (\c -> c == '$' || c == '(')
          case c of
            '$' -> cont (Literal (T.singleton '$'))
            _   -> (cont . Interpolate) =<< takeWhile1P Nothing (/=')') <* char ')'
    done <- atEnd
    if done
      then return (h : acc)
      else rest
