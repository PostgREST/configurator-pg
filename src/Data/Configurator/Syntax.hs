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
import           Text.Megaparsec.Char.Lexer
import           Data.Char               (isAlpha, isAlphaNum, isSpace)
import           Data.Configurator.Types
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as L
import           Data.Text.Lazy.Builder  (fromText, singleton,
                                          toLazyText)
type Parser = Parsec Void Text

topLevel :: Parser [Directive]
topLevel = directives <* skipLWS <* eof

directive :: Parser Directive
directive =
  choice [
    try (string "import" *> skipLWS *> (Import <$> string_))
  , try (string "#;") *> skipHWS *> (DirectiveComment <$> directive)
  , (\ i f -> f i) <$> try ident <* skipLWS <*> (bindRHS <|> groupRHS)
  ]
  where
    bindRHS = char '=' *> skipLWS *> (flip Bind <$> value)
    groupRHS = char '{' *> skipLWS *> (flip Group <$> directives) <* skipLWS <* char '}'

directives :: Parser [Directive]
directives = try (skipLWS *> directive <* skipHWS) `sepEndBy`
             (satisfy $ \c -> c == '\r' || c == '\n')

data Skip = Space | Comment

-- | Skip lines, comments, or horizontal white space.
skipLWS :: Parser ()
skipLWS = loop
  where
    loop = takeWhileP (Just "space") isSpace >> ((comment >> loop) <|> return ())

    comment = try beginComment >> takeWhileP Nothing (\c -> c /= '\r' && c /= '\n')

    beginComment = do
      _ <- char '#'
      mc <- peekChar
      case mc of
        Just ';' -> fail ""
        _        -> return ()

    peekChar = optional (lookAhead anySingle)

scan :: s -> (s -> Char -> Maybe s) -> Parser Text
scan s f = fst <$> match (p s)
  where
    p st = eof <|> do
      c <- lookAhead anySingle
      case f st c of
        Nothing -> return ()
        Just st' -> anySingle >> p st'

-- | Skip comments or horizontal white space.
skipHWS :: Parser ()
skipHWS = scan Space go *> pure ()
  where go Space ' '    = Just Space
        go Space '\t'   = Just Space
        go Space '#'    = Just Comment
        go Space _      = Nothing
        go Comment '\r' = Nothing
        go Comment '\n' = Nothing
        go Comment _    = Just Comment

data IdentState = First | Follow

ident :: Parser Key
ident = do
  n <- scan First go
  when (n == "import") $
    fail $ "reserved word (" ++ show n ++ ") used as identifier"
  when (T.null n) $ fail "no identifier found"
  when (T.last n == '.') $ fail "identifier must not end with a dot"
  return n
 where
  go First c =
      if isAlpha c
      then Just Follow
      else Nothing
  go Follow c =
      if isAlphaNum c || c == '_' || c == '-'
      then Just Follow
      else if c == '.'
           then Just First
           else Nothing

value :: Parser Value
value = choice [
          string "on" *> pure (Bool True)
        , string "off" *> pure (Bool False)
        , string "true" *> pure (Bool True)
        , string "false" *> pure (Bool False)
        , String <$> string_
        , Number <$> scientific
        , List <$> brackets '[' ']'
                   ((value <* skipLWS) `sepBy` (char ',' <* skipLWS))
        ]

string_ :: Parser Text
string_ = do
  s <- char '"' *> scan False isChar <* char '"'
  if "\\" `T.isInfixOf` s
    then unescape s
    else return s
 where
  isChar True _ = Just False
  isChar _ '"'  = Nothing
  isChar _ c    = Just (c == '\\')

brackets :: Char -> Char -> Parser a -> Parser a
brackets open close p = char open *> skipLWS *> p <* char close

embed :: Parser a -> Text -> Parser a
embed p s = case parse p "(embed)" s of
              Left _  -> fail "embedded parser failed"
              Right v -> return v

unescape :: Text -> Parser Text
unescape = fmap (L.toStrict . toLazyText) . embed (p mempty)
 where
  p acc = do
    h <- takeWhileP Nothing (/='\\')
    let rest = do
          let cont c = p (acc `mappend` fromText h `mappend` singleton c)
          c <- char '\\' *> oneOf ("ntru\"\\" :: [Char])
          case c of
            'n'  -> cont '\n'
            't'  -> cont '\t'
            'r'  -> cont '\r'
            '"'  -> cont '"'
            '\\' -> cont '\\'
            _    -> cont =<< hexQuad
    done <- atEnd
    if done
      then return (acc `mappend` fromText h)
      else rest

hexQuad :: Parser Char
hexQuad = do
  a <- embed hexadecimal =<< takeP Nothing 4
  if a < 0xd800 || a > 0xdfff
    then return (chr a)
    else do
      b <- embed hexadecimal =<< string "\\u" *> takeP Nothing 4
      if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
        then return $! chr (((a - 0xd800) `shiftL` 10) + (b - 0xdc00) + 0x10000)
        else fail "invalid UTF-16 surrogates"

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
