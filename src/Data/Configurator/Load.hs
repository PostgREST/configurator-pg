module Data.Configurator.Load
  ( load
  ) where

import Protolude

import           Control.Exception                (throw)
import           Text.Megaparsec                  (parse, errorBundlePretty)
import qualified Data.Map.Strict                  as M
import           Data.Scientific                  (toBoundedInteger,
                                                   toRealFloat)
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL
import           Data.Text.Lazy.Builder           (fromString,
                                                   fromText,
                                                   toLazyText)
import           Data.Text.Lazy.Builder.Int       (decimal)
import           Data.Text.Lazy.Builder.RealFloat (realFloat)
import qualified System.Environment

import Data.Configurator.Syntax
import Data.Configurator.Types

-- | Read and parse a configuration file.
--
-- This may cause IO exceptions for reading this file or
-- imported files, and 'ParseError' if there is a problem
-- with parsing or evaluating the file.
load :: FilePath -> IO Config
load path = applyDirective "" "" M.empty (Import $ T.pack path)

loadOne :: Path -> IO [Directive]
loadOne path = do
  s <- readFile (T.unpack path)
  case parse topLevel (T.unpack path) s of
    Left err         -> throw $ ParseError $ T.pack $ errorBundlePretty err
    Right directives -> return directives

applyDirective :: Key -> Path -> Config -> Directive -> IO Config
applyDirective prefix path config directive = case directive of
  Bind key (String str) -> do
    v <- interpolate prefix str config
    return $! M.insert (prefix <> key) (String v) config
  Bind key value ->
    return $! M.insert (prefix <> key) value config
  Group key directives -> foldM (applyDirective prefix' path) config directives
    where prefix' = prefix <> key <> "."
  Import relpath ->
    let path' = relativize path relpath
    in do
      directives <- loadOne path'
      foldM (applyDirective prefix path') config directives
  DirectiveComment _ -> return config

interpolate :: Key -> Text -> Config -> IO Text
interpolate prefix s config
  | "$" `T.isInfixOf` s =
    case parse interp "(interpolated)" s of
      Left err   -> throw $ ParseError $ T.pack $ errorBundlePretty err
      Right xs -> TL.toStrict . toLazyText . mconcat <$> mapM interpret xs
  | otherwise = return s

 where
  lookupEnv name = msum $ map (flip M.lookup config) fullnames
    where fullnames = map (T.intercalate ".") -- ["a.b.c.x","a.b.x","a.x","x"]
                    . map (reverse . (name:)) -- [["a","b","c","x"],["a","b","x"],["a","x"],["x"]]
                    . tails                   -- [["c","b","a"],["b","a"],["a"],[]]
                    . reverse                 -- ["c","b","a"]
                    . filter (not . T.null)   -- ["a","b","c"]
                    . T.split (=='.')         -- ["a","b","c",""]
                    $ prefix                  -- "a.b.c."

  interpret (Literal x)   = return (fromText x)
  interpret (Interpolate name) =
    case lookupEnv name of
      Just (String x) -> return (fromText x)
      Just (Number r) ->
        case toBoundedInteger r :: Maybe Int64 of
          Just n  -> return (decimal n)
          Nothing -> return (realFloat (toRealFloat r :: Double))
      Just _  -> throw $ ParseError $ "variable '" <> name <> "' is not a string or number"
      Nothing -> do
        var <- System.Environment.lookupEnv (T.unpack name)
        case var of
          Nothing -> throw $ ParseError $ "no such variable: '" <> name <> "'"
          Just x  -> return (fromString x)

relativize :: Path -> Path -> Path
relativize parent child
  | T.head child == '/' = child
  | otherwise           = fst (T.breakOnEnd "/" parent) `T.append` child
