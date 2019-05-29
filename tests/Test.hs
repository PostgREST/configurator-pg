{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (lookup)

import           Data.Configurator
import           Data.Function                  (on)
import           Data.List                      (sortBy)
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           System.Environment
import           System.FilePath
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testCase "read" readTest
    , testCase "load" loadTest
    , testCase "types" typesTest
    , testCase "interp" interpTest
    , testCase "scoped-interp" scopedInterpTest
    , testCase "import" importTest
    , testCase "readme" readmeTest
    ]

withLoad :: FilePath -> (Config -> IO ()) -> IO ()
withLoad name t = load (testFile name) >>= t

testFile :: FilePath -> FilePath
testFile name = "tests" </> "resources" </> name

parse :: Config -> Parser Value a -> Key -> Either Text a
parse cfg p key = runParser (required key p) cfg

parseOpt :: Config -> Parser Value a -> Key -> Either Text (Maybe a)
parseOpt cfg p key = runParser (optional key p) cfg

parseSub :: Config -> Parser Value a -> Key -> Either Text [(Key, a)]
parseSub cfg p prefix = runParser (subassocs prefix p) cfg

readTest :: Assertion
readTest = load (testFile "pathological.cfg") >> return ()

loadTest :: Assertion
loadTest =
  withLoad "pathological.cfg" $ \cfg -> do
    assertEqual "int property"
      (Right 1)
      (parse cfg int "aa")

    assertEqual "int as value"
      (Right (Number 1))
      (parse cfg value "aa")

    assertEqual "string property"
      (Right "foo")
      (parse cfg string "ab")

    assertEqual "nested int"
      (Right 1)
      (parse cfg int "ac.x")

    assertEqual "nested bool"
      (Right True)
      (parse cfg bool "ac.y")

    assertEqual "simple bool"
      (Right False)
      (parse cfg bool "ad")

    assertEqual "simple int 2"
      (Right 1)
      (parse cfg int "ae")

    assertEqual "simple int 2"
      (Right [2, 3])
      (parse cfg (list int) "af")

    assertEqual "deep bool"
      (Right False)
      (parse cfg bool "ag.q-e.i_u9.a")

    assertEqual "notacomment"
      (Right 42)
      (parse cfg int "notacomment")

    assertEqual "comment"
      (Left "missing key: comment.x")
      (parse cfg int "comment.x")

typesTest :: Assertion
typesTest =
  withLoad "pathological.cfg" $ \cfg -> do
    assertEqual "bad text"
      (Left "expected a string")
      (parse cfg string "aa")

    assertEqual "bad text list"
      (Left "expected a list")
      (parse cfg (list string) "ab")

    assertEqual "bad text list 2"
      (Left "expected a string")
      (parse cfg (list string) "af")

    assertEqual "heterogeneous list"
      (Right [Number 1, Number 2, String "3"])
      (parse cfg (list value) "xs")

    assertEqual "bad text list (heterogeneous)"
      (Left "expected a string")
      (parse cfg string "xs")

    assertEqual "bad int list (heterogeneous)"
      (Left "expected an integer")
      (parse cfg int "xs")

    assertEqual "assocs"
      (Right [("ac.x", Number 1), ("ac.y", Bool True)])
      (parseSub cfg value "ac")

    home <- T.pack <$> getEnv "HOME"
    assertEqual "assocs'"
      (Right (sortBy (compare `on` fst)
               [ ("aa", Number 1)
               , ("ab", String "foo")
               , ("ad", Bool False)
               , ("ae", Number 1)
               , ("af", List [Number 2, Number 3])
               , ("ba", String home)
               , ("xs", List [Number 1, Number 2, String "3"])
               , ("c" , String "x")
               , ("notacomment", Number 42)
               ]))
      (parseSub cfg value "")

interpTest :: Assertion
interpTest =
  withLoad "pathological.cfg" $ \cfg -> do
    home    <- getEnv "HOME"
    assertEqual "home interp"
      (Right (T.pack home))
      (parse cfg string "ba")

scopedInterpTest :: Assertion
scopedInterpTest = withLoad "interp.cfg" $ \cfg -> do
    home <- T.pack <$> getEnv "HOME"

    assertEqual "myprogram.exec"
      (Right $ home <> "/services/myprogram/myprogram")
      (parse cfg string "myprogram.exec")

    assertEqual "myprogram.stdout"
      (Right $ home <> "/services/myprogram/stdout")
      (parse cfg string "myprogram.stdout")

    assertEqual "nested scope"
      (Right $ home <> "/top/layer1/layer2")
      (parse cfg string "top.layer1.layer2.dir")

importTest :: Assertion
importTest =
  withLoad "import.cfg" $ \cfg -> do
    assertEqual "simple"
      (Right 1)
      (parse cfg int "x.aa")

    assertEqual "nested"
      (Right 1)
      (parse cfg int "x.ac.x")

data Settings = Settings
  { hostname  :: Text
  , port      :: Int
  , logfile   :: Maybe FilePath
  , loglevels :: Maybe [Int]
  , users     :: [(Text, Text)]
  , passwords :: [(Text, Text)]
  }
  deriving (Show, Eq)

settingsParser :: Parser Config Settings
settingsParser =
  Settings
    <$> required "hostname" string
    <*> (fromMaybe 1234 <$> optional "port" int)
    <*> optional "logfile" (T.unpack <$> string)
    <*> optional "loglevels" (list int)
    <*> subassocs "users" string
    <*> subassocs "passwords" string

readmeTest :: Assertion
readmeTest =
  withLoad "readme.cfg" $ \cfg -> do
    assertEqual "readme"
      (Right (Settings
                "localhost"
                8000
                (Just "/var/log/log.txt")
                (Just [1,4,5])
                [("users.alice", "alice@example.com"), ("users.bob", "bob@example.com")]
                [("passwords.alice", "secret")]))
      (runParser settingsParser cfg)
