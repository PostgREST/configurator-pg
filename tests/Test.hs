{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Protolude hiding (bool, list, optional)

import           Data.Configurator
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
    [ testCase "read-simple" $ readTest "simple.cfg"
    , testCase "read-pathological" $ readTest "pathological.cfg"
    , testCase "load" loadTest
    , testCase "load" loadTest
    , testCase "types" typesTest
    , testCase "interp" interpTest
    , testCase "scoped-interp" scopedInterpTest
    , testCase "import" importTest
    , testCase "readme" readmeTest
    -- top level parsing errors
    , testCase "quote-error" $ parseErrorTest "err-single-quote.cfg"
    -- errors with imported files
    , testCase "import-error" $ ioErrorTest "err-import.cfg"
    , testCase "import-error-2" $ parseErrorTest "err-import-2.cfg"
    -- interpolation parsing errors
    , testCase "parse-interp-error-1" $ parseErrorTest "err-parse-interp-1.cfg"
    , testCase "parse-interp-error-2" $ parseErrorTest "err-parse-interp-2.cfg"
    , testCase "parse-interp-error-3" $ parseErrorTest "err-parse-interp-3.cfg"
    , testCase "parse-interp-error-4" $ parseErrorTest "err-parse-interp-4.cfg"
    -- interpolation interpretation errors
    , testCase "interpolate-error-1" $ parseErrorTest "err-interpolate-1.cfg"
    , testCase "interpolate-error-2" $ parseErrorTest "err-interpolate-2.cfg"
    ]

withLoad :: FilePath -> (Config -> IO ()) -> IO ()
withLoad name t = load (testFile name) >>= t

testFile :: FilePath -> FilePath
testFile name = "tests" </> "resources" </> name

errorFile :: FilePath -> FilePath
errorFile name = testFile name <> ".err"

parse :: Config -> Parser Value a -> Key -> Either Text a
parse cfg p key = runParser (required key p) cfg

parseOpt :: Config -> Parser Value a -> Key -> Either Text (Maybe a)
parseOpt cfg p key = runParser (optional key p) cfg

parseSub :: Config -> Parser Value a -> Key -> Either Text [(Key, a)]
parseSub cfg p prefix = runParser (subassocs prefix p) cfg

readTest :: FilePath -> Assertion
readTest file = load (testFile file) >> return ()

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

parseErrorTest :: FilePath -> Assertion
parseErrorTest file = do
  err <- readFile $ errorFile file
  (load (testFile file) >> assertFailure "expected a parse error")
    `catch` \ (ParseError err') -> do
       assertEqual "" err err'

ioErrorTest :: FilePath -> Assertion
ioErrorTest file = do
  err <- readFile $ errorFile file
  (load (testFile file) >> assertFailure "expected an IO error")
    `catch` \ (ex :: IOException) -> do
       assertEqual "" err (show ex)
