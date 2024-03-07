# Revision history for configurator-pg

## 0.2.10 -- 2024-03-06

* Allow megaparsec-9.6
* Remove unused bytestring dependency from tests

## 0.2.9 -- 2023-12-07

* Allow text-2.1 and GHC 9.8.

## 0.2.8 -- 2023-12-07

* Allow megaparsec-9.5 and GHC 9.4.8.

## 0.2.7 -- 2022-08-11

* Allow text-2.0 and GHC 9.4.

## 0.2.6 -- 2022-05-24

* Allow megaparsec-9.2 and GHC 9.2.

## 0.2.5 -- 2020-10-26

* Allow bytestring-0.11.

## 0.2.4 -- 2020-09-04

* Allow megaparsec-9.0.

## 0.2.3 -- 2020-04-19

* Drop support for GHC 7.10.

## 0.2.2 -- 2020-04-07

* Support GHC 8.10.

## 0.2.1 -- 2020-03-11

* Include test output files in source release.

## 0.2.0 -- 2020-03-10

* Rewrite file parser with Megaparsec instead of
  Attoparsec for better error messages. No expected
  change in functionality except possibly in
  pathological cases.

## 0.1.0.6 -- 2020-03-08

* Fix GHC 7.10 build by depending on package fail.

## 0.1.0.5 -- 2020-03-07

* Provide a MonadFail instance for Parser.
  This fixes a PostgREST compile failure with GHC 8.8.3.

## 0.1.0.4 -- 2020-03-07

* Relax bounds to support GHC 8.8.3.

## 0.1.0.3 -- 2019-06-04

* Include another missing test data file.

## 0.1.0.2 -- 2019-06-04

* Lower bounds to support GHC 7.10.3.
* Include test data to fix tests.

## 0.1.0.1 -- 2019-06-03

* Include README.md in release.

## 0.1.0.0 -- 2019-06-03

* First version.
