module Main
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import ParserSpec

main = defaultMain parserTests

