module ParserSpec (
  parserTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Error(ParseError, Message, errorMessages, messageEq)
import Data.Char
import Data.Either (isLeft)
import Numeric

import Data.Hednist.Types
import Data.Hednist.Parser

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

parserTests = testGroup "Parser Tests"
  [ nilTests
  , characterTests
  , symbolTests
  , keywordTests
  , intTests
  , floatTests
  , listTests
  , vectorTests
  , stringTests
  , mapTests ]

-- Nil block
nilTests = testGroup "Nil Tests"
  [ testCase "nil is recognized" testNilValid
  , testCase "non-nils are not recognized" testNilInvalid ]

testNilValid = parse nil "" "nil" @?= Right EDNNil

testNilInvalid = isLeft (parse nil "" "null") @?= True

-- Character block

characterTests = testGroup "Character Tests"
  [ testProperty "basic characters are recognized" checkCharRecognized
  , testCase "newline is recognized" testNewlineRecognized
  , testCase "return is recognized" testReturnRecognized
  , testCase "space is recognized" testSpaceRecognized
  , testCase "tab is recognized" testTabRecognized
  , testCase "unicode space is recognized" testUnicodeRecognized
  , testCase "unicode space is recognized by character" testUnicodeCharacterRecognized ]

checkCharRecognized :: Char -> Bool
checkCharRecognized char =  parse character "" ("\\" ++ [char]) == Right (EDNChar char)

testNewlineRecognized = parse character "" "\\newline" @?= Right (EDNChar '\n')

testReturnRecognized = parse character "" "\\return" @?= Right (EDNChar '\r')

testSpaceRecognized = parse character "" "\\space" @?= Right (EDNChar ' ')

testTabRecognized = parse character "" "\\tab" @?= Right (EDNChar '\t')

testUnicodeRecognized = parse unicode "" "u0020" @?= Right ' '

testUnicodeCharacterRecognized = parse character "" "\\u0020" @?= Right (EDNChar ' ')

symbolTests = testGroup "Symbol Tests"
  [ testCase "simple symbols are recognized" testNakedSymbolRecognized
  , testCase "symbols with odd characters are recognized" testComplexSymbolRecognized
  , testCase "symbols with namespaces are recognized" testNamespaceSymbolRecognized ]

testNakedSymbolRecognized = parse symbol "" "aqrz.119" @?= Right (EDNSymbol Nothing "aqrz.119")

testComplexSymbolRecognized = parse symbol "" "+a564#" @?= Right (EDNSymbol Nothing "+a564#")

testNamespaceSymbolRecognized = parse symbol "" "+a5/abc" @?= Right (EDNSymbol (Just "+a5") "abc")

keywordTests = testGroup "Keyword Tests"
  [ testCase "simple keywords are recognized" testSimpleKeyword
  , testCase "namespaced keywords are recognized" testNamespacedKeyword
  , testCase "funky keywords are recognized" testFunkyKeyword ]

testSimpleKeyword = parse keyword "" ":a" @?= Right (EDNKeyword Nothing "a")

testNamespacedKeyword = parse keyword "" ":a.d/c" @?= Right (EDNKeyword (Just "a.d") "c")

testFunkyKeyword = parse keyword "" ":+a1d.f*/_q" @?= Right (EDNKeyword (Just "+a1d.f*") "_q")

intTests = testGroup "Int Tests"
  [ testProperty "ints are recognized" checkRecognizesIntegerSmall
  , testProperty "integers are recognized" checkRecognizesIntegerBig ]

checkRecognizesIntegerSmall :: Int -> Bool
checkRecognizesIntegerSmall i = parse integer "" (show i) == Right (EDNInt i)

checkRecognizesIntegerBig :: Integer -> Bool
checkRecognizesIntegerBig i = parse integer "" (show i ++ "N") == Right (EDNInteger i)

floatTests = testGroup "Float Tests"
  [ testProperty "coerced floats are recognized" checkRecognizesCast
  , testProperty "floats are recognized" checkRecognizesFloat
  , testProperty "exponents are recognized" checkRecognizesExponent ]

checkRecognizesCast :: Int -> Bool
checkRecognizesCast i = parse float "" (show i ++ "M") == Right (EDNFloat $ realToFrac i)

checkRecognizesFloat :: Double -> Bool
checkRecognizesFloat f = parse float "" (show f) == Right (EDNFloat f)

checkRecognizesExponent :: Int -> NonNegative Int -> Int -> Bool
checkRecognizesExponent f (NonNegative d) e =
  parse float "" str == Right (EDNFloat $ head * (10 ** realToFrac e))
  where head = read (show f ++ "." ++ show d)
        str = show head ++ "E" ++ show e

listTests = testGroup "List Tests"
  [ testCase "empty lists are recognized" testEmptyList
  , testCase "lists with elements are recognized" testFilledList ]

testEmptyList = parse list "" "()" @?=  Right (EDNList [])

testFilledList = parse list "" "(1 2)" @?= Right (EDNList [EDNInt 1, EDNInt 2])

vectorTests = testGroup "Vector Tests"
  [ testCase "empty vectors are recognized" testEmptyVector
   , testCase "vectors with elements are recognized" testFilledVector ]

testEmptyVector = parse Data.Hednist.Parser.vector "" "[]" @?= Right (EDNVector [])

testFilledVector = parse Data.Hednist.Parser.vector "" "[1 2]" @?= Right (EDNVector [EDNInt 1, EDNInt 2])

stringTests = testGroup "String Tests"
  [ testCase "simple strings are recognized" testSimpleString ]

testSimpleString = parse str "" "\"hi\"" @?= Right (EDNString "hi")

-- TODO: Generate strings based on the specification and ensure they are parsed

mapTests = testGroup "Map Tests"
  [ testCase "simple maps are recognized" testSimpleMap ]

testSimpleMap = parse dict "" "{:a 1, :b 2}" @?= Right (EDNMap [(EDNKeyword Nothing "a", EDNInt 1), (EDNKeyword Nothing "b", EDNInt 2)])
