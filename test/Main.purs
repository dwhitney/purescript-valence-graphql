module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (drop, fold, range, reverse)
import Data.Char (fromCharCode)
import Data.Either (Either(..), isLeft)
import Data.List.Lazy (foldMap, (:), replicateM)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (singleton, toCharArray)
import Data.String as S
import Test.QuickCheck (class Arbitrary, (===))
import Test.QuickCheck.Gen (Gen, chooseInt, elements)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck, quickCheck')
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Text.Parsing.Parser (runParser)
import Valence.Query.Parser (floatValue, intValue, name, punctuator, stringValue)

complexQuery :: String
complexQuery = """
# test query
query FetchLukeAndLeiaAliased($someVar: Int = 1.23,$anotherVar: Int = 123)@include(if: true) @include(if: false){
  luke: human(id: "1000")@include(if: true){
    friends(sort: NAME)
  }
  leia: human(id , : , "10103\n \u00F6 ö") {
    name # some name
  }

  ... on User {
    birth{day}
  }

  ...Foo
}

fragment Foo on User @foo(bar: 1){
  baz # field in fragment!
}
"""

query1 :: String
query1 = """{ asdf(foo : "bar☃3", bar : 10, baz : 10e100, biz : 10.100e2){ a b c ... on Foo} }"""

{- 
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  oneStr <- pure $ runParser query1 lexicalTokens
  twoStr <- pure $ runParser complexQuery lexicalTokens
  oneDoc <- pure $ runParser query1 document
  let foo = spy oneDoc
  log $ show oneStr
-}

lowerCaseLetters :: Array Char 
lowerCaseLetters = toCharArray "abcdefghijklmnopqrstuvwxyz"

lowerCaseElements :: Gen Char
lowerCaseElements = elements ('a' :| (drop 1 lowerCaseLetters ))

upperCaseLetters :: Array Char 
upperCaseLetters = toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

numbers :: Array Char 
numbers = toCharArray "0123456789"

numbersNEL :: NonEmpty Array Char 
numbersNEL = ('0' :| (drop 1 numbers))

alphaNum :: Array Char
alphaNum = lowerCaseLetters <> upperCaseLetters <> numbers


nameFirstElements :: Gen Char
nameFirstElements = elements ('_' :| (lowerCaseLetters <> upperCaseLetters))

nameTailElements :: Gen Char
nameTailElements = elements ('_' :| (lowerCaseLetters <> upperCaseLetters <> numbers))

newtype Name = Name String

instance arbitraryName :: Arbitrary Name where
  arbitrary = do
    h     <- nameFirstElements
    i     <- chooseInt 0 100
    t     <- replicateM i nameTailElements
    pure $ Name (foldMap singleton (h : t))

newtype Punctuator = Punctuator String

instance arbitraryPunctuator :: Arbitrary Punctuator where
  arbitrary = do
    p <- punctuation
    pure (Punctuator p)
    where
      punctuation = elements ("!" :| ["$","(",")","...",":","=","@","[", "]","{","|","}"]) 


newtype IntValue = IntValue String

integerPart :: Gen String
integerPart = do
  s <- elements ('-' :| [])
  f <- elements ('1' :| (drop 2 numbers))
  i <- chooseInt 0 10
  n <- replicateM i (elements numbersNEL)
  pure $ foldMap singleton (s : f : n)

instance arbitraryIntValue :: Arbitrary IntValue where
  arbitrary = IntValue <$> integerPart 

fractionalPart :: Gen String
fractionalPart = do
  i <- chooseInt 1 10
  n <- replicateM i (elements numbersNEL) 
  pure $ ("." <> (foldMap singleton n))

exponentPart :: Gen String
exponentPart = do
  e <- elements ("e" :| ["E"])
  s <- elements ("" :| ["-", "+"])
  i <- chooseInt 1 6
  n <- replicateM i (elements numbersNEL)
  pure $ (e <> s <> (foldMap singleton n))

newtype IntegerFractional= IntegerFractional String

instance ipfp :: Arbitrary IntegerFractional where
  arbitrary = IntegerFractional <$> integerFactional 

integerFactional :: Gen String
integerFactional = do
  i <- integerPart
  f <- fractionalPart
  pure (i <> f)


newtype IntegerExponent = IntegerExponent String

integerExponent :: Gen String
integerExponent = do
  i <- integerPart
  e <- exponentPart
  pure (i <> e)

instance ie :: Arbitrary IntegerExponent where
  arbitrary = IntegerExponent <$> integerExponent

newtype IntegerFractionalExponent = IntegerFractionalExponent String

integerFractionalExponent :: Gen String 
integerFractionalExponent = do
    i <- integerPart
    f <- fractionalPart
    e <- exponentPart
    pure (i <> f <> e)


instance ife :: Arbitrary IntegerFractionalExponent where
  arbitrary = IntegerFractionalExponent <$> integerFractionalExponent

newtype FloatValue = FloatValue String

instance arbitraryFloatValue :: Arbitrary FloatValue where
  arbitrary = do
    i <- chooseInt 0 2
    case i of 
      0 -> FloatValue <$> integerFactional 
      1 -> FloatValue <$> integerExponent
      _ -> FloatValue <$> integerFractionalExponent

newtype StringValue = StringValue String

escapedChar :: Gen String
escapedChar = elements ("\\\"" :| ["\\\\", "\\/", "\\\\b", "\\\\f", "\\\n", "\\\r", "\\\t"])

sourceCharButNotSome :: Gen String
sourceCharButNotSome = 
  (singleton <<< fromCharCode) <$> (elements (32 :| ([33] <> (range 35 91) <> (range 93 65535))))

escapedUnicode :: Gen String
escapedUnicode = do
  s <- replicateM 4 (elements ('a' :| ['b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6', '7','8','9']))
  pure ("\\u" <> (foldMap singleton s))

instance arbitraryStringValue :: Arbitrary StringValue where
  arbitrary = do
    i <- chooseInt 1 50
    c <- chooseInt 0 2
    s <- replicateM i (case c of
          0 -> escapedChar 
          0 -> escapedUnicode
          _ -> sourceCharButNotSome)

    pure (StringValue ( "\"" <> (fold s) <> "\"") ) 


main :: Eff (QCRunnerEffects () ) Unit
main = run [consoleReporter] do 
  describe "Valence.Query.Parser" do
    describe "Name" do
      it "the common case" do
        (runParser "asdf" name) `shouldEqual` (Right "asdf")
    
      it "parses name beginning with underscore " do
        (runParser "_asdf" name) `shouldEqual` (Right "_asdf")
     
      it "doesn't parse a name beginning with a non-letter" do
        (isLeft (runParser "0asdf" name)) `shouldEqual` true 
      
      it "parses a name with second char as a num beginning with a non-letter" do
        (runParser "a0sdf" name) `shouldEqual` (Right "a0sdf")

      it "parses random names" do 
        quickCheck (\(Name n) -> (runParser n name) === (Right n))

      it "parses random punctuation" do
        quickCheck (\(Punctuator p) -> (runParser p punctuator) === (Right p))

      -- IntValue
    describe "IntValue" do
      it "parses zero" do
        (runParser "0" intValue) `shouldEqual` (Right "0")

      it "parses negative zero" do
        (runParser "-0" intValue) `shouldEqual` (Right "-0")
    
      it "parses random intValues" do
        quickCheck (\(IntValue n) -> (runParser n intValue) === (Right n))

      -- FloatValue
    describe "FloatValue" do
      it "parses IntegerPart FractionalPart" do 
        quickCheck (\(IntegerFractional n) -> (runParser n floatValue) === (Right n))
 
      it "parses IntegerPart ExponentPart" do 
        quickCheck (\(IntegerExponent n) -> (runParser n floatValue) === (Right n))

      it "parses IntegerPart FractionalPart ExponentPart" do 
        quickCheck (\(IntegerFractionalExponent n) -> (runParser n floatValue) === (Right n))

      it "parses random floatValues" do
        quickCheck (\(FloatValue n) -> (runParser n floatValue) === (Right n))
    
      -- StringValue
    describe "StringValue" do
   
      it "parses a StringValue with \\u0009" do
        (runParser "\"\\u0009\"" stringValue) `shouldEqual` (Right "\\u0009")
      
      it "parses a StringValue with \\u000A" do
        (runParser "\"\\u000A\"" stringValue) `shouldEqual` (Right "\\u000A")
      
      it "parses a StringValue with \\u000D" do
        (runParser "\"\\u000D\"" stringValue) `shouldEqual` (Right "\\u000D")
      
      it "parses random stringValues" do
        quickCheck' 1000 (\(StringValue s) -> (runParser s stringValue) === (Right (strReverse (S.drop 1 (strReverse (S.drop 1 s))))))

      

strReverse :: String -> String
strReverse str = foldMap singleton (reverse $ toCharArray str)
