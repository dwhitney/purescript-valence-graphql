module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (drop, fromFoldable, fold, range, reverse)
import Data.Char (fromCharCode)
import Data.Either (Either(..), isLeft)
import Data.Int (round)
import Data.List.Lazy (foldMap, (:), replicateM)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (singleton, toCharArray)
import Data.String as S
import Debug.Trace (spy)
import Global (readInt, readFloat)
import Test.QuickCheck (class Arbitrary, arbitrary, (===))
import Test.QuickCheck.Gen (Gen, chooseInt, elements)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck, quickCheck')
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Text.Parsing.Parser (ParseError(..), runParser)
import Text.Parsing.Parser.Pos (Position(Position))
import Valence.Query.AST (Alias(Alias), Argument(Argument), Arguments(Arguments), DefaultValue(DefaultValue), Definition(..), Directive(Directive), Directives(Directives), Field(..), FragmentName(FragmentName), FragmentSpread(FragmentSpread), GQLType(NamedType, ListType, NonNullType), InlineFragment(..), NonNull(NonNullNamed, NonNullList), ObjectField(ObjectField), OperationType(..), Selection(..), SelectionSet(..), TypeCondition(TypeCondition), Value(NullValue, BooleanValue, ListValue, StringValue, ObjectValue, FloatValue, IntValue, EnumValue, Variable), VariableDefinition(..), VariableDefinitions(..), toQueryString)
import Valence.Query.Parser (alias, argument, arguments, defaultValue, definition, directive, directives, document, field, floatValue, fragmentName, fragmentSpread, gqlType, inlineFragment, intValue, name, punctuator, selection, selectionSet, stringValue, typeCondition, value, variableDefinition, variableDefinitions)

kitchenSink :: String
kitchenSink = """
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

newtype NameGen = NameGen String

nameGen :: Gen String
nameGen = do
  h     <- nameFirstElements
  i     <- chooseInt 0 25 
  t     <- replicateM i nameTailElements
  pure $ (foldMap singleton (h : t))


instance arbitraryNameGen :: Arbitrary NameGen where
  arbitrary = NameGen <$> nameGen

newtype PunctuatorGen = PunctuatorGen String

instance arbitraryPunctuator :: Arbitrary PunctuatorGen where
  arbitrary = do
    p <- punctuation
    pure (PunctuatorGen p)
    where
      punctuation = elements ("!" :| ["$","(",")","...",":","=","@","[", "]","{","|","}"]) 


newtype IntValueGen = IntValueGen String

integerPart :: Gen String
integerPart = do
  s <- elements ('-' :| [])
  f <- elements ('1' :| (drop 2 numbers))
  i <- chooseInt 0 3 
  n <- replicateM i (elements numbersNEL)
  pure $ foldMap singleton (s : f : n)

instance arbitraryIntValue :: Arbitrary IntValueGen where
  arbitrary = IntValueGen <$> integerPart 

fractionalPart :: Gen String
fractionalPart = do
  i <- chooseInt 1 3 
  n <- replicateM i (elements numbersNEL) 
  pure $ ("." <> (foldMap singleton n))

exponentPart :: Gen String
exponentPart = do
  e <- elements ("e" :| ["E"])
  s <- elements ("" :| ["-", "+"])
  i <- chooseInt 1 2
  n <- replicateM i (elements numbersNEL)
  pure $ (e <> s <> (foldMap singleton n))

newtype IntegerFractional = IntegerFractional String

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

newtype FloatValueGen = FloatValueGen String

instance arbitraryFloatValue :: Arbitrary FloatValueGen where
  arbitrary = do
    i <- chooseInt 0 2
    case i of 
      0 -> FloatValueGen <$> integerFactional 
      1 -> FloatValueGen <$> integerExponent
      _ -> FloatValueGen <$> integerFractionalExponent

newtype StringValueGen = StringValueGen String

newtype ArbitraryValue = ArbitraryValue Value

instance arbitraryValue :: Arbitrary ArbitraryValue where
  arbitrary = ArbitraryValue <$> do
    i <- chooseInt 0 8
    case i of
      0 -> Variable <$> nameGen
      1 -> arbitrary <#> (\(IntValueGen i) -> IntValue (round (readInt 10 i)))
      2 -> arbitrary <#> (\(FloatValueGen i) -> FloatValue (readFloat i))
      3 -> StringValue <$> genStringValue
      4 -> BooleanValue <$> arbitrary
      5 -> EnumValue <$> nameGen
      6 -> pure NullValue
      7 -> ListValue <$> (do
        i <- chooseInt 0 2 
        l <- (replicateM i arbitrary) <#> (\list -> map (\(ArbitraryValue v) -> v) (fromFoldable list))
        pure l)
      _ -> arbitrary <#> (\(FloatValueGen i) -> FloatValue (readFloat i))

escapedChar :: Gen String
escapedChar = elements ("\\\"" :| ["\\\\", "\\/", "\\\\b", "\\\\f", "\\\n", "\\\r", "\\\t"])

sourceCharButNotSomeArray :: NonEmpty Array String
sourceCharButNotSomeArray = map (fromCharCode >>> singleton )  (32 :| ([33] <> (range 35 91) <> (range 93 65535)))

sourceCharButNotSome :: Gen String
sourceCharButNotSome = elements sourceCharButNotSomeArray

escapedUnicode :: Gen String
escapedUnicode = do
  s <- replicateM 4 (elements ('a' :| ['b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6', '7','8','9']))
  pure ("\\u" <> (foldMap singleton s))

genStringValue :: Gen String
genStringValue = do
    i <- chooseInt 1 20
    c <- chooseInt 0 2
    s <- replicateM i (case c of
          0 -> escapedChar 
          0 -> escapedUnicode
          _ -> sourceCharButNotSome)
    pure (fold s)

instance arbitraryStringValue :: Arbitrary StringValueGen where
  arbitrary = genStringValue <#> (\str -> StringValueGen ( "\"" <> str <> "\""))

newtype ArbitraryAlias = ArbitraryAlias Alias

instance arbitraryAlias :: Arbitrary ArbitraryAlias where
  arbitrary = do
    n <- nameGen
    pure (ArbitraryAlias (Alias n))

newtype ArbitraryArgument = ArbitraryArgument Argument

instance arbitraryArgument :: Arbitrary ArbitraryArgument where
  arbitrary = do
    n                   <- nameGen
    (ArbitraryValue v)  <- arbitrary 
    pure $ ArbitraryArgument (Argument n v)

newtype ArbitraryArguments = ArbitraryArguments Arguments 

instance arbitraryArguments :: Arbitrary ArbitraryArguments where
  arbitrary = do
    i     <- chooseInt 1 3 
    args  <- (replicateM i arbitrary) <#> (\list -> map (\(ArbitraryArgument arg) -> arg) list)
    pure (ArbitraryArguments (Arguments (fromFoldable args)))

newtype ArbitraryDirective = ArbitraryDirective Directive

instance arbitraryDirective :: Arbitrary ArbitraryDirective where
  arbitrary = do
    n                         <- nameGen
    b                         <- arbitrary
    case b of 
      true -> do
          (ArbitraryArguments args) <- arbitrary 
          pure (ArbitraryDirective (Directive n (Just args)))
      false -> pure (ArbitraryDirective (Directive n Nothing))

newtype ArbitraryDirectives = ArbitraryDirectives Directives

instance arbitraryDirectives :: Arbitrary ArbitraryDirectives where
  arbitrary = do
    (ArbitraryDirective d)  <- arbitrary
    i                       <- chooseInt 0 2
    ds                      <- replicateM i (arbitrary <#> (\(ArbitraryDirective d) -> d))
    pure (ArbitraryDirectives (Directives (d :| (fromFoldable ds))))

newtype ArbitraryGQLType = ArbitraryGQLType GQLType 
newtype ArbitraryNonNull = ArbitraryNonNull NonNull

instance arbitraryGQLType :: Arbitrary ArbitraryGQLType where
  arbitrary = do
    i <- chooseInt 0 2
    ArbitraryGQLType <$> (case i of 
      0 -> NamedType <$> nameGen  
      1 -> ListType <$> (arbitrary <#> (\(ArbitraryGQLType t) -> t))
      _ -> NonNullType <$> (arbitrary <#> (\(ArbitraryNonNull t) -> t))
    ) 
instance arbitraryNonNull :: Arbitrary ArbitraryNonNull where
  arbitrary = do
    i <- chooseInt 0 1
    ArbitraryNonNull <$> (case i of 
      0 -> NonNullNamed <$> nameGen 
      _ -> NonNullList <$> (arbitrary <#> (\(ArbitraryGQLType t) -> t))
    )

newtype ArbitraryDefaultValue = ArbitraryDefaultValue DefaultValue

instance arbitraryDefaultValue :: Arbitrary ArbitraryDefaultValue where
  arbitrary = do
    (ArbitraryValue v) <- arbitrary
    pure (ArbitraryDefaultValue (DefaultValue v))

newtype ArbitraryTypeCondition = ArbitraryTypeCondition TypeCondition 

instance arbitraryTypeCondition :: Arbitrary ArbitraryTypeCondition where
  arbitrary = do
    n <- nameGen
    pure (ArbitraryTypeCondition (TypeCondition n))

newtype ArbitraryFragmentName = ArbitraryFragmentName FragmentName 

instance arbitraryFragmentName :: Arbitrary ArbitraryFragmentName where
  arbitrary = do
    n <- nameGen
    if n == "on"
      then pure (ArbitraryFragmentName (FragmentName "onx"))
      else pure (ArbitraryFragmentName (FragmentName n))

newtype ArbitraryFragmentSpread = ArbitraryFragmentSpread FragmentSpread

instance arbitraryFragmentSpread :: Arbitrary ArbitraryFragmentSpread where
  arbitrary = do
    (ArbitraryFragmentName n) <- arbitrary
    b                         <- arbitrary
    if b
      then do
        (ArbitraryDirectives d) <- arbitrary
        pure (ArbitraryFragmentSpread (FragmentSpread n (Just d)))
      else pure (ArbitraryFragmentSpread (FragmentSpread n Nothing))

newtype ArbitraryField = ArbitraryField Field

instance arbitraryField :: Arbitrary ArbitraryField where
  arbitrary = do
    maybeAlias        <- arbitrary >>= (if _ then (arbitrary <#> (\(ArbitraryAlias alias) -> Just alias)) else pure Nothing)  
    maybeArgs         <- arbitrary >>= (if _ then (arbitrary <#> (\(ArbitraryArguments args) -> Just args)) else pure Nothing)  
    maybeDirectives   <- arbitrary >>= (if _ then (arbitrary <#> (\(ArbitraryDirectives dirs) -> Just dirs)) else pure Nothing)  
    maybeSelectionSet <- arbitrary >>= (if _ then (arbitrary <#> (\(ArbitrarySelectionSet ss) -> Just ss)) else pure Nothing)  
    n                 <- nameGen
    pure (ArbitraryField (Field maybeAlias n maybeArgs maybeDirectives maybeSelectionSet))

newtype ArbitrarySelection = ArbitrarySelection Selection

instance arbitrarySelection :: Arbitrary ArbitrarySelection where
  arbitrary = do
    i <- chooseInt 0 2
    case i of 
      0 -> arbitrary <#> (\(ArbitraryField field) -> ArbitrarySelection (SelectionField field))
      1 -> arbitrary <#> (\(ArbitraryFragmentSpread spread) -> ArbitrarySelection (SelectionFragmentSpread spread))
      _ -> arbitrary <#> (\(ArbitraryInlineFragment inline) -> ArbitrarySelection (SelectionInlineFragment inline))

newtype ArbitrarySelectionSet = ArbitrarySelectionSet SelectionSet 

instance arbitrarySelectionSet :: Arbitrary ArbitrarySelectionSet where
  arbitrary = do
    (ArbitrarySelection s)  <- arbitrary
    i                       <- chooseInt 0 2
    ss                      <- replicateM i (arbitrary <#> (\(ArbitrarySelection sel) -> sel))
    pure (ArbitrarySelectionSet (SelectionSet (s :| (fromFoldable ss))))


newtype ArbitraryInlineFragment = ArbitraryInlineFragment InlineFragment

instance arbitraryInlineFragment :: Arbitrary ArbitraryInlineFragment where
  arbitrary = do
    tc                          <- arbitrary >>= if _
                                                  then (arbitrary <#> (\(ArbitraryTypeCondition tc) -> Just tc))
                                                  else pure Nothing
    d                           <- arbitrary >>= if _ 
                                                  then (arbitrary <#> (\(ArbitraryDirectives d) -> Just d))
                                                  else pure Nothing
    (ArbitrarySelectionSet ss)  <- arbitrary
    pure (ArbitraryInlineFragment (InlineFragment tc d ss))

newtype ArbitraryVariableDefinition = ArbitraryVariableDefinition VariableDefinition

instance arbitraryVariableDefinition :: Arbitrary ArbitraryVariableDefinition where
  arbitrary = do
    n <- nameGen
    (ArbitraryGQLType t) <- arbitrary
    d                     <- arbitrary >>= (if _ then arbitrary <#> (\(ArbitraryDefaultValue v) -> Just v) else pure Nothing) 
    pure (ArbitraryVariableDefinition (VariableDefinition n t d))

newtype ArbitraryVariableDefinitions = ArbitraryVariableDefinitions VariableDefinitions

instance arbitraryVariableDefinitions :: Arbitrary ArbitraryVariableDefinitions where
  arbitrary = do
    (ArbitraryVariableDefinition v) <- arbitrary
    i                               <- chooseInt 0 2
    vs                              <- (replicateM i arbitrary) <#> (\list -> map (\(ArbitraryVariableDefinition d) -> d) (fromFoldable list))
    pure (ArbitraryVariableDefinitions (VariableDefinitions (v :| vs)))

newtype ArbitraryDefinition = ArbitraryDefinition Definition 

instance arbitraryDefinition :: Arbitrary ArbitraryDefinition where
  arbitrary = ArbitraryDefinition <$> (arbitrary >>= if _ 
    then do
      q   <- arbitrary <#> if _ then Query else Mutation
      n   <- arbitrary >>= if _ then Just <$> nameGen else pure Nothing
      vd  <- arbitrary >>= if _ then Just <$> (arbitrary <#> (\(ArbitraryVariableDefinitions def) -> def)) else pure Nothing
      d   <- arbitrary >>= if _ then Just <$> (arbitrary <#> (\(ArbitraryDirectives dir) -> dir)) else pure Nothing
      ss  <- arbitrary <#> (\(ArbitrarySelectionSet set) -> set)
      pure (Operation q n vd d ss)

    else do
      fn  <- arbitrary <#> (\(ArbitraryFragmentName n) -> n)
      tc  <- arbitrary <#> (\(ArbitraryTypeCondition t) -> t)
      d   <- arbitrary >>= if _ then Just <$> (arbitrary <#> (\(ArbitraryDirectives dir) -> dir)) else pure Nothing
      ss  <- arbitrary <#> (\(ArbitrarySelectionSet set) -> set)
      pure (Fragment fn tc d ss)
  ) 

main :: Eff (QCRunnerEffects () ) Unit
main = run [consoleReporter] do 

  describe "Valence.Query.Parser" do
{-}
    describe "NameGen" do
      it "the common case" do
        (runParser "asdf" name) `shouldEqual` (Right "asdf")
    
      it "parses name beginning with underscore " do
        (runParser "_asdf" name) `shouldEqual` (Right "_asdf")
     
      it "doesn't parse a name beginning with a non-letter" do
        (isLeft (runParser "0asdf" name)) `shouldEqual` true 
      
      it "parses a name with second char as a num beginning with a non-letter" do
        (runParser "a0sdf" name) `shouldEqual` (Right "a0sdf")

      it "parses random names" do 
        quickCheck (\(NameGen n) -> (runParser n name) === (Right n))

      it "parses random punctuation" do
        quickCheck (\(PunctuatorGen p) -> (runParser p punctuator) === (Right p))

      -- IntValue
    describe "intValue" do
      it "parses zero" do
        (runParser "0" intValue) `shouldEqual` (Right "0")

      it "parses negative zero" do
        (runParser "-0" intValue) `shouldEqual` (Right "-0")
    
      it "parses random intValues" do
        quickCheck (\(IntValueGen n) -> (runParser n intValue) === (Right n))

      -- FloatValue
    describe "FloatValue" do
      it "parses IntegerPart FractionalPart" do 
        quickCheck (\(IntegerFractional n) -> (runParser n floatValue) === (Right n))
 
      it "parses IntegerPart ExponentPart" do 
        quickCheck (\(IntegerExponent n) -> (runParser n floatValue) === (Right n))

      it "parses IntegerPart FractionalPart ExponentPart" do 
        quickCheck (\(IntegerFractionalExponent n) -> (runParser n floatValue) === (Right n))

      it "parses random floatValues" do
        quickCheck (\(FloatValueGen n) -> (runParser n floatValue) === (Right n))
    
      -- StringValue
    describe "StringValue" do
   
      it "parses a StringValue with \\u0009" do
        (runParser "\"\\u0009\"" stringValue) `shouldEqual` (Right "\\u0009")
      
      it "parses a StringValue with \\u000A" do
        (runParser "\"\\u000A\"" stringValue) `shouldEqual` (Right "\\u000A")
      
      it "parses a StringValue with \\u000D" do
        (runParser "\"\\u000D\"" stringValue) `shouldEqual` (Right "\\u000D")
      
      it "parses random stringValues" do
        quickCheck' 1000 (\(StringValueGen s) -> (runParser s stringValue) === (Right (strReverse (S.drop 1 (strReverse (S.drop 1 s))))))

    describe "Value" do

      it "parses Variable" do
        (runParser "$foo" value) `shouldEqual` (Right (Variable "foo"))

      it "parses IntValue" do
        (runParser "10" value) `shouldEqual` (Right (IntValue (10 :: Int)))

      it "parses FloatValue" do
        (runParser "10.9" value) `shouldEqual` (Right (FloatValue (10.9)))

      it "parses StringValue" do
        (runParser "\"Hello, World!\"" value) `shouldEqual` (Right (StringValue ("Hello, World!")))

      it "parses BooleanValue true" do
        (runParser "true" value) `shouldEqual` (Right (BooleanValue true))

      it "parses BooleanValue false" do
        (runParser "false" value) `shouldEqual` (Right (BooleanValue false))

      it "parses EnumValue" do
        (runParser "thisisanenum" value) `shouldEqual` (Right (EnumValue ("thisisanenum")))

      it "parses NullValue" do
        (runParser "null" value) `shouldEqual` (Right NullValue)

      it "parses ListValue" do
        (runParser "[asdf true \"Hello, World!\", 10, 15.9]" value) `shouldEqual` (Right (ListValue [(EnumValue "asdf"), (BooleanValue true), (StringValue "Hello, World!"), (IntValue 10), (FloatValue 15.9) ]))
      
      it "parses an empty list" do 
        (runParser "[]" value) `shouldEqual` (Right (ListValue []))

      it "parses ObjectValue" do
        (runParser "{ foo:\"Hello, World!\" bar:[true false null]}" value) `shouldEqual` (
          Right (ObjectValue [
            (ObjectField "foo" (StringValue "Hello, World!")) 
          , (ObjectField "bar" (ListValue [(BooleanValue true), (BooleanValue false), NullValue]))
          ]
         ))

    describe "Alias" do 
      it "parses the common case" do
        (runParser "alias:" alias) `shouldEqual` (Right (Alias "alias"))
      it "parses with some ignored tokens" do
        (runParser "alias,,,  :" alias) `shouldEqual` (Right (Alias "alias"))
      it "passes quickCheck" do
        quickCheck (\(ArbitraryAlias a @ (Alias n)) -> (runParser (toQueryString a) alias )=== (Right (Alias n)))

    describe "Argument" do
      it "passes quickCheck" do
        quickCheck (\(ArbitraryArgument arg @ (Argument n v)) -> (runParser (toQueryString arg) argument) === (Right (Argument n v)))

    describe "Arguments" do     
      it "should fail on empty arguments" do 
        (runParser "()" arguments) `shouldEqual` (Left (ParseError "empty list of arguments aren't allowed" (Position {line: 1, column: 3})))
      it "passes quickCheck" do
        quickCheck (\ arbArgs @ (ArbitraryArguments (Arguments args)) -> (runParser (toQueryString (Arguments args)) arguments) === (Right (Arguments args)))

    describe "Directive" do
      it "passes quickCheck" do
        quickCheck (\(ArbitraryDirective d) -> (runParser (toQueryString d) directive) === (Right d))

    describe "Directives" do
      it "passes quickCheck" do
        quickCheck (\(ArbitraryDirectives (Directives ds)) -> (runParser (toQueryString (Directives ds)) directives) === (Right (Directives ds)))

    describe "GQLType" do
      it "parses NamedType" do
        (runParser "Dustin" gqlType) `shouldEqual` (Right (NamedType "Dustin")) 
      it "parses NonNullType - NamedType" do
        (runParser "Dustin!" gqlType) `shouldEqual` (Right (NonNullType (NonNullNamed "Dustin")))
      it "parse ListType" do
        (runParser "[Dustin]" gqlType) `shouldEqual` (Right (ListType (NamedType "Dustin")))
      it "passes quickCheck" do
       quickCheck (\(ArbitraryGQLType t) -> (runParser (toQueryString t) gqlType) === (Right t))

    describe "DefaultValue" do
      it "passes quickCheck" do
       quickCheck (\(ArbitraryDefaultValue d) -> (runParser (toQueryString d) defaultValue) === (Right d))

    describe "TypeCondition" do
      it "passes quickCheck" do
       quickCheck (\(ArbitraryTypeCondition t) -> (runParser (toQueryString t) typeCondition) === (Right t))

    describe "FragmentName" do
      it "fails on 'on'" do
        (runParser "on" fragmentName) `shouldEqual` (Left (ParseError "Did not expect to find 'on'" (Position {line: 1, column: 3})))
      it "passes quickCheck" do
       quickCheck (\(ArbitraryFragmentName t) -> (runParser (toQueryString t) fragmentName) === (Right t))

    describe "FragmentSpread" do
      it "passes quickCheck" do
       quickCheck' 5 (\(ArbitraryFragmentSpread t) -> (runParser (toQueryString t) fragmentSpread) === (Right t))

    describe "Field" do 
      it "passes quickCheck" do
        quickCheck' 5 (\(ArbitraryField f ) -> (runParser (toQueryString f) field) === (Right f))

    describe "Selection" do
      it "passes quickCheck" do
        quickCheck' 5 (\(ArbitrarySelection s) -> (runParser (toQueryString s) selection) === (Right s))


    describe "SelectionSet" do
      it "passes quickCheck" do
        quickCheck' 5 (\(ArbitrarySelectionSet s) -> (runParser (toQueryString s) selectionSet) === (Right s))

    describe "InlineFragment" do
      it "passes quickCheck" do
        quickCheck' 5 (\(ArbitraryInlineFragment f) -> (runParser (toQueryString f) inlineFragment) === (Right f))

    describe "VariableDefinition" do
      it "passes quickCheck" do
        quickCheck' 5 (\(ArbitraryVariableDefinition d) -> (runParser (toQueryString d) variableDefinition) === (Right d))
    describe "VariableDefinitions" do
      it "passes quickCheck" do
        quickCheck (\(ArbitraryVariableDefinitions ds) -> (runParser (toQueryString ds) variableDefinitions) === (Right ds))

    describe "Definition" do
      it "passes quickCheck" do
        --quickCheck' 5 (\(ArbitraryDefinition d) -> (runParser (toQueryString d) definition) === (Right d))
-}
    describe "Document" do
      it "parses the Kitchen Sink query" do 
        (spy (runParser query1 document)) `shouldEqual` (Left (ParseError "Did not expect to find 'on'" (Position {line: 1, column: 3})))

      it "parses the Kitchen Sink query" do 
        (spy (runParser kitchenSink document)) `shouldEqual` (Left (ParseError "Did not expect to find 'on'" (Position {line: 1, column: 3})))


strReverse :: String -> String
strReverse str = foldMap singleton (reverse $ toCharArray str)
