module Valence.Query.Parser where

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (elem, fold, many)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.String (fromCharArray, singleton)
import Global (readInt, readFloat)
import Prelude hiding (between)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, lookAhead, manyTill, option, optionMaybe, skipMany, try)
import Text.Parsing.Parser.String (char, eof, oneOf, satisfy, string)
import Text.Parsing.Parser.Token (alphaNum, digit, letter)
import Valence.Query.AST as AST


--| AST 

document :: Parser String AST.Document
document = AST.Document <$> definitions

definitions :: Parser String AST.Definitions
definitions = do
  d   <- definition
  ds  <- many definition
  pure $ (d :| ds)

definition :: Parser String AST.Definition
definition = operation <|> fragment
  where
    operation = simpleOperation <|> normalOperation
    simpleOperation = selectionSet <#> (\set -> (AST.Operation AST.Query Nothing Nothing [] set))
    normalOperation = AST.Operation <$> operationType <*> (optionMaybe name) <*> (optionMaybe variableDefinitions) <*> directives <*> selectionSet
    operationType = (AST.Query <$ (nameMatcher "query")) <|> (AST.Mutation <$ (nameMatcher "mutation"))
    fragment = do
      _   <- nameMatcher "fragment"
      fn  <- fragmentName
      tc  <- typeCondition
      ds  <- directives
      ss  <- selectionSet    
      pure (AST.Fragment fn tc ds ss)

selectionSet :: Parser String AST.SelectionSet 
selectionSet = fix (\parser -> between lCurlyBracket rCurlyBracket (do
  s   <- selection
  ss  <- many selection
  pure (s :| ss)))

optionalSelectionSet :: Parser String AST.OptionalSelectionSet
optionalSelectionSet = fix (\parser -> between lCurlyBracket rCurlyBracket (many selection))

selection :: Parser String AST.Selection
selection = fix (\parser -> selectionField parser <|> selectionFragmentSpread <|> (selectionInlineFragment parser))
  where
    selectionField parser = AST.SelectionField <$> field parser
    selectionFragmentSpread = AST.SelectionFragmentSpread <$> fragmentSpread 
    selectionInlineFragment parser = AST.SelectionInlineFragment <$> inlineFragment 
    field parser = AST.Field <$> 
                    (optionMaybe (try alias)) <*> 
                    name <*> 
                    (optionMaybe ( arguments)) <*> 
                    (optionMaybe ( directives)) <*> 
                    (optionMaybe ( selectionSet))


alias :: Parser String AST.Alias
alias = AST.Alias <$> name <* colon

arguments :: Parser String AST.Arguments
arguments = between lParen rParen (many argument)

argument :: Parser String AST.Argument
argument = do
  n <- name
  _ <- colon
  v <- value
  pure (AST.Argument n v)

value :: Parser String AST.Value
value = fix (\parser ->
    try variable            <|>
    try fValue              <|>
    try iValue              <|>
    try sValue              <|>
    try booleanValue        <|>
    try nullValue           <|>
    try enumValue           <|>
    try (listValue parser)  <|>
    try (objectValue parser))
  where
    iValue                = AST.IntValue      <$> (intValue <#> readInt 10)
    fValue                = AST.FloatValue    <$> (floatValue <#> readFloat)
    sValue                = AST.StringValue   <$> stringValue
    booleanValue          = AST.BooleanValue  <$> (try (nameMatcher "true" $> true) <|> try (nameMatcher "false" $> false))
    enumValue             = AST.EnumValue     <$> notNameMatcher ["true", "false", "null"]
    listValue valueParser = AST.ListValue     <$> (between lBracket rBracket (many valueParser))
    objectValue parser    = AST.ObjectValue   <$> (between lCurlyBracket rCurlyBracket (many $ (objectField parser)))
    variable              = AST.Variable      <$> variableStr
    objectField parser    = AST.ObjectField   <$> (name <* colon) <*> value
    nullValue             = AST.NullValue     <$  nameMatcher "null"
 
variableStr :: Parser String String
variableStr = dollarSign *> name

fragmentSpread :: Parser String AST.FragmentSpread
fragmentSpread = AST.FragmentSpread <$> (ellipses *> fragmentName) <*> directives

inlineFragment :: Parser String AST.InlineFragment
inlineFragment = fix (\parser -> 
  AST.InlineFragment <$> (ellipses *> (optionMaybe typeCondition)) <*> directives <*> selectionSet)

fragmentName :: Parser String AST.FragmentName
fragmentName = AST.FragmentName <$> notNameMatcher ["on"]

typeCondition :: Parser String AST.TypeCondition
typeCondition = AST.TypeCondition <$> ((nameMatcher "on") *> name)

directives :: Parser String AST.Directives
directives = many directive

directive :: Parser String AST.Directive
directive = AST.Directive <$> (at *> name) <*> arguments

variableDefinition :: Parser String AST.VariableDefinition
variableDefinition = AST.VariableDefinition <$> (dollarSign *> name <* colon) <*>  gqlType <*> (optionMaybe defaultValue)

variableDefinitions :: Parser String AST.VariableDefinitions
variableDefinitions = between lParen rParen do
  v <- variableDefinition
  vs <- many variableDefinition
  pure (v :| vs)

defaultValue :: Parser String AST.DefaultValue
defaultValue = AST.DefaultValue <$> (equals *> value)

gqlType :: Parser String AST.GQLType
gqlType = fix (\parser ->  namedType <|> listType parser <|> nonNullType parser)
  where
    namedType = AST.NamedType <$> name
    listType parser = between lBracket rBracket gqlType
    nonNullType parser = AST.NonNullType <$> (nonNullNamed <|> nonNullList parser)
    nonNullNamed = AST.NonNullNamed <$> (name <* exclamation)
    nonNullList parser = AST.NonNullList <$> ((listType parser) <* exclamation)

nameMatcher :: String -> Parser String String 
nameMatcher str = name >>= (\n -> 
  if n == str 
  then pure n   
  else fail ("Expected " <> str <> " but found " <> n))

notNameMatcher :: Array String -> Parser String String
notNameMatcher badNames = name >>= (\str ->
  if elem str badNames
  then fail ("Did not expect to find '" <> str <> "'")
  else pure str)

--| Lexical "tokens"

type TokenParser = Parser String String

lexicalTokens :: Parser String (Array String)
lexicalTokens = do
  _      <- skipMany ignoredTokens
  tokens <- many (punctuator <|> name <|> floatValue <|> intValue  <|> stringValue) 
  _      <- eof 
  pure tokens

ignoredTokens :: Parser String String
ignoredTokens = unicodeBOM <|> whiteSpace <|> lineTerminator <|> comment <|> comma

lexicalToken :: TokenParser -> TokenParser
lexicalToken parser = (parser <* (skipMany ignoredTokens)) -- <#> spy

name :: TokenParser
name = lexicalToken $ ((singleton <$> (letter <|> (char '_'))) <> (many (alphaNum <|> (char '_')) <#> foldMap singleton))

intValue :: TokenParser
intValue = lexicalToken integerPart
  
integerPart :: Parser String String 
integerPart = ((try negativeZero) <|> (try normalInt))
  where
     
    negativeZero          = optionalNegativeSign <> (string "0")
    normalInt             = optionalNegativeSign <> nonZeroDigit <> digits
    
    optionalNegativeSign  = option "" (string "-")
    nonZeroDigit          = singleton <$> satisfy (\c -> c >= '1' && c <= '9')

floatValue :: TokenParser
floatValue = lexicalToken floatParser
  where
    floatParser       = (try intFracExp) <|> (try intExp) <|> (try intFrac)
    
    intFracExp        = integerPart <> fractionalPart <> exponentPart 
    intExp            = integerPart <> exponentPart
    intFrac           = integerPart <> fractionalPart 
   
    optionalSign      = option "" (string "+" <|> string "-")
    exponentPart      = exponentIndicator <> optionalSign <> digits
    exponentIndicator = singleton <$> oneOf ['e', 'E']
    fractionalPart    = (string ".") <> digits

digits :: Parser String String
digits =  fold <$> (many (singleton <$> digit)) 

stringValue :: TokenParser
stringValue = lexicalToken $ do
  _    <- quote
  strs <- many ((try stringCharacter) <|> (try escapedCharacter) <|> (try escapedUnicode))
  _    <- quote
  pure $ fold strs
    where
      quote = string "\""
      stringCharacter = singleton <$> (satisfy (\c ->
        (not (elem c ['"','\\','\n','\r'])) && 
        ((c >= '\x0020' && c <= '\xFFFF') || (c == '\t'))
      ))
      
      escapedCharacter = do
        _ <- char '\\'
        c <- oneOf ['"', '\\', '/', '\b', '\f', '\n', '\r', '\t']
        pure $ fromCharArray ['\\', c]
      
      --escapedCharacter = try (string "\\\\") <|> try (string "\\\"") <|> try (string "\\/") <|> try (string "\\b") <|> try (string "\\f") <|> 
      --  try (string "\\n") <|> try (string "\\r") <|> try (string "\\t")


      escapedUnicode = do
        _  <- char '\\'
        _  <- char 'u'
        u1 <- unicodeChar
        u2 <- unicodeChar
        u3 <- unicodeChar
        u4 <- unicodeChar
        pure $ fromCharArray ['\\', 'u', u1, u2, u3, u4]
        where
          unicodeChar = satisfy (\c ->
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

punctuator :: TokenParser
punctuator = punctuation 
  where
    punctuation = 
      exclamation     <|>
      dollarSign      <|>
      lParen          <|>
      rParen          <|>
      colon           <|>
      equals          <|>
      at              <|>
      lBracket        <|>
      rBracket        <|>
      lCurlyBracket   <|>
      rCurlyBracket   <|>
      pipe            <|>
      ellipses

exclamation :: TokenParser
exclamation = lexicalToken $ string "!"

dollarSign :: TokenParser
dollarSign = lexicalToken $ string "$"

lParen :: TokenParser
lParen = lexicalToken $ string "("

rParen :: TokenParser
rParen = lexicalToken $ string ")"

colon :: TokenParser
colon = lexicalToken $ string ":"

equals :: TokenParser
equals = lexicalToken $ string "="

at :: TokenParser
at = lexicalToken $ string "@"

lBracket :: TokenParser
lBracket = lexicalToken $ string "["

rBracket :: TokenParser
rBracket = lexicalToken $ string "]"

lCurlyBracket :: TokenParser
lCurlyBracket = lexicalToken $ string "{"

rCurlyBracket :: TokenParser
rCurlyBracket = lexicalToken $ string "}"

pipe :: TokenParser
pipe = lexicalToken $ string "|"

ellipses :: TokenParser
ellipses = lexicalToken $ string "..."



--| Ignored "tokens"

type IgnoredTokenParser = Parser String Unit 

unicodeBOM :: Parser String String 
unicodeBOM = string "\xFEFF"

whiteSpace :: Parser String String 
whiteSpace = (string "\t" <|> string " ") 

lineTerminator :: Parser String String 
lineTerminator = newline <|> crnl <|> cr_not_nl
  where
    newline = string "\n"
    crnl = (string "\r\n")
    cr_not_nl = (string "\r") <> (singleton <$> (lookAhead $ satisfy (\c -> c /= '\n')))

comment :: Parser String String 
comment = ((string "#") <> commentChars)
  where
    commentChars = fold <$> manyTill sourceCharacter lineTerminator
    sourceCharacter = singleton <$> (tabAndTerminators <|> base)
      where 
        tabAndTerminators = oneOf ['\t', '\n', '\r']
        base              = satisfy (\c -> (c >= '\x0020' && c <= '\xFFFF'))

   
comma :: Parser String String 
comma = string ","


