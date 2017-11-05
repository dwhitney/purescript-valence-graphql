module Valence.Query.AST where

import Prelude

import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty)

-- Misc typeclasses
class QueryString a where
  toQueryString :: a -> String

-- Top Level

newtype Document = Document (NonEmpty Array Definition) 

instance showDocument :: Show Document where
  show (Document defs) = "(Document " <> show defs <> ")"

instance eqDocument :: Eq Document where
  eq (Document defs1) (Document defs2) = defs1 == defs2

instance queryStringDocument :: QueryString Document where
  toQueryString (Document defs) = foldMap (\def -> toQueryString def <> " ") defs

data Definition 
  = Operation OperationType (Maybe String) (Maybe VariableDefinitions) (Maybe Directives) SelectionSet
  | Fragment FragmentName TypeCondition (Maybe Directives) SelectionSet

instance showDefinition :: Show Definition where
  show (Operation opType maybeName maybeVariableDefinitions maybeDirectives ss) = 
    "(Operation " <> showWithSpace maybeName <> showWithSpace maybeVariableDefinitions <> showWithSpace maybeDirectives <> show ss <> ")"
  show (Fragment fn tc dir ss) = 
    "(Fragment " <> show fn <> " " <> show tc <> " " <> show dir <> " " <> show ss <> ")"

instance eqDefinition :: Eq Definition where
  eq (Operation o1 n1 vd1 d1 ss1) (Operation o2 n2 vd2 d2 ss2) =
    (o1 == o2) && (n1 == n2) && (vd1 == vd2) && (d1 == d2) && (ss1 == ss2)
  eq (Fragment fn1 tc1 dir1 ss1) (Fragment fn2 tc2 dir2 ss2) = 
    (fn1 == fn2) && (tc1 == tc2) && (dir1 == dir2) && (ss1 == ss2)   
  eq _ _ = false

instance queryStringDefinition :: QueryString Definition where
  toQueryString (Operation opType maybeName maybeVariableDefinitions maybeDirectives ss) = 
    toQueryString opType <> " " <> (fromMaybe "" maybeName) <> " " <> queryStringWithSpace maybeVariableDefinitions <> toQueryString ss
  toQueryString (Fragment fn tc maybeDir ss) = 
    "fragment " <> toQueryString fn <> " " <> queryStringWithSpace maybeDir <> " " <> toQueryString ss

data OperationType = Query | Mutation

instance showOperationType :: Show OperationType where
  show Query = "Query"
  show Mutation = "Mutation"

instance eqOperationType :: Eq OperationType where
  eq Query Query = true
  eq Mutation Mutation = true
  eq _ _ = false

instance queryStringOperationType :: QueryString OperationType where
  toQueryString Query = "query"
  toQueryString Mutation = "mutation"

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment

instance showSelection :: Show Selection where
  show (SelectionField field) = "(SelectionField " <> (show field) <> ")"
  show (SelectionFragmentSpread spread) = "(SelectionFragmentSpread " <> (show spread) <> ")"
  show (SelectionInlineFragment inline) = "(SelectionInlineFragment " <> (show inline) <> ")"

instance eqSelection :: Eq Selection where
  eq (SelectionField field1) (SelectionField field2) = field1 == field2
  eq (SelectionFragmentSpread spread1) (SelectionFragmentSpread spread2) = spread1 == spread2 
  eq (SelectionInlineFragment inline1) (SelectionInlineFragment inline2) = inline1 == inline2 
  eq _ _ = false

instance queryStringSelection :: QueryString Selection where
  toQueryString (SelectionField field) = toQueryString field
  toQueryString (SelectionFragmentSpread spread) = toQueryString spread
  toQueryString (SelectionInlineFragment inline) = toQueryString inline

newtype SelectionSet = SelectionSet (NonEmpty Array Selection)

instance showSelectionSet :: Show SelectionSet where
  show (SelectionSet set) = "(SelectionSet " <> (show set) <> ")"

instance eqSelectionSet :: Eq SelectionSet where
  eq (SelectionSet set1)  (SelectionSet set2) = set1 == set2

instance queryStringSelectionSet :: QueryString SelectionSet where
  toQueryString (SelectionSet set) = "{" <> (foldMap (\s -> (toQueryString s) <> " ") set) <> "}"

-- Fields

data Field = Field (Maybe Alias) String (Maybe Arguments) (Maybe Directives) (Maybe SelectionSet)

instance showField :: Show Field where
  show (Field maybeAlias n maybeArguments maybeDirectives maybeSelectionSet) = 
    "(Field " <> 
      showWithSpace maybeAlias <>
      n <> " " <>
      showWithSpace maybeArguments <>
      showWithSpace maybeDirectives <>
      showWithSpace maybeSelectionSet <>
      ")" 

showWithSpace :: ∀ a. Show a => (Maybe a) -> String
showWithSpace m = fromMaybe "" (m <#> (\a -> (show a) <> " "))

queryStringWithSpace :: ∀ a. QueryString a => (Maybe a) -> String
queryStringWithSpace m = fromMaybe "" (m <#> (\a -> (toQueryString a) <> " "))

instance eqField :: Eq Field where
  eq (Field alias1 name1 args1 directives1 selectionSet1) (Field alias2 name2 args2 directives2 selectionSet2) = 
    (alias1 == alias2) && (name1 == name2) && (args1 == args2) && (directives1 == directives2) && (selectionSet1 == selectionSet2)

instance queryStringField :: QueryString Field where
  toQueryString (Field aliasOpt n argsOpt directivesOpt selectionSetOpt) = 
    fold [
      queryStringWithSpace aliasOpt
     , (n <> " ")
     , queryStringWithSpace argsOpt
     , queryStringWithSpace directivesOpt
     , queryStringWithSpace selectionSetOpt
    ]

newtype Alias = Alias String

instance showAliaas :: Show Alias where
  show (Alias a) = "(Alias " <> a <> ")"

instance aliasQueryString :: QueryString Alias where
  toQueryString (Alias a) = a <> ":"

instance eqAlias :: Eq Alias where
  eq (Alias a1) (Alias a2)  = a1 == a2

data Argument = Argument String Value

instance showArgument :: Show Argument where
  show (Argument n v) = "(Argument " <> n <> " " <> (show v) <> ")"

instance argumentQueryString ::  QueryString Argument where
  toQueryString (Argument n v ) = n <> ":" <> (toQueryString v)

instance eqArgument :: Eq Argument where
  eq (Argument n1 v1) (Argument n2 v2) = (n1 == n2) && (v1 == v2)

newtype Arguments = Arguments (Array Argument)

instance showArguments :: Show Arguments where
  show (Arguments args) = "(Arguments " <> (foldMap show args) <> ")"

instance eqArguments :: Eq Arguments where
  eq (Arguments args1) (Arguments args2) = args1 == args2

instance queryStringArguments :: QueryString Arguments where
  toQueryString (Arguments args) = "(" <> (foldMap (\arg -> (toQueryString arg) <> " ") args) <> ")"

-- Fragments

data FragmentSpread = FragmentSpread FragmentName (Maybe Directives)

instance showFragmentSpread :: Show FragmentSpread where
  show (FragmentSpread n (Just d)) = "(FragmentSpread " <> (show n) <> " " <> (show d) <> ")"
  show (FragmentSpread n Nothing) = "FragmentSpread " <> (show n) <> ")"

instance eqFragmentSpread :: Eq FragmentSpread where
  eq (FragmentSpread n1 d1) (FragmentSpread n2 d2) = (n1 == n2) && (d1 == d2)

instance queryStringFragmentSpread :: QueryString FragmentSpread where
  toQueryString (FragmentSpread n (Just directives)) = "... " <> (toQueryString n) <> " " <> (toQueryString directives)
  toQueryString (FragmentSpread n Nothing) = "... " <> (toQueryString n) 

newtype FragmentName = FragmentName String

instance showFragmentName :: Show FragmentName where
  show (FragmentName n) = "(FragmentName " <> n <> ")"

instance eqFragmentName :: Eq FragmentName where
  eq (FragmentName c1) (FragmentName c2) = c1 == c2

instance queryStringFragmentName :: QueryString FragmentName where
  toQueryString (FragmentName n) = n

data InlineFragment = InlineFragment (Maybe TypeCondition) (Maybe Directives) SelectionSet

instance showInlineFragment :: Show InlineFragment where
  show (InlineFragment (Just tc) (Just d) ss) = "(InlineFragment" <> (show tc) <> " " <> (show d) <> " " <> (show ss) <> ")"
  show (InlineFragment Nothing (Just d) ss) = "(InlineFragment" <> " " <> (show d) <> " " <> (show ss) <> ")"
  show (InlineFragment (Just tc) Nothing ss) = "(InlineFragment" <> (show tc) <> (show ss) <> ")"
  show (InlineFragment Nothing Nothing ss) = "(InlineFragment" <> (show ss) <> ")"

instance eqInlineFragment :: Eq InlineFragment where
  eq (InlineFragment tc1 d1 ss1) (InlineFragment tc2 d2 ss2) =
    (tc1 == tc2) && (d1 == d2) && (ss1 == ss2)

instance queryStringInlineFragment :: QueryString InlineFragment where
  toQueryString (InlineFragment tc d ss) = 
    "... " <> (queryStringWithSpace tc) <> (queryStringWithSpace d) <> (toQueryString ss)

-- Values

data Value
  = Variable String
  | IntValue Int
  | FloatValue Number
  | StringValue String
  | BooleanValue Boolean
  | NullValue
  | EnumValue String
  | ListValue (Array Value)
  | ObjectValue (Array ObjectField)

instance valueShow :: Show Value where
  show (Variable v) = "(Variable " <> (show v) <> ")"
  show (IntValue num) = "(IntValue " <> (show num) <> ")"
  show (FloatValue num) = "(FloatValue " <> (show num) <> ")"
  show (StringValue str) = "(StringValue " <> (show str) <> ")"
  show (BooleanValue b) = "(BooleanValue " <> (show b) <> ")"
  show NullValue = "(NullValue)"
  show (EnumValue enum) = "(EnumValue " <> (show enum) <> ")"
  show (ListValue v) = "(ListValue " <> (show v) <> ")"
  show (ObjectValue fields) = "(ObjectValue " <> (show fields) <> ")"

instance valueEQ :: Eq Value where
  eq (Variable v1) (Variable v2) = v1 == v2 
  eq (IntValue num1) (IntValue num2) = num1 == num2
  eq (FloatValue num1) (FloatValue num2) = num1 == num2
  eq (StringValue str1) (StringValue str2) = str1 == str2
  eq (BooleanValue b1) (BooleanValue b2) = b1 == b2
  eq (NullValue) (NullValue) = true
  eq (EnumValue enum1) (EnumValue enum2) = enum1 == enum2 
  eq (ListValue v1) (ListValue v2) = v1 == v2 
  eq (ObjectValue fields1) (ObjectValue fields2) = fields1 == fields2
  eq _ _ = false

instance valueQueryString :: QueryString Value where
  toQueryString (Variable v) = "$" <> v 
  toQueryString (IntValue num) = show num
  toQueryString (FloatValue num) = show num
  toQueryString (StringValue str) = "\"" <> str <> "\""
  toQueryString (BooleanValue b) = show b
  toQueryString NullValue = "null"
  toQueryString (EnumValue enum) = enum
  toQueryString (ListValue vals) = "[" <> (foldMap (\v -> (toQueryString v) <> " ") vals) <> "]"
  toQueryString (ObjectValue fields) = "{" <> (foldMap (\field -> (toQueryString field) <> " ") fields) <> "}"


data ObjectField = ObjectField String Value

instance objectFieldShow :: Show ObjectField where
  show (ObjectField n v) = "(ObjectField (Name " <> n <> ") " <> (show v) <> ")"

instance objectFieldEq :: Eq ObjectField where
  eq (ObjectField n1 v1) (ObjectField n2 v2) = (n1 == n2) && (v1 == v2)

instance objectFieldQueryString :: QueryString ObjectField where
  toQueryString (ObjectField n v ) = 
    n <> ":" <> (toQueryString v) 

type ObjectFields = Array ObjectField

-- Types

data GQLType
  = NamedType String
  | ListType GQLType
  | NonNullType NonNull

instance showGQLType :: Show GQLType where
  show (NamedType n) = "(NamedType " <> n <> ")"
  show (ListType arr) = "(ListType "  <> (show arr) <> ")"
  show (NonNullType n) = "(NonNullType " <> (show n) <> ")"

instance eqGQLType :: Eq GQLType where
  eq (NamedType n1) (NamedType n2) = n1 == n2
  eq (ListType n1) (ListType n2) = n1 == n2
  eq (NonNullType n1) (NonNullType n2) = n1 == n2
  eq _ _ = false

instance queryStringGQLType :: QueryString GQLType where
  toQueryString (NamedType n) = n
  toQueryString (ListType n) = "[" <> (toQueryString n) <> "]"
  toQueryString (NonNullType n) = toQueryString n

data NonNull
  = NonNullNamed String
  | NonNullList GQLType

instance showNonNull :: Show NonNull where
  show (NonNullNamed n) = "(NonNullNamed " <> n <> ")"
  show (NonNullList n) = "(NonNullList " <> (show n) <> ")"

instance eqNonNull :: Eq NonNull where
  eq (NonNullNamed n1) (NonNullNamed n2) = n1 == n2
  eq (NonNullList n1) (NonNullList n2) = n1 == n2
  eq _ _ = false

instance queryStringNonNull :: QueryString NonNull where
  toQueryString (NonNullNamed n) = n <> "!"
  toQueryString (NonNullList n) = "[" <> (toQueryString n) <> "]!"

newtype TypeCondition = TypeCondition String

instance showTypeCondition :: Show TypeCondition where
  show (TypeCondition n) = "(TypeCondition " <> n <> ")"

instance eqTypeCondition :: Eq TypeCondition where
  eq (TypeCondition c1) (TypeCondition c2) = c1 == c2

instance queryStringTypeCondition :: QueryString TypeCondition where
  toQueryString (TypeCondition n) = "on " <> n

-- Variables

data VariableDefinition = VariableDefinition String GQLType (Maybe DefaultValue)

instance showVariableDefinition :: Show VariableDefinition where
  show (VariableDefinition n t md) = 
    "(VariableDefinition " <> n <> (show t) <> showWithSpace md <> ")"

instance eqVariableDefinition :: Eq VariableDefinition where
  eq (VariableDefinition n1 t1 d1) (VariableDefinition n2 t2 d2) = (n1 == n2) && (t1 == t2) && (d1 == d2)

instance queryStringVariableDefinition :: QueryString VariableDefinition where
  toQueryString (VariableDefinition n t md) = "$" <> n <> ": " <> toQueryString t <> " " <> queryStringWithSpace md

newtype VariableDefinitions = VariableDefinitions (NonEmpty Array VariableDefinition)

instance showVariableDefinitions :: Show VariableDefinitions where
  show (VariableDefinitions v) = "(VariableDefinitions " <> show v <> ")"

instance eqVariableDefinitions :: Eq VariableDefinitions where
  eq (VariableDefinitions v1) (VariableDefinitions v2) = v1 == v2

instance queryStringVariableDefinitions :: QueryString VariableDefinitions where
  toQueryString (VariableDefinitions vs) = "(" <> (foldMap (\v -> toQueryString v <> " ") vs) <> ")"

newtype DefaultValue = DefaultValue Value

instance showDefaultValue :: Show DefaultValue where
  show (DefaultValue v) = "(DefaultValue " <> (show v) <> ")"

instance eqDefaultValue :: Eq DefaultValue where
  eq (DefaultValue v1) (DefaultValue v2) = v1 == v2

instance queryStringDefaultValue :: QueryString DefaultValue where
  toQueryString (DefaultValue v) = "= " <> (toQueryString v)

-- Directives

data Directive = Directive String (Maybe Arguments)

instance showDirective :: Show Directive where
  show (Directive name args) = "(Directive " <> name <> (show args) <> ")"

instance eqDirective :: Eq Directive where
  eq (Directive n1 args1) (Directive n2 args2) = (n1 == n2) && (args1 == args2)

instance queryStringDirective :: QueryString Directive where
  toQueryString (Directive n (Just args)) = "@" <> n <> " " <> (toQueryString args)
  toQueryString (Directive n Nothing) = "@" <> n 

newtype Directives = Directives (NonEmpty Array Directive)

instance showDirectives :: Show Directives where
  show (Directives ds) = "(Directives " <> (show ds) <> ")"

instance eqDirectives :: Eq Directives where
  eq (Directives ds1) (Directives ds2) = ds1 == ds2

instance queryStringDirectives :: QueryString Directives where
  toQueryString (Directives ds) = foldMap (\d -> (toQueryString d) <> " ") ds


