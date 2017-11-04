module Valence.Query.AST where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)

-- Misc typeclasses
class QueryString a where
  toQueryString :: a -> String

-- Top Level

newtype Document = Document Definitions

type Definitions = NonEmpty Array Definition

data Definition 
  = Operation OperationType (Maybe String) (Maybe VariableDefinitions) (Maybe Directives) SelectionSet
  | Fragment FragmentName TypeCondition Directives SelectionSet

data OperationType = Query | Mutation

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment


type SelectionSet = NonEmpty Array Selection

type OptionalSelectionSet = Array Selection

-- Fields

data Field = Field (Maybe Alias) String (Maybe Arguments) (Maybe Directives) (Maybe SelectionSet)

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
  show (FragmentSpread (FragmentName n) (Just d)) = ""
  show (FragmentSpread (FragmentName n) Nothing) = ""

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

data InlineFragment = InlineFragment (Maybe TypeCondition) Directives SelectionSet

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

type VariableDefinitions = NonEmpty Array VariableDefinition

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


