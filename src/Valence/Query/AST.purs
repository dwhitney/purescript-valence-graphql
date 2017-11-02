module Valence.Query.AST where

import Prelude

import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)

-- Top Level

newtype Document = Document Definitions

type Definitions = NonEmpty Array Definition

data Definition 
  = Operation OperationType (Maybe String) (Maybe VariableDefinitions) Directives SelectionSet
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

data Argument = Argument String Value

type Arguments = Array Argument

-- Fragments

data FragmentSpread = FragmentSpread FragmentName Directives

newtype FragmentName = FragmentName String

data InlineFragment = InlineFragment (Maybe TypeCondition) Directives SelectionSet

-- Values

data Value
  = Variable String
  | IntValue Number
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

data ObjectField = ObjectField String Value

instance objectFieldShow :: Show ObjectField where
  show (ObjectField n v) = "(ObjectField (Name " <> n <> ") " <> (show v) <> ")"

instance objectFieldEq :: Eq ObjectField where
  eq (ObjectField n1 v1) (ObjectField n2 v2) = (n1 == n2) && (v1 == v2)

type ObjectFields = Array ObjectField

-- Types

data GQLType
  = NamedType String
  | ListType (Array GQLType)
  | NonNullType NonNull

data NonNull
  = NonNullNamed String
  | NonNullList GQLType

newtype TypeCondition = TypeCondition String

-- Variables

data VariableDefinition = VariableDefinition String GQLType (Maybe DefaultValue)

type VariableDefinitions = NonEmpty Array VariableDefinition

newtype DefaultValue = DefaultValue Value

-- Directives

data Directive = Directive String Arguments

type Directives = Array Directive


