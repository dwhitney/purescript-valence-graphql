module Valence.Query.AST where


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

data ObjectField = ObjectField String Value

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


