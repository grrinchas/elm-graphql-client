module AST exposing (..)

type alias Name = String
type alias Comment = String
type alias Alias = Name
type alias Variable = Name
type alias DefaultValue = Value
type alias Queries = List Definition


type alias OperationTypeDefinition =
    { operationType: OperationType
    , name : Maybe Name
    , variables: List VariableDefinition
    , directives: List Directive
    , selections: List Selection
    }

type alias Field =
    { alias_ : Maybe Alias
    , name : Name
    , arguments: List Argument
    , directives: List Directive
    , selections: List Selection
    }


type alias ObjectField =
    { name: Name
    , value: Value
    }


type alias Argument =
    { name: Name
    , value: Value
    }


type alias Directive =
    { name: Name
    , arguments: List Argument
    }


type alias VariableDefinition =
    { variable: Variable
    , type_: Type
    , default: Maybe DefaultValue
    }


type alias FragmentSpread =
    { name: Name
    , directives: List Directive
    }


type alias Fragment =
    { name: Name
    , type_: Type
    , directives: List Directive
    , selections: List Selection
    }


type alias InlineFragment =
    { type_: Maybe Type
    , directives: List Directive
    , selections: List Selection
    }


type Definition
    = SelectionDefinition (List Selection)
    | OperationDefinition OperationTypeDefinition
    | FragmentDefinition Fragment


type OperationType
    = Query
    | Mutation


type Value
    = VariableValue Name
    | IntValue Int
    | FloatValue Float
    | StringValue String
    | BooleanValue Bool
    | NullValue
    | EnumValue Name
    | ListValue (List Value)
    | ObjectValue (List ObjectField)


type Selection
    = FieldSelection Field
    | FragmentSpreadSelection FragmentSpread
    | InlineFragmentSelection InlineFragment


type Type
    = NamedType Name
    | ListType Type
    | NotNullNamed Name
    | NotNullList Type


toOperationDefinition: Definition -> Maybe OperationTypeDefinition
toOperationDefinition def =
    case def of
        SelectionDefinition _ -> Nothing
        FragmentDefinition _ -> Nothing
        OperationDefinition maybeOp ->
            Just maybeOp


toFragmentDefinition: Definition -> Maybe Fragment
toFragmentDefinition def =
    case def of
        SelectionDefinition _ -> Nothing
        OperationDefinition _ -> Nothing
        FragmentDefinition fragment ->
            Just fragment


getOperation: Name ->  Queries -> Maybe OperationTypeDefinition
getOperation name doc =
    let filterName = \name_ -> case name_ of
            Nothing -> False
            Just n -> n == name
    in
    List.map toOperationDefinition doc
        |> List.filter (\op -> Maybe.map (\op -> filterName op.name) op |> Maybe.withDefault False)
        |> List.head
        |> Maybe.withDefault Nothing


getFragment: Name ->  Queries -> Maybe Fragment
getFragment name doc =
    List.map toFragmentDefinition doc
        |> List.filter (\op -> Maybe.map (\op -> name == op.name) op |> Maybe.withDefault False)
        |> List.head
        |> Maybe.withDefault Nothing


getAllFragments: Queries -> Selection -> List Fragment
getAllFragments doc sel =
    case sel of
        FieldSelection field ->
            field.selections
                |> List.concatMap (getAllFragments doc)
                |> removeDuplicates

        FragmentSpreadSelection spread ->
            case getFragment spread.name doc of
                Nothing -> []

                Just fragment ->
                    List.concatMap (getAllFragments doc) fragment.selections
                        |> (::) fragment
                        |> removeDuplicates

        InlineFragmentSelection _ -> []


removeDuplicates: List a -> List a
removeDuplicates =
    List.foldr (\x list -> if List.member x list then list else (::) x list) []


-- Schema

type alias ScalarType = Type
type alias Schema = List SchemaDefinition

type alias UnionType =
    { type_: Type
    , values: List Type
    }


type alias EnumType =
    { type_: Type
    , values: List Name
    }


type alias ObjectType =
    { name: Name
    , interfaces: List Type
    , fields: List FieldDefinition
    }


type alias InterfaceType =
    { name: Name
    , fields: List FieldDefinition
    }


type alias InputType =
    { name: Name
    , fields: List InputValueDefinition
    }


type alias InputValueDefinition =
    { name: Name
    , type_: Type
    , default: Maybe Value
    }


type alias FieldDefinition =
    { name: Name
    , arguments: List InputValueDefinition
    , type_: Type
    }


type SchemaDefinition
    = ObjectTypeDefinition ObjectType
    | InterfaceTypeDefinition InterfaceType
    | UnionTypeDefinition UnionType
    | ScalarTypeDefinition ScalarType
    | EnumTypeDefinition EnumType
    | InputTypeDefinition InputType
    | ExtendedTypeDefinition ObjectType




