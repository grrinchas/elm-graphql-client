module Encoder exposing (..)

import AST exposing (..)


document: Queries -> String
document doc =
    List.map definition doc
        |> String.join "\n\n"


operationTypeDefinition: OperationTypeDefinition -> String
operationTypeDefinition op =
    String.concat
        [ operationType op.operationType
        , Maybe.withDefault "" op.name
        , variableDefinitionSet op.variables
        , directiveSet op.directives
        , selectionSet op.selections
        ]


variableDefinitionSet: List VariableDefinition -> String
variableDefinitionSet vars =
    case vars of
        [] -> ""
        _ -> List.map variableDefinition vars
            |> (\list -> " (" ++ String.join ", " list ++ ") ")



variableDefinition: VariableDefinition -> String
variableDefinition var =
    String.concat
        [ "$" ++ var.variable ++ ": "
        , type_ var.type_
        , case var.default of
            Nothing -> ""
            Just val -> " = " ++ value val
        ]


variable: Variable -> String
variable var = "$" ++ var


nameValue: {name: String, value: Value} -> String
nameValue arg = arg.name ++ ": " ++ value arg.value


nameValueSet: List {name: Name, value: Value} -> String -> String -> String
nameValueSet arg open close =
    List.map nameValue arg
        |> (\list -> open ++ String.join ", " list ++ close)


argumentSet: List Argument -> String
argumentSet args =
    case args of
        [] -> ""
        list -> nameValueSet list "(" ")"


directive: Directive -> String
directive dir = " @" ++ dir.name ++ argumentSet dir.arguments


selectionSet: List Selection -> String
selectionSet list =
    case list of
        [] -> ""
        sel -> List.map selection sel
                |> (\sel ->" {" ++ String.join " " sel ++"} ")


directiveSet: List Directive -> String
directiveSet dir =
    case dir of
        [] -> ""
        dir -> List.map directive dir |> String.join " "


field: Field -> String
field field = String.concat
    [ case field.alias_ of
        Nothing -> ""
        Just al -> al ++ ": "
    , field.name
    , argumentSet field.arguments
    , directiveSet field.directives
    , selectionSet field.selections
    ]


fragmentSpread: FragmentSpread -> String
fragmentSpread s = "..." ++ s.name ++ directiveSet s.directives


typeCondition: Type -> String
typeCondition ty= " on " ++ type_ ty


fragment: Fragment ->  String
fragment f =
    String.concat
        [ " fragment "
        , f.name
        , typeCondition f.type_
        , directiveSet f.directives
        , selectionSet f.selections
        ]


inlineFragment: InlineFragment -> String
inlineFragment f =
    String.concat
        [ "..."
        , case f.type_ of
            Nothing -> ""
            Just t -> typeCondition t
        , directiveSet f.directives
        , selectionSet f.selections
        ]


type_ : Type -> String
type_ ty =
    case ty of
        NamedType name -> name
        ListType ty -> "[" ++ type_ ty ++ "]"
        NotNullNamed name -> name ++ "!"
        NotNullList list -> "[" ++ type_ ty ++ "]"


value: Value -> String
value val =
    case val of
        VariableValue var -> variable var
        BooleanValue bool -> toString bool
        FloatValue float -> toString float
        IntValue int -> toString int
        EnumValue enum -> enum
        NullValue -> "null"
        StringValue string -> string
        ListValue list ->
            List.map value list
                |> (\list -> " [" ++ String.join ", " list ++"] ")
        ObjectValue obj -> nameValueSet obj " {" "} "


selection: Selection -> String
selection sel =
    case sel of
        FieldSelection f -> field f
        FragmentSpreadSelection f -> fragmentSpread f
        InlineFragmentSelection f -> inlineFragment f


operationType: OperationType -> String
operationType op =
    case op of
        Query -> " query "
        Mutation -> " mutation "


definition: Definition -> String
definition def =
    case def of
        SelectionDefinition set -> selectionSet set
        OperationDefinition op -> operationTypeDefinition op
        FragmentDefinition frag -> fragment frag

