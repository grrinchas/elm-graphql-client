module Parser exposing (..)

import Combine exposing (..)
import AST exposing (..)
import Combine.Num exposing (float, int)


comment: Parser s Comment
comment = regex "#.*(\n|\r\n|\r)"


skipSpace: Parser s ()
skipSpace = skipMany (regex "," <|> comment <|> whitespace )


lex: Parser s c -> Parser s c
lex p = p <* skipSpace


token: String -> Parser s String
token s = lex (string s )


braces: Parser s c -> Parser s c
braces p = between (token "{") (token "}") p


parentheses: Parser s c -> Parser s c
parentheses p = between (token "(") (token ")") p


brackets: Parser s c -> Parser s c
brackets p = between (token "[") (token "]") p


operationType : Parser s OperationType
operationType = (token "query" $> Query) <|> (token "mutation" $> Mutation)


name: Parser s Name
name = lex <| regex "[_A-Za-z][_0-9A-Za-z]*"


typeCondition: Parser s Type
typeCondition = NamedType <$> (token "on" *> name)


stringValue: Parser s String
stringValue = lex (regex "(\"\"|\"([^\"\\(\n|\r\n|\r)]|\\u[0-9A-Fa-f]{4}|\"\\\b\f\n\r\t/)+\")") <?> "expected string"


variable: Parser s Variable
variable = token "$" *> name <?> "expected variable"


selectionSet: Parser s (List Selection)
selectionSet = braces (many1 <| lazy (\() -> selection))


directiveSet: Parser s (List Directive)
directiveSet = many1 directive


alias_: Parser s Alias
alias_ = name <* token ":"


queryDocument: Parser s Queries
queryDocument = skipSpace *> many1 definition <* skipSpace


operationTypeDefinition : Parser s OperationTypeDefinition
operationTypeDefinition =
    operationType
    >>= (\type_ -> maybe name
    >>= (\name -> optional [] (parentheses <| many1 variableDefinition)
    >>= (\variables -> optional [] directiveSet
    >>= (\directives -> selectionSet
    >>= (\selections ->
    succeed (OperationTypeDefinition type_ name variables directives selections) <?> "expected operation type definition"
    )))))


fragment: Parser s Fragment
fragment =
    token "fragment"
    >>= (\_ -> name
    >>= (\name -> typeCondition
    >>= (\type_ -> optional [] directiveSet
    >>= (\directives -> selectionSet
    >>= (\selections ->
    succeed (Fragment name type_ directives selections) <?> "expected fragment definition"
    )))))


fragmentSpread: Parser s FragmentSpread
fragmentSpread =
    token "..."
    >>= (\_ -> name
    >>= (\name -> if name == "on" then fail "fragment spread name can't be " else optional [] directiveSet
    >>= (\directives ->
    succeed (FragmentSpread name directives) <?> "expected fragment spread"
    )))


inlineFragment: Parser s InlineFragment
inlineFragment =
    token "..."
    >>= (\_ -> maybe typeCondition
    >>= (\type_ -> optional [] directiveSet
    >>= (\directives -> lazy (\() -> selectionSet)
    >>= (\selections ->
    succeed (InlineFragment type_ directives selections) <?> "expected inline fragment"
    ))))


nameValue: Parser s {name: Name, value: Value}
nameValue =
    name
    >>= (\name -> token ":"
    >>= (\_ -> value
    >>= (\value ->
    succeed {name = name, value = value} <?> "expected name with value"
    )))


variableDefinition: Parser s VariableDefinition
variableDefinition =
    variable
    >>= (\variable -> token ":"
    >>= (\_ -> type_
    >>= (\type_ -> maybe value
    >>= (\default ->
    succeed (VariableDefinition variable type_ default) <?> "expected variable definition"
    ))))


field: Parser s Field
field =
    maybe alias_
    >>= (\alias_ -> name
    >>= (\name -> optional [] (parentheses <| many1 nameValue)
    >>= (\arguments -> optional [] directiveSet
    >>= (\directives -> optional [] selectionSet
    >>= (\selections ->
    succeed ( Field alias_ name arguments directives selections) <?> "expected selection"
    )))))


directive: Parser s Directive
directive =
    token "@"
    >>= (\_ -> name
    >>= (\name -> optional [] (parentheses <| many1 nameValue)
    >>= (\arguments ->
    succeed (Directive name arguments) <?> "expected directive"
    )))


type_ : Parser s Type
type_ =
    NotNullNamed <$> name <* token "!"
    <|> NamedType <$> name
    <|> lazy (\() -> NotNullList <$> brackets type_<* token "!")
    <|> lazy (\() -> ListType <$> brackets type_)
    <?> "expected type"


-- TODO: Add support for exponential notation
value: Parser s Value
value =
    VariableValue <$> variable
    <|> (token "true" $> BooleanValue True) <|> (token "false" $> BooleanValue False)
    <|> NullValue <$ token "null"
    <|> FloatValue <$> lex float
    <|> IntValue <$> lex int
    <|> EnumValue <$> name
    <|> StringValue <$> stringValue
    <|> lazy (\() -> ObjectValue <$> braces (many (lazy (\() -> nameValue))))
    <|> lazy (\() -> ListValue <$> brackets (many value))
    <?> "expected value"


definition: Parser s Definition
definition =
    SelectionDefinition <$> selectionSet
    <|> OperationDefinition <$> operationTypeDefinition
    <|> FragmentDefinition <$> fragment
    <?> "expected definition"


selection: Parser s Selection
selection =
    FieldSelection <$> lazy (\() -> field)
   <|> FragmentSpreadSelection <$> fragmentSpread
   <|> InlineFragmentSelection <$> lazy (\() -> inlineFragment)
   <?> "expected selection"


-- schema


scalarType: Parser s ScalarType
scalarType = token "scalar" *> type_


defaultValue: Parser s Value
defaultValue = token "=" *> value


extendedType: Parser s ObjectType
extendedType = token "extend" *> objectType


schemaDocument: Parser s Schema
schemaDocument = skipSpace *> many1 schemaDefinition <* skipSpace


interfaces: Parser s (List Type)
interfaces = token "implements" *> many1 type_


unionType: Parser s UnionType
unionType =
    token "union"
    >>= (\_ -> type_
    >>= (\ty -> token "="
    >>= (\_ -> sepBy1 (token "|") type_
    >>= (\values ->
    succeed (UnionType ty values) <?> "expected union type"
    ))))


enumType: Parser s EnumType
enumType =
    token "enum"
    >>= (\_ -> type_
    >>= (\ty -> braces <| many1 name
    >>= (\values ->
    succeed (EnumType ty values) <?> "expected enum type"
    )))


inputValueDefinition: Parser s InputValueDefinition
inputValueDefinition =
    name
    >>= (\name -> token ":"
    >>= (\_ -> type_
    >>= (\ty -> maybe defaultValue
    >>= (\value ->
    succeed (InputValueDefinition name ty value) <?> "expected input value definition"
    ))))


fieldDefinition: Parser s FieldDefinition
fieldDefinition =
    name
    >>= (\name -> optional [] (parentheses <| many1 inputValueDefinition)
    >>= (\args -> token ":"
    >>= (\_ -> type_
    >>= (\ty ->
    succeed (FieldDefinition name args ty) <?> "expected field definition"
    ))))


objectType: Parser s ObjectType
objectType =
    token "type"
    >>= (\_ -> name
    >>= (\name -> optional [] interfaces
    >>= (\interfaces -> braces (many1 fieldDefinition)
    >>= (\fields ->
    succeed (ObjectType name interfaces fields) <?> "expected object type"
    ))))


interfaceType: Parser s InterfaceType
interfaceType =
    token "interface"
    >>= (\_ -> name
    >>= (\name -> braces (many1 fieldDefinition)
    >>= (\fields ->
    succeed (InterfaceType name fields) <?> "expected interface type"
    )))


inputType: Parser s InputType
inputType =
    token "input"
    >>= (\_ -> name
    >>= (\name -> braces <| many1 inputValueDefinition
    >>= (\inputs ->
    succeed (InputType name inputs) <?> "expected input type"
    )))

schemaDefinition: Parser s SchemaDefinition
schemaDefinition =
    ObjectTypeDefinition <$> objectType
    <|> InterfaceTypeDefinition <$> interfaceType
    <|> UnionTypeDefinition <$> unionType
    <|> ScalarTypeDefinition <$> scalarType
    <|> EnumTypeDefinition <$> enumType
    <|> InputTypeDefinition <$> inputType
    <|> ExtendedTypeDefinition <$> extendedType
    <?> "expected schema definition"

