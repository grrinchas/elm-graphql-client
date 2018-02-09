module GraphQL exposing
    ( Payload
    , send
    , Fetch
    , Variable
    , variable
    , Location
    , local
    , remote
    , queries
    , endpoint
    , authorisedEndpoint
    , addHeaders
    , withName, withVariables, withDecoder, withAuthorisation
    , parseLocalQueries
    , parseRemoteQueries
    )

{-| A library for parsing GraphQL query document and creating requests. The document can be local or remote.

The main point of this library is to write GraphQL queries in GraphQL language instead of custom DSLs

# Usage
@docs Payload, send, Variable, variable

# Customisation

Following functions allows easy way to customise `initialPayload` using pipeline operator `|>`

     initialPayload
       |> withName "user"
       |> withDecoder userDecoder
       |> withVariables
           [ variable "id" "323" ]
       |> withAuthorisation "secretToken"
       |> send OnFetchedUser

@docs withName, withVariables, withDecoder, withAuthorisation

# Local and Remote queries
@docs Location, local, remote

# Requests
@docs Fetch, queries, endpoint, authorisedEndpoint, addHeaders

# Parsing
@docs parseLocalQueries, parseRemoteQueries

-}

import AST exposing (Definition(FragmentDefinition, OperationDefinition, SelectionDefinition), Queries, getAllFragments)
import Combine
import Encoder
import Http exposing (Body, Request, Response)
import Json.Decode exposing (Decoder)
import Json.Encode
import Parser
import Task
import Time exposing (Time)


{-| Represents a payload for the request. Every request needs:

* `queries` - the location of the document
* `endpoint` - a GraphQL endpoint to which request will be sent to
* `name` - of the query which must be present in the document
* `variables` - for the query

This is how `initialPayload` may look.

    initialPayload: Payload dec
    initialPayload =
      { queries = remote (queries "http://localhost:3000/queries.graphql")
      , endpoint = endpoint "http://endpoint" (Json.Decode.fail "missing decoder")
      , name = ""
      , variables = []
      }
-}
type alias Payload dec =
    { queries: Location Queries
    , endpoint: Fetch dec
    , name: String
    , variables: List Variable
    }


{-| Sends a request and maps response to the `msg`. This function is almost identical to
    [Http.send](http://package.elm-lang.org/packages/elm-lang/http/1.0.0/Http). It takes a message
    which can accept `Result Http.Error a` and a `Payload`. This is how you could use previously defined
    `initialPayload` to send request for `allPublications`.

    type Msg
      = GetPublications
      | OnFetchedPublications (Result Http.Error (List Publication))

    update: Msg -> Model -> (Model, Cmd Msg)
    update msg model =
      case msg of
        GetPublications ->
          initialPayload
            |> GraphQL.withName "allPublications"
            |> GraphQL.withDecoder publicationsDecoder
            |> GraphQL.send OnFetchedPublications
            |> (\cmd -> (model, cmd))

        OnFetchedPublications result ->
            ...
-}
send: (Result Http.Error a -> msg) -> Payload a -> Cmd msg
send msg input =
    case (input.endpoint, input.queries) of
        (endpoint, Local doc) ->
            Http.send msg <| Http.request { endpoint | body = buildQuery input.name doc input.variables}

        (endpoint, Remote queries) ->
            Http.toTask (Http.request queries)
                |> Task.andThen (\doc ->
                    Http.toTask <| Http.request {endpoint | body = buildQuery input.name doc input.variables})
                |> Task.attempt msg





{-| Represents variable for the GraphQL query.

-}
type Variable =
    Variable String String

{-| Main way to create a variable. First parameter is the name and the second value of the variable.

    variable "id" "32"
    variable "type" "Post"

-}
variable: String -> String -> Variable
variable name value = Variable name value


{-| Represents location of the queries document. It can be local or remote.

-}
type Location a
    = Local a
    | Remote (Fetch a)


{-| Creates remote `Location` for the request. Main usage of it is to define remote queries document in the `Payload`.

    queries = remote (queries "http://localhost:3000/queries.graphql")

-}
remote: Fetch a -> Location a
remote value = Remote value


{-| Creates local `Location` for the request. It is used to define local queries document in the `Payload`

    queries =  local (parseLocalQueries "query user {User { id }}" |> Result.withDefault [])

-}
local: a -> Location a
local value = Local value


{-| Represents request object. Think of `Fetch` as a
    [Http.Request](http://package.elm-lang.org/packages/elm-lang/http/1.0.0/Http). It has exact the same fields. Using
    `Fetch` you can build custom requests. For example, this is how `queries` request is defined

    queries: String -> Fetch Queries
    queries url =
      { method = "GET"
      , headers = []
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectStringResponse parseRemoteQueries
      , timeout = Nothing
      , withCredentials = False
      }

-}
type alias Fetch dec =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect dec
    , timeout : Maybe Time
    , withCredentials : Bool
    }


{-| Creates a simple request for remote queries document. First parameter is an URL of the document.

-}
queries: String -> Fetch Queries
queries url =
    { method = "GET"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectStringResponse parseRemoteQueries
    , timeout = Nothing
    , withCredentials = False
    }


{-| Creates a simple request to query an endpoint. First parameter is the endpoint and the second Json decoder.

    endpoint "http://endpoint" (Json.Decode.fail "missing decoder")

-}
endpoint: String -> Decoder a -> Fetch a
endpoint url decoder =
    { method = "POST"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }


{-| Creates an authorised request with `Bearer` token to query an endpoint.
    First parameter is the endpoint, second - token, third - Json decoder.

    authorisedEndpoint "http://endpoint" "secretToken" (Json.Decode.fail "missing decoder")

-}
authorisedEndpoint: String -> String ->  Decoder a  ->  Fetch a
authorisedEndpoint url token decoder =
    endpoint url decoder
        |> addHeaders [ Http.header "Authorization" <| "Bearer " ++ token ]


{-| Convenience function to add headers to the request. This is how `authorisedEndpoint` is defined

    authorisedEndpoint: String -> String ->  Decoder a  ->  Fetch a
    authorisedEndpoint url token decoder =
      endpoint url decoder
        |> addHeaders [ Http.header "Authorization" <| "Bearer " ++ token ]

-}
addHeaders: List Http.Header -> Fetch dec -> Fetch dec
addHeaders headers request =
    { request | headers = headers }


{-| Changes the name of the `Payload`.

-}
withName: String -> Payload dec -> Payload dec
withName name input = {input| name = name}


{-| Changes decoder of the `Payload`.

-}
withDecoder: Decoder dec -> Payload dec -> Payload dec
withDecoder dec input =
    case input.endpoint of
        endpoint -> { input | endpoint = {endpoint | expect = Http.expectJson dec}}


{-| Changes list of variables of the `Payload`.

-}
withVariables: List Variable -> Payload dec -> Payload dec
withVariables var input =  {input | variables = var}


{-| Adds `Authorisation` header to the `Payload` endpoint with provided token.

-}
withAuthorisation: String -> Payload dec -> Payload dec
withAuthorisation token input =
    {input | endpoint = addHeaders [ Http.header "Authorization" <| "Bearer " ++ token ] input.endpoint }



fetchCustomQueries: Fetch Queries -> (Result Http.Error Queries -> msg) -> Cmd msg
fetchCustomQueries fetch msg = Http.send msg <| Http.request fetch


fetchQueries: String -> (Result Http.Error Queries -> msg) -> Cmd msg
fetchQueries url msg = fetchCustomQueries (queries url) msg


buildQuery: String -> Queries -> List Variable -> Http.Body
buildQuery operation doc variables =
    case AST.getOperation operation doc of
        Nothing ->
            [("error", Json.Encode.string ("Can't find operation with the name: " ++ operation))]
                |> Json.Encode.object
                |> Http.jsonBody
        Just op -> let encoded =
                List.concatMap (getAllFragments doc) op.selections
                    |> List.map Encoder.fragment
                    |> String.join "\n\n"
                    |> (++) (Encoder.operationTypeDefinition op)
            in
            [ ("query", Json.Encode.string encoded)
            , ("variables", Json.Encode.object <| List.map encodeVariable variables)
            ]
                |> Json.Encode.object
                |> Http.jsonBody


encodeVariable: Variable -> (String, Json.Encode.Value)
encodeVariable (Variable name value) = (name, Json.Encode.string value)


{-| Parses given string and returns Either `Queries` or `String` error. This function should be used
to create `Local Queries`.

-}
parseLocalQueries: String -> Result String Queries
parseLocalQueries body =
    case Combine.parse Parser.queryDocument body of
         Err (_, _, res) -> Err <| toString res
         Ok (st, stream, doc) -> Ok doc


{-| Parses remote queries document response and returns Either `Queries` or `String` error.
    This function should be used when building custom request `Fetch Queries`.

-}
parseRemoteQueries: Response String -> Result String Queries
parseRemoteQueries response =
    case response.status.code /= 200 of
        True ->
            Err response.status.message

        False ->
            case parseLocalQueries response.body of
                Err err -> Err err
                Ok doc  -> Ok doc

