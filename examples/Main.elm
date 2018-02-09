module Main exposing (..)

import AST exposing (Queries)
import GraphQL exposing (Location(Local))
import Html exposing (Html, button, div, li, text, ul)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import RemoteData exposing (WebData)


type alias User =
    { info: UserInfo
    , publications: List Publication
    }


type alias UserInfo =
    { id: String
    , username: String
    , email: String
    , picture: String
    }

type alias Publication =
    { owner: UserInfo
    , id: String
    , title : String
    , content: String
    , image: String
    }


type alias Model =
    { users: List User
    , publications: List Publication
    , token: String
    }


initialModel: Model
initialModel =
    { users = []
    , publications = []
    , token = "secret token"
    }


initialPayload: GraphQL.Payload a
initialPayload =
    { queries = GraphQL.remote (GraphQL.queries "http://localhost:3000/queries.graphql")
    , endpoint = GraphQL.endpoint "https://api.graph.cool/simple/endpoint" (Json.Decode.fail "missing decoder")
    , name = ""
    , variables = []
    }


main: Program Never Model Msg
main = Html.program
    { init = (initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }



type Msg
    = GetPublications
    | GetUser String
    | OnFetchedPublications (Result Http.Error (List Publication))
    | OnFetchedUser (WebData User)



update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetPublications ->
            initialPayload
                |> GraphQL.withName "allPublications"
                |> GraphQL.withDecoder publicationsDecoder
                |> GraphQL.send OnFetchedPublications
                |> (\cmd -> (model, cmd))

        GetUser id ->
            initialPayload
                |> GraphQL.withName "user"
                |> GraphQL.withDecoder userDecoder
                |> GraphQL.withVariables
                    [ GraphQL.variable "id" id ]
                |> GraphQL.withAuthorisation model.token
                |> GraphQL.send (RemoteData.fromResult )
                |> Cmd.map OnFetchedUser
                |> (\cmd -> (model, cmd))

        OnFetchedPublications list ->
            let _ = Debug.log "" list in
            Result.withDefault [] list
                |> (\pubs -> { model | publications = pubs })
                |> (\m -> (m, Cmd.none))

        OnFetchedUser user ->
            let _ = Debug.log "" user in
            RemoteData.map (\u -> (::) u model.users) user
                |> RemoteData.map (\users -> { model | users = users })
                |> RemoteData.map (\m -> (m, Cmd.none))
                |> RemoteData.withDefault (model, Cmd.none)


userDecoder: Json.Decode.Decoder User
userDecoder =
    Json.Decode.map2 User
        (Json.Decode.at ["data", "User"] userInfoDecoder)
        (Json.Decode.at ["data", "User", "publications"] <| Json.Decode.list publicationDecoder)


userInfoDecoder: Json.Decode.Decoder UserInfo
userInfoDecoder =
    Json.Decode.map4 UserInfo
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "username" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
        (Json.Decode.field "picture" Json.Decode.string)


publicationDecoder: Json.Decode.Decoder Publication
publicationDecoder =
    Json.Decode.map5 Publication
        (Json.Decode.field "owner" userInfoDecoder)
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
        (Json.Decode.field "image" Json.Decode.string)


publicationsDecoder: Json.Decode.Decoder (List Publication)
publicationsDecoder =
    Json.Decode.at ["data", "allPublications"] <| Json.Decode.list publicationDecoder


view: Model -> Html Msg
view model =
    div []
        [ button [onClick GetPublications] [text "Publications"]
        , button [onClick <| GetUser "34240"] [text "Users"]
        , ul [] <|
            List.map (\pub -> li [] [text <| pub.title] ) model.publications
        , ul [] <|
            List.map (\user -> li [] [text <| user.info.username] ) model.users
        ]






