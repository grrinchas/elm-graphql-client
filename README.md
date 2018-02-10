## elm-graphql-client

```bash
elm-package install grrinchas/elm-graphql-client
```

Parses [GraphqQL](http://facebook.github.io/graphql/October2016/) query document and provides 
interface to send requests. This allows us to write queries in GraphQL language instead 
of custom DSLs. And share same queries between different platforms. For example, Elm, Android, iOS etc.

Let's say we wish to execute a queries against the following schema

```graphql

type Publication @model {
    id: ID! @isUnique

    title: String!
    image: String!
    content: String!
    owner: User! @relation(name: "UserOnPublication")
}

type User @model {
    id: ID! @isUnique

    username: String! @isUnique
    email: String! @isUnique
    picture: String! 

    publications: [Publication!]! @relation(name: "UserOnPublication")

}

```

First we create a `queries.graphql` file which will be served from `http://localhost:3000/queries.graphql` 


 ```graphql
query allPublications {
    allPublications {
        ...publicationInfo
        owner {
            ...userInfo
        }
    }
}

query user($id: ID!) {
    User(id: $id) {
        ...userInfo
        publications {
            ...publicationInfo
        }
    }
}

fragment publicationInfo on Publication {
    id
    title
    image
    content
}

fragment userInfo on User {
    id
    username
    email
    picture
}

```

Then we create initial `Payload` which will be used to send requests.

```elm
initialPayload: Payload decoder
initialPayload =
    { queries = GraphQL.remote <| GraphQL.queries "http://localhost:3000/queries.graphql"
    , endpoint = GraphQL.endpoint "https://api.graph.cool/simple/endpoint" (Json.Decode.fail "missing decoder")
    , name = ""
    , variables = []
    }
```

* `queries` - represents queries document. Because our document is not local, we wrap it in the `GraphQL.remote`.
* `endpoint` - represents GraphQL endpoint. **NOTE:** that the library does not support automatic decoding, therefore
we need to provide custom decoder for each request.
* `name` - is the name of the query in the document. In our case it can be `allPublications` or `user`. **NOTE:** 
that shorthand queries are not supported and will be ignored, therefore each query must have a name.
* We can pass `variables` - to the query.


Then we can query `allPublications` and `user`

```elm
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
                |> GraphQL.send ( RemoteData.fromResult )
                |> Cmd.map OnFetchedUser
                |> (,) model
                
        -- rest of the code 
```

* For the first query we add a name and a decoder to `initialPayload` and send it. 
* For the second query we add extra variables and a `Bearer` token. And map it to 
[RemoteData](https://github.com/krisajenkins/remotedata/tree/4.3.3).

For complete example please refer to [examples](https://github.com/grrinchas/elm-graphql-client/tree/master/examples)

## Performance considerations

Note that for every query `elm-graphql-client` makes two requests: 
    
1. One request for queries document.
2. And actual query to GraphQL endpoint.

Therefore, for production you may wish to embed queries document as a string and use `GraphQL.local`

```elm
localQuery: String
localQuery = "query allPublications { allPublications {id title} }"

initialPayload: Payload decoder
initialPayload =
    { queries = GraphQL.local (GraphQL.parseLocalQueries localQuery |> Result.withDefault [])
    , endpoint = GraphQL.endpoint "https://api.graph.cool/simple/endpoint" (Json.Decode.fail "missing decoder")
    , name = ""
    , variables = []
    }
```


