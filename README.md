# purescript-graphql-fundeps

Type-safe graphql queries using functional dependencies.

Use the `GraphQLReqRes` class to define expected input and output.
Then, create a `client` and use it to interact with your API.

```purescript
endpoint = "https://api.react-finland.fi/graphql" :: String

foreign import data ReactFinlandConferences :: GraphQL

instance graphqlReactFinlandConferences ::
  GraphQLReqRes ReactFinlandConferences """query {
  conferences {
    name
    id
  }
}
""" () ( conferences :: Array { name :: String, id :: String } )

foreign import data ReactFinlandConference :: GraphQL

instance graphqlReactFinlandConference ::
  GraphQLReqRes ReactFinlandConference """query($id:ID!) {
  conference(id:$id) {
    year
    websiteUrl
    speakers {
      firstName
      lastName
    }
  }
}
""" ( id :: String ) ( conference ::
        { year :: String
        , websiteUrl :: String
        , speakers ::
            Array
              { firstName :: String
              , lastName :: String
              }
        }
    )

client :: GraphQLClientAff
client = graphQL "https://api.react-finland.fi/graphql" [] driver

main ∷ Effect Unit
main =
  launchAff_  do
    { conferences } <- client (Gql :: Gql ReactFinlandConferences) {}
    for_ conferences \{ id } -> do
      { conference: { websiteUrl, speakers } } <- client (Gql :: Gql ReactFinlandConference) { id }
      Log.info websiteUrl
      Log.info $ show speakers
```

You can check out the [test](./test/Main.purs) for a full example.

## Development

You can either use the nix shell, or the npm package.

### Nix shell

Having set up [flakes](https://nixos.wiki/wiki/Flakes), either using [direnv](https://direnv.net/) (`direnv allow`) or `nix develop`

Buliding:

```
spago build
```

Testing:

```
spago -x test.dhall test
```


### Npm

Just run `npm i` and then:

Building:

```
spago build
```

Testing:

```
npm run test
```
