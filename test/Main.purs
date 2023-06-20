module Test.Main where

import Prelude

import Affjax.Node as AffjaxNode
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class.Console as Log
import GraphQL.FunDeps (class GraphQLReqRes, Gql(..), GraphQL, GraphQLClientAff, graphQL)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

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
client = graphQL "https://api.react-finland.fi/graphql" [] AffjaxNode.driver

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec'
        ( defaultConfig
            { timeout = Just (Milliseconds 10000.0)
            }
        )
        [ consoleReporter ] do
        describe "GraphQL Fundeps" do
          it "Works" do
            { conferences } <- client (Gql :: Gql ReactFinlandConferences) {}
            for_ conferences \{ id } -> do
              { conference: { websiteUrl, speakers } } <- client (Gql :: Gql ReactFinlandConference) { id }
              Log.info websiteUrl
              Log.info $ show speakers
