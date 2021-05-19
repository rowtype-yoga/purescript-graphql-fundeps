module GraphQL.FunDeps where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))

data GraphQL

data Gql (operation :: GraphQL)
  = Gql

class GraphQLReqRes (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type) | operation -> gql i o

type Endpoint = String

type GraphQLClient' (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type) = GraphQLReqRes operation gql i o => IsSymbol gql => JSON.WriteForeign { | i } => JSON.ReadForeign { | o } => Gql operation -> Record i -> Aff { | o }

type GraphQLClient = forall (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type). GraphQLClient' operation gql i o

graphQL :: Endpoint -> Array RequestHeader -> (forall (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type). GraphQLReqRes operation gql i o => IsSymbol gql => JSON.WriteForeign { | i } => JSON.ReadForeign { | o } => Gql operation -> Record i -> Aff { | o })
graphQL endpoint headers = go
  where
  go :: forall (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type). GraphQLReqRes operation gql i o => IsSymbol gql => JSON.WriteForeign { | i } => JSON.ReadForeign { | o } => Gql operation -> Record i -> Aff { | o }
  go _ variables = do
    let
      input =
        { variables
        , query: String.replaceAll (String.Pattern "\n") (String.Replacement " ") (String.replaceAll (String.Pattern "\r\n") (String.Replacement " ") (reflectSymbol (Proxy :: Proxy gql)))
        }
    res <-
      AX.request
        ( AX.defaultRequest
            { url = endpoint
            , method = Left POST
            , responseFormat = ResponseFormat.string
            , content =
              Just
                (RequestBody.string (JSON.writeJSON input))
            , headers = headers <> [ContentType $ MediaType "application/json"]
            }
        )
    case res of
      Left err -> do
        liftEffect $ Log.info "Request did not go through"
        throwError (error $ AX.printError err)
      Right response -> case (JSON.readJSON response.body) of
        Left err1 -> throwError (error $ ("Got an error " <> show response.body <> " err " <> show err1 <> " using input " <> (JSON.writeJSON input)))
        Right ({ data: d } :: { data :: { | o } }) -> pure d