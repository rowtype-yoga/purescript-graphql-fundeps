module GraphQL.FunDeps where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import Foreign (ForeignError)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))

data GraphQL

data Gql (operation :: GraphQL) = Gql

class
  GraphQLReqRes
    (operation :: GraphQL)
    (gql :: Symbol)
    (i :: Row Type)
    (o :: Row Type)
  | operation -> gql i o

type Endpoint = String
type NetworkErrorInfo =
  { endpoint :: Endpoint
  , requestHeaders :: Array RequestHeader
  , requestBody :: String
  , error :: AX.Error
  }

type ParsingErrorInfo =
  { endpoint :: Endpoint
  , requestHeaders :: Array RequestHeader
  , requestBody :: String
  , response :: AX.Response String
  , jsonParsingError :: NonEmptyList ForeignError
  }

type GraphQLClient''
  (operation :: GraphQL)
  (gql :: Symbol)
  (i :: Row Type)
  (o :: Row Type)
  (m :: Type -> Type)
  (e :: Type) =
  GraphQLReqRes operation gql i o
  => IsSymbol gql
  => JSON.WriteForeign { | i }
  => JSON.ReadForeign { | o }
  => MonadAff m
  => MonadThrow e m
  => (NetworkErrorInfo -> e)
  -> (ParsingErrorInfo -> e)
  -> Gql operation
  -> Record i
  -> m { | o }

type GraphQLClient =
  forall (operation :: GraphQL)
    (gql :: Symbol) (i :: Row Type) (o :: Row Type) (m :: Type -> Type) (e :: Type)
   . GraphQLReqRes operation gql i o
  => IsSymbol gql
  => JSON.WriteForeign { | i }
  => JSON.ReadForeign { | o }
  => MonadAff m
  => MonadThrow e m
  => (NetworkErrorInfo -> e)
  -> (ParsingErrorInfo -> e)
  -> Gql operation
  -> Record i
  -> m { | o }

type GraphQLClientAff =
  forall (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type)
   . GraphQLReqRes operation gql i o
  => IsSymbol gql
  => JSON.WriteForeign { | i }
  => JSON.ReadForeign { | o }
  => Gql operation
  -> Record i
  -> Aff { | o }

graphQL
  :: Endpoint
  -> Array RequestHeader
  -> ( forall (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type)
        . GraphQLReqRes operation gql i o
       => IsSymbol gql
       => JSON.WriteForeign { | i }
       => JSON.ReadForeign { | o }
       => Gql operation
       -> Record i
       -> Aff { | o }
     )
graphQL endpoint headers = graphQL'
  endpoint
  headers
  (error <<< AX.printError <<< _.error)
  (error <<< _.body <<< _.response)

graphQL' :: Endpoint -> Array RequestHeader -> GraphQLClient
graphQL' endpoint headers' = go
  where
  requestHeaders = headers' <> [ ContentType $ MediaType "application/json" ]

  go
    :: forall (operation :: GraphQL) (gql :: Symbol)
         (i :: Row Type) (o :: Row Type) (m :: Type -> Type) (e :: Type)
     . GraphQLReqRes operation gql i o
    => IsSymbol gql
    => JSON.WriteForeign
         { | i }
    => JSON.ReadForeign { | o }
    => MonadAff m
    => MonadThrow e m
    => (NetworkErrorInfo -> e)
    -> (ParsingErrorInfo -> e)
    -> Gql operation
    -> Record i
    -> m { | o }
  go networkErrorF readErrorF _ variables = do
    let
      input =
        { variables
        , query: String.replaceAll (String.Pattern "\n")
            (String.Replacement " ")
            ( String.replaceAll
                (String.Pattern "\r\n")
                (String.Replacement " ")
                (reflectSymbol (Proxy :: Proxy gql))
            )
        }
      requestBody = JSON.writeJSON input
    res <-
      liftAff $ AX.request
        ( AX.defaultRequest
            { url = endpoint
            , method = Left POST
            , responseFormat = ResponseFormat.string
            , content =
                Just
                  (RequestBody.string requestBody)
            , headers = requestHeaders
            }
        )
    case res of
      Left error -> do
        liftEffect $ Log.info "Request did not go through"
        throwError (networkErrorF { endpoint, requestHeaders, error, requestBody })
      Right response -> case (JSON.readJSON response.body) of
        Left jsonParsingError -> throwError
          ( readErrorF
              { endpoint
              , requestHeaders
              , jsonParsingError
              , requestBody
              , response
              }
          )
        Right ({ data: d } :: { data :: { | o } }) -> pure d