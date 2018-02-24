module Restful.Endpoint
    exposing
        ( (</>)
        , AccessToken
        , BackendUrl
        , EndPoint
        , EntityId
        , EntityUuid
        , KeyValue
        , TokenStrategy
        , decodeEntityId
        , decodeEntityUuid
        , delete
        , drupalEndpoint
        , encodeEntityId
        , encodeEntityUuid
        , endpoint
        , fromEntityId
        , fromEntityUuid
        , get
        , get404
        , patch
        , patch_
        , post
        , put
        , put_
        , select
        , toEntityId
        , toEntityUuid
        , tokenHeader
        , tokenUrlParam
        , withError
        , withKeyValue
        , withParams
        , withPath
        , withPostedValue
        , withTokenStrategy
        )

{-| These functions facilitate CRUD operations upon entities exposed through a
Restful API. It is oriented towards a Drupal backend, but could be used (or
modified to use) with other backends that produce similar JSON.


## Constructing Endpoints

@docs EndPoint, drupalEndpoint, endpoint
@docs KeyValue, withKeyValue, withParams, withPostedValue, withError, withPath, withTokenStrategy


## Access Tokens

@docs AccessToken, TokenStrategy, tokenHeader, tokenUrlParam


## CRUD Operations

@docs BackendUrl
@docs get, get404, select, patch, patch_, post, put, put_, delete


## EntityId

@docs EntityId, decodeEntityId, encodeEntityId, fromEntityId, toEntityId


## EntityUuid

@docs EntityUuid, decodeEntityUuid, encodeEntityUuid, fromEntityUuid, toEntityUuid


## Helpers

@docs (</>)

-}

import Gizra.Json exposing (decodeInt)
import Http exposing (Error(..), expectJson)
import HttpBuilder exposing (..)
import Json.Decode exposing (Decoder, field, index, list, map, map2, succeed)
import Json.Encode exposing (Value)


{-| The base URL for a backend (i.e. the part that doesn't vary from
one endpoint to another).
-}
type alias BackendUrl =
    String


{-| An access token.
-}
type alias AccessToken =
    String


{-| This is a start at a nicer idiom for dealing with Restful JSON endpoints.
The basic idea is to include in this type all those things about an endpoint
which don't change. For instance, we know the path of the endpoint, what kind
of JSON it emits, etc. -- that never varies.

The structure is somewhat specialized for a headless Drupal backend using its
Restful module. However, it may work in other REST environments (let us know
if any changes are needed to handle your case).

The parameters have the following significance.

| Type | Significance |
| ---- | ------------ |
| `error` | Your error type. <p>If you don't want to do something special with errors, then it can just be `Http.Error` |
| `params` | A type for the query params that this endpoint uses. <p>If your endpoint doesn't take params, just use `()` (or, a phantom type variable, if you like). |
| `key` | Your ID type. We usually use some kind of `EntityId`. |
| `value` | Your value type. |
| `posted` | The type you would use in POST requests, when creating a new value. May be missing some information from `value` which the backend will supply. May be the same as `value` if POST isn't special. |

The fields you need to fill in for an endpoint are as follows:

| Field | Usage |
| ----- | ----- |
| `decodeKey` | Given the JSON the backend returns for each item, how can we decode your `key` type? <p>If you're using a kind of `EntityId`, for the `key`, then you can just supply `decodeEntityId`. |
| `decodeMultiple` | For requests like `select`, which return multiple values, what do we need to decode to get a list we can apply `decodeKey` or `decodeVvalue` to? |
| `decodeSingle` | For requests like `get`, which return a single value, what do we need to decode before we apply `decodeKey` or `decodeValue`? That is, how must we traverse the JSON to get to the value which `decodeKey` or `decodeValue` can decode? |
| `decodeValue` | Given the JSON the backend returns for each item, how can we decode your `value` type? |
| `encodeParams` | Takes your `params` type and converts it into something we can feed to `HttpBuilder.withQueryParams`. So, you get type-safety for the params! If you never take params, you can supply `always []`. |
| `encodePostedValue` | An encoder for your `postedValue`, used when creating new values using POST requests. May be the same as `encodeValue` if POST isn't special in your case. |
| `encodeValue` | An encoder for your `value` type (e.g. for PUT). |
| `keyToUrlPart` | Given your `key`, what should we put after the `path` in a URL to refer to this entity (e.g. for PATCH, PUT or DELETE)? |
| `mapError` | Converts an `Http.Error` to the more meaningful `error` type you'd like to use. Or, just supply `identity` if your `error` type is `Http.Error`. |
| `path` | The relative path to the endpoint ... that is, the part that varies between one endpoint and another, as a suffix to the `BackendUrl`. |
| `tokenStrategy` | Given an `AccessToken`, how should we pass it to the backend? |

-}
type EndPoint error params key value posted
    = EndPoint
        { encodeParams : params -> List ( String, String )
        , encodePostedValue : posted -> Value
        , kv : KeyValue key value
        , mapError : Error -> error
        , path : String
        , tokenStrategy : TokenStrategy
        }


{-| Various things which relate to the key and value. These all need to be changed
together ... they could be split up a bit if Elm had Rank-N types ... in that case,
we'd use:

    { decodeMultiple : forall a. Decoder a -> Decoder (List a)
    , decodeSingle : forall a. Decoder a -> Decoder a
    }

... and then the things relating to keys could be changed separately from the things
relating to values.

-}
type alias KeyValue key value =
    { decodeKey : Decoder key
    , decodeMultiple : Decoder ( key, value ) -> Decoder (List ( key, value ))
    , decodeSingle : Decoder ( key, value ) -> Decoder ( key, value )
    , decodeValue : Decoder value
    , encodeValue : value -> Value
    , keyToUrlPart : key -> String
    }


{-| Use the supplied settings for the `key` and `value` with this endpoint.

For complicated reasons, these must all be specified together.

-}
withKeyValue : KeyValue key value -> EndPoint error params a b posted -> EndPoint error params key value posted
withKeyValue kv (EndPoint endpoint) =
    EndPoint { endpoint | kv = kv }


{-| Use the supplied function to encode `params` for queries with this endpoint.
-}
withParams : (params -> List ( String, String )) -> EndPoint error p key value posted -> EndPoint error params key value posted
withParams encodeParams (EndPoint endpoint) =
    EndPoint { endpoint | encodeParams = encodeParams }


{-| Use the supplied function to encode new `posted` values.
-}
withPostedValue : (posted -> Value) -> EndPoint error params key value a -> EndPoint error params key value posted
withPostedValue encodePostedValue (EndPoint endpoint) =
    EndPoint { endpoint | encodePostedValue = encodePostedValue }


{-| Use the supplied function to convert `Http.Error` to your desired `error` type.
-}
withError : (Error -> error) -> EndPoint a params key value posted -> EndPoint error params key value posted
withError mapError (EndPoint endpoint) =
    EndPoint { endpoint | mapError = mapError }


{-| Use the supplied path for this endpoint.
-}
withPath : String -> EndPoint error params key value posted -> EndPoint error params key value posted
withPath path (EndPoint endpoint) =
    EndPoint { endpoint | path = path }


{-| Use the supplied token strategy.
-}
withTokenStrategy : TokenStrategy -> EndPoint error params key value posted -> EndPoint error params key value posted
withTokenStrategy tokenStrategy (EndPoint endpoint) =
    EndPoint { endpoint | tokenStrategy = tokenStrategy }


{-| Construct a Drupal-oriented endpoint, with as many defaults filled in as possible.

  - The first parameter is the `path` to the endpoint.
  - The second parameter is a decoder for your `value` type.
  - The third parameter is an encoder for your `value` type.

Yes, just three parameters! We'll supplement that with various Drupal-oriented defaults:

  - The `key` is some kind of `EntityId`, and it can be found in an `id` field in the JSON.
    But you can change that with ...

  - You create values with the full `value` type (not a partial `posted` type).
    But you can change that with ...

  - Multiple values are returned as a JSON array inside a `data` field.
    But you can change that with ...

  - Single values are returned as a single-elmeent JSON array, inside a `data` field.
    But you can change that with ...

  - Your endpoint doesn't use any URL params. But you can change that with ...

  - You're not using a custom error type. But you can change that with ...

  - An access token, if provided, will be sent as a URL param named "access_token".
    But you can change that with ...

-}
drupalEndpoint : String -> Decoder value -> (value -> Value) -> EndPoint Error () (EntityId a) value value
drupalEndpoint path decodeValue encodeValue =
    EndPoint
        { kv =
            { decodeKey = decodeId toEntityId
            , decodeMultiple = decodeDrupalList
            , decodeSingle = decodeSingleEntity
            , decodeValue = decodeValue
            , encodeValue = encodeValue
            , keyToUrlPart = fromEntityId >> toString
            }
        , encodeParams = always []
        , encodePostedValue = encodeValue
        , mapError = identity
        , path = path
        , tokenStrategy = TokenUrlParam "access_token"
        }


{-| Produces an `EndPoint` with very basic defaults ... it will need customization to actuall work
with the JSON your backend returns.

  - The first parameter is the `path` to the endpoint.
  - The second parameter is a decoder for your `value` type.
  - The third parameter is an encoder for your `value` type.

-}
endpoint : String -> Decoder value -> (value -> Value) -> EndPoint Error () Int value value
endpoint path decodeValue encodeValue =
    EndPoint
        { kv =
            { decodeKey = decodeId identity
            , decodeMultiple = list
            , decodeSingle = identity
            , decodeValue = decodeValue
            , encodeValue = encodeValue
            , keyToUrlPart = toString
            }
        , encodeParams = always []
        , encodePostedValue = encodeValue
        , mapError = identity
        , path = path
        , tokenStrategy = TokenUrlParam "access_token"
        }


{-| We can use two strategies to send an `AccessToken` to the backend -- either an
HTTP header (with a key and value), or a param for the URL (with key and value).
Use `tokenHeader` or `tokenUrlParam` to construct.
-}
type TokenStrategy
    = TokenHeader String
    | TokenUrlParam String


{-| Send the `AccessToken` to the backend using the specified HTTP header.
-}
tokenHeader : String -> TokenStrategy
tokenHeader =
    TokenHeader


{-| Send the `AccessToken` to the backend using the specified parameter in the URL.
-}
tokenUrlParam : String -> TokenStrategy
tokenUrlParam =
    TokenUrlParam


{-| Appends the second parameter to the first, joining them with a "/", but avoiding "//".

    "http://www.apple.com"  </> "path"  --> "http://www.apple.com/path"

    "http://www.apple.com"  </> "/path" --> "http://www.apple.com/path"

    "http://www.apple.com/" </> "path"  --> "http://www.apple.com/path"

    "http://www.apple.com/" </> "/path" --> "http://www.apple.com/path"

-}
(</>) : String -> String -> String
(</>) left right =
    case ( String.endsWith "/" left, String.startsWith "/" right ) of
        ( False, False ) ->
            left ++ "/" ++ right

        ( True, True ) ->
            left ++ String.dropLeft 1 right

        _ ->
            left ++ right


withAccessToken : TokenStrategy -> Maybe AccessToken -> RequestBuilder a -> RequestBuilder a
withAccessToken strategy maybeToken builder =
    case maybeToken of
        Just token ->
            case strategy of
                TokenHeader header ->
                    withHeader header token builder

                TokenUrlParam param ->
                    withQueryParams [ ( param, token ) ] builder

        Nothing ->
            builder


expectMultiple : KeyValue key value -> RequestBuilder a -> RequestBuilder (List ( key, value ))
expectMultiple kv =
    map2 (,) kv.decodeKey kv.decodeValue
        |> kv.decodeMultiple
        |> expectJson
        |> withExpect


expectSingle : KeyValue key value -> RequestBuilder a -> RequestBuilder ( key, value )
expectSingle kv =
    map2 (,) kv.decodeKey kv.decodeValue
        |> kv.decodeSingle
        |> expectJson
        |> withExpect


{-| We could avoid this if Elm had Rank-N types, because in that case
`EndPoint.decodeSingle` could demand a polymorphic function. Without that, we
need to fulfill the more specific type signature in `Endpoint.decodeSingle` ...
fortunately, in the cases we need that, we actually know the key!
-}
expectSingleWithKey : KeyValue key value -> key -> RequestBuilder a -> RequestBuilder value
expectSingleWithKey kv key =
    map2 (,) (succeed key) kv.decodeValue
        |> kv.decodeSingle
        |> map Tuple.second
        |> expectJson
        |> withExpect


{-| Select entities from an endpoint.

What we hand you is a `Result` with a list of entities, since that is the most
"natural" thing to hand back. You can convert it to a `RemoteData` easily with
a `RemoteData.fromResult` if you like.

-}
select : BackendUrl -> Maybe AccessToken -> EndPoint error params key value posted -> params -> (Result error (List ( key, value )) -> msg) -> Cmd msg
select backendUrl accessToken ((EndPoint endpoint) as ep) params tagger =
    HttpBuilder.get (backendUrl </> endpoint.path)
        |> withQueryParams (endpoint.encodeParams params)
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectMultiple endpoint.kv
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Gets a entity from the backend via its ID.

If we get a 404 error, we'll give you an `Ok Nothing`, rather than an error,
since the request essentially succeeded ... there merely was no entity with
that ID. If you'd prefer an error in that situation, you can use `get404`
instead.

-}
get : BackendUrl -> Maybe AccessToken -> EndPoint error params key value posted -> key -> (Result error (Maybe ( key, value )) -> msg) -> Cmd msg
get backendUrl accessToken ((EndPoint endpoint) as ep) key tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.get
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingle endpoint.kv
        |> send
            (\result ->
                let
                    recover =
                        case result of
                            Err (BadStatus response) ->
                                if response.status.code == 404 then
                                    Ok Nothing
                                else
                                    Result.map Just result

                            _ ->
                                Result.map Just result
                in
                    recover
                        |> Result.mapError endpoint.mapError
                        |> tagger
            )


{-| Let `get`, but treats a 404 response as an error in the `Result`, rather than a `Nothing` response.
-}
get404 : BackendUrl -> Maybe AccessToken -> EndPoint error params key value posted -> key -> (Result error ( key, value ) -> msg) -> Cmd msg
get404 backendUrl accessToken ((EndPoint endpoint) as ep) key tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.get
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingle endpoint.kv
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Sends a `POST` request to create the specified value.
-}
post : BackendUrl -> Maybe AccessToken -> EndPoint error params key value posted -> value -> (Result error ( key, value ) -> msg) -> Cmd msg
post backendUrl accessToken ((EndPoint endpoint) as ep) value tagger =
    (backendUrl </> endpoint.path)
        |> HttpBuilder.post
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingle endpoint.kv
        |> withJsonBody (endpoint.kv.encodeValue value)
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Sends a `PUT` request to create the specified value.

Assumes that the backend will respond with the full value. If that's not true, you
can use `put_` instead.

-}
put : BackendUrl -> Maybe AccessToken -> EndPoint error params key value posted -> key -> value -> (Result error value -> msg) -> Cmd msg
put backendUrl accessToken ((EndPoint endpoint) as ep) key value tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.put
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingleWithKey endpoint.kv key
        |> withJsonBody (endpoint.kv.encodeValue value)
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Like `put`, but ignores any value sent by the backend back ... just interprets errors.
-}
put_ : BackendUrl -> Maybe AccessToken -> EndPoint error params key value posted -> key -> value -> (Result error () -> msg) -> Cmd msg
put_ backendUrl accessToken ((EndPoint endpoint) as ep) key value tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.put
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> withJsonBody (endpoint.kv.encodeValue value)
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Sends a `PATCH` request for the specified key and value.

Now, the point of a `PATCH` request is that you're not sending the **full** value,
but some subset. So, you supply your own JSON value, rather than using the one that
the endpoint would create use for PUT or POST. (We could have a separate config for
each kind of PATCH, which would contribute to type-safety, but is possibly overkill).

This function assumes that the backend will send the full value back. If it won't, then
you can use `patch_` instead.

-}
patch : BackendUrl -> Maybe AccessToken -> EndPoint error params key value posted -> key -> Value -> (Result error value -> msg) -> Cmd msg
patch backendUrl accessToken ((EndPoint endpoint) as ep) key value tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.patch
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingleWithKey endpoint.kv key
        |> withJsonBody value
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Like `patch`, but doesn't try to decode the response ... just reports errors.
-}
patch_ : BackendUrl -> Maybe AccessToken -> EndPoint error params key value posted -> key -> Value -> (Result error () -> msg) -> Cmd msg
patch_ backendUrl accessToken ((EndPoint endpoint) as ep) key value tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.patch
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> withJsonBody value
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Delete entity.
-}
delete : BackendUrl -> Maybe AccessToken -> EndPoint error params key value posted -> key -> (Result error () -> msg) -> Cmd msg
delete backendUrl accessToken ((EndPoint endpoint) as ep) key tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.delete
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Convenience for the pattern where you have a field called "id",
and you want to wrap the result in a type (e.g. PersonId Int). You can
just use `decodeId PersonId`.
-}
decodeId : (Int -> a) -> Decoder a
decodeId wrapper =
    map wrapper (field "id" decodeInt)


decodeData : Decoder a -> Decoder a
decodeData =
    field "data"


{-| Given a decoder for an entity, applies it to a JSON response that consists
of a `data` field with a array of length 1, containing the entity. (This is
what Drupal sends when you do a PUT, POST, or PATCH.)

For instance, if you POST an entity, Drupal will send back the JSON for that entity,
as the single element of an array, then wrapped in a `data` field, e.g.:

    { data :
        [
            {
                id: 27,
                label: "The label",
                ...
            }
        ]
    }

To decode this, write a decoder for the "inner" part (the actual entity), and then
supply that as a parameter to `decodeSingleEntity`.

-}
decodeSingleEntity : Decoder a -> Decoder a
decodeSingleEntity =
    decodeData << index 0


decodeDrupalList : Decoder a -> Decoder (List a)
decodeDrupalList =
    decodeData << list


{-| This is a wrapper for an `Int` id. It takes a "phantom" type variable
in order to gain type-safety about what kind of entity it is an ID for.
So, to specify that you have an id for a clinic, you would say:

    clinidId : EntityId ClinicId

-}
type EntityId a
    = EntityId Int


{-| This is how you create a EntityId, if you have an `Int`. You can create
any kind of `EntityId` this way ... so you would normally only do this in
situations that are fundamentally untyped, such as when you are decoding
JSON data. Except in those kind of "boundary" situations, you should be
working with the typed EntityIds.
-}
toEntityId : Int -> EntityId a
toEntityId =
    EntityId


{-| This is how you get an `Int` back from a `EntityId`. You should only use
this in boundary situations, where you need to send the id out in an untyped
way. Normally, you should just pass around the `EntityId` itself, to retain
type-safety.
-}
fromEntityId : EntityId a -> Int
fromEntityId (EntityId a) =
    a


{-| Decodes a EntityId.

This just turns JSON int (or string that is an int) to a EntityId. You need
to supply the `field "id"` yourself, if necessary, since id's could be present
in other fields as well.

This decodes any kind of EntityId you like (since there is fundamentally no type
information in the JSON iself, of course). So, you need to verify that the type
is correct yourself.

-}
decodeEntityId : Decoder (EntityId a)
decodeEntityId =
    Json.Decode.map toEntityId decodeInt


{-| Encodes any kind of `EntityId` as a JSON int.
-}
encodeEntityId : EntityId a -> Value
encodeEntityId =
    Json.Encode.int << fromEntityId


{-| This is a wrapper for an UUID.
-}
type EntityUuid a
    = EntityUuid String


{-| This is how you create a EntityUuid, if you have a `String`. You can create
any kind of `EntityUuid` this way ... so you would normally only do this in
situations that are fundamentally untyped, such as when you are decoding
JSON data. Except in those kind of "boundary" situations, you should be
working with the typed EntityUuids.
-}
toEntityUuid : String -> EntityUuid a
toEntityUuid =
    EntityUuid


{-| This is how you get a `String` back from a `EntityUuid`. You should only use
this in boundary situations, where you need to send the UUID out in an untyped
way. Normally, you should just pass around the `EntityUuid` itself, to retain
type-safety.
-}
fromEntityUuid : EntityUuid a -> String
fromEntityUuid (EntityUuid a) =
    a


{-| Decodes a EntityUuid.
-}
decodeEntityUuid : Decoder (EntityUuid a)
decodeEntityUuid =
    Json.Decode.map toEntityUuid Json.Decode.string


{-| Encodes any kind of `EntityUuid` as a JSON string.
-}
encodeEntityUuid : EntityUuid a -> Value
encodeEntityUuid =
    Json.Encode.string << fromEntityUuid


urlForKey : BackendUrl -> EndPoint error params key value posted -> key -> String
urlForKey backendUrl (EndPoint endpoint) key =
    backendUrl </> endpoint.path </> endpoint.kv.keyToUrlPart key
