module Restful.Endpoint
    exposing
        ( (</>)
        , AccessToken
        , Backend
        , BackendUrl
        , EndPoint
        , EntityId
        , EntityUuid
        , TokenStrategy
        , backend
        , decodeEntityId
        , decodeEntityUuid
        , delete
        , drupalBackend
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
        , withBackend
        , withDrupalResponses
        , withPlainResponses
        , withResponses
        , withCreatedType
        , withErrorType
        , withKeyType
        , withParamsType
        , withPath
        , withTokenStrategy
        , withValueType
        )

{-| These functions and types are intended to facilitate CRUD operations upon
backend entities exposed through a Restful HTTP API.


## Backends

@docs Backend, backend, drupalBackend
@docs withResponses, withDrupalResponses, withPlainResponses


## Endpoints

@docs EndPoint, drupalEndpoint, endpoint
@docs withBackend, withKeyType, withValueType, withParamsType, withCreatedType, withErrorType, withPath, withTokenStrategy


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


{-| This represents an idiom for dealing with Restful JSON endpoints.
The basic idea is to include in the `EndPoint` type all those things about an
endpoint which don't change. For instance, we know the path to the endpoint,
what kind of JSON it emits, etc. -- that never varies.

The structure is somewhat specialized for a headless Drupal backend using its
Restful module. However, it should be adaptable for use in other REST
environments (let us know if any changes are needed to handle your case).

The type parameters have the following significance.

| Type | Significance |
| ---- | ------------ |
| `error` | Your error type. <p>If you don't want to do something special with errors, then it can just be `Http.Error` |
| `params` | A type for the query params that this endpoint uses. <p>If your endpoint doesn't take params, just use `()` (or, a phantom type variable, if you like). |
| `key` | Your ID type. We usually use some kind of `EntityId`, but you can use something else if you like. |
| `value` | Your value type. |
| `created` | The type you would use in POST requests, when creating a new value. May be missing some information from `value` which the backend will supply. May be the same as `value` if POST isn't special. |

To create an `EndPoint`, start with `drupalEndpoint` (or `endpoint`), and then use the various
`with...` functions to customize it as needed.

-}
type EndPoint error params key value created
    = EndPoint
        -- Ideally, `decodeSingle` and `decodeMultiple` would remember that
        -- their "real" type signature is the more general:
        --
        -- , decodeMultiple : forall a. Decoder a -> Decoder (List a)
        -- , decodeSingle : forall a. Decoder a -> Decoder a
        --
        -- ... but Elm doesn't have Rank-N types, so there is no way to
        -- remember that they can operate on any type. (We could add an `a`
        -- type to `EndPoint`, but that doesn't help because the compiler would
        -- fix it as `(key, value)` anyway, through type inference.)
        --
        -- To work around that, we define `decodeMultiple` and `decodeSingle`
        -- in their more polymorphic form in a separate `Backend` type,
        -- and require that to be supplied to several configuration functions,
        -- even if unchanged.
        { decodeKey : Decoder key
        , decodeMultiple : Decoder ( key, value ) -> Decoder (List ( key, value ))
        , decodeSingle : Decoder ( key, value ) -> Decoder ( key, value )
        , decodeValue : Decoder value
        , encodeCreatedValue : created -> Value
        , encodeParams : params -> List ( String, String )
        , encodeValue : value -> Value
        , keyToUrlPart : key -> String
        , mapError : Error -> error
        , path : String
        , tokenStrategy : TokenStrategy
        }


{-| Common configuration for endpoints connected to a particular backend.

You might wonder why the `BackendUrl` could not be specified here, rather than
asking for it with each CRUD request. The reason is that, in our setups, the
`BackendUrl` is typically provided a run-time, whereas the rest of the
information needed to construct the `Backend` or an `EndPoint` is known at
compile-time. So, it's convenient to construct the `Backend` and
`EndPoint` values statically, without requiring parameters.

-}
type Backend a
    = Backend
        { decodeSingle : Decoder a -> Decoder a
        , decodeMultiple : Decoder a -> Decoder (List a)
        }


{-| Constructs a default `Backend`, which decodes responses via `withPlainResponses`.
-}
backend : Backend a
backend =
    Backend
        { decodeSingle = identity
        , decodeMultiple = Json.Decode.list
        }


{-| A `Backend` which decodes the kind of responses a Drupal backend sends.
-}
drupalBackend : Backend a
drupalBackend =
    withDrupalResponses backend


{-| Specify how to unwrap responses produced by the backend.

  - The first parameter is used for functions like `get`, which return only one
    value. So, the question is: given what the backend sends, what do we need to
    decode to get an item to which we can apply the `key` or `value` decoders?

    If the decoders actually operate on exactly what the backend returns, you
    could supply `identity`.

  - The second parameter is used for functions like `select`, which return a
    list of values. So, the question is: given what the backend sends, what do
    we need to decode to get a list of items to which we can apply the `key` or
    `value` decoders?

    If the backend actually returns just a JSON array of the things the decoders
    can handle, you could just supply `Json.Decode.list`.

For a pre-built version that handles how Drupal sends responses, see `withDrupalResponses`.

-}
withResponses : (Decoder a -> Decoder a) -> (Decoder a -> Decoder (List a)) -> Backend b -> Backend a
withResponses decodeSingle decodeMultiple (Backend backend) =
    Backend
        { backend
            | decodeSingle = decodeSingle
            , decodeMultiple = decodeMultiple
        }


{-| Unwrap responses the Drupal way.

  - Single responses are sent as the first element of a JSON array, inside a
    field called "data".

  - Multiple responses are sent as a JSON array, inside a field called "data".

So, this is equivalent to something like:

    withResponses
        (field "data" << index 0)
        (field "data" << list)

-}
withDrupalResponses : Backend a -> Backend b
withDrupalResponses =
    withResponses decodeDrupalSingle decodeDrupalList


{-| Unwrap responses in the simplest possible way:

  - Single responses are sent in a way that your decoders can handle directly.

  - Multiple responses are sent as a JSON array of things your decoders can
    handle directly.

So, this is equivalent to:

    withResponses identity Json.Decode.list

-}
withPlainResponses : Backend a -> Backend b
withPlainResponses =
    withResponses identity list


{-| Use the supplied backend with the endpoint.
-}
withBackend : Backend ( k, v ) -> EndPoint e p k v c -> EndPoint e p k v c
withBackend (Backend backend) (EndPoint endpoint) =
    EndPoint
        { endpoint
            | decodeSingle = backend.decodeSingle
            , decodeMultiple = backend.decodeMultiple
        }


{-| Use the specified `key` type with this endpoint.

The first parameter is a decoder for your `key` type, given the JSON the
backend returns for each item. If you're using a kind of `EntityId`, for the
`key`, then you can just supply `decodeEntityId`.

The second parameter helps construct the URL for cases where the `key` is
included in the URL (e.g. PUT, PATCH or DELETE). Given your `key`, what should
we put after the endpoint's `path`?

The third parameter must be provided even if it hasn't changed, for complicated
reasons that I'll blog about someday (the lack of Rank-N types).

-}
withKeyType : Decoder key -> (key -> String) -> Backend ( key, v ) -> EndPoint e p k v c -> EndPoint e p key v c
withKeyType decodeKey keyToUrlPart (Backend backend) (EndPoint endpoint) =
    EndPoint
        { endpoint
            | decodeSingle = backend.decodeSingle
            , decodeMultiple = backend.decodeMultiple
            , decodeKey = decodeKey
            , keyToUrlPart = keyToUrlPart
        }


{-| Use the specified `value` type with this endpoint.

The first parameter is a decoder for your `value` type, given the JSON the
backend returns for each item.

The second parameter is an encoder for your `value` type, for use in `PUT`
requests.

The third parameter must be provided even if it hasn't changed, for complicated
reasons that I'll blog about someday (the lack of Rank-N types).

-}
withValueType : Decoder value -> (value -> Value) -> Backend ( k, value ) -> EndPoint e p k v c -> EndPoint e p k value c
withValueType decodeValue encodeValue (Backend backend) (EndPoint endpoint) =
    EndPoint
        { endpoint
            | decodeSingle = backend.decodeSingle
            , decodeMultiple = backend.decodeMultiple
            , decodeValue = decodeValue
            , encodeValue = encodeValue
        }


{-| Use the supplied function to convert your `params` type into something we can feed to
`HttpBuilder.withQueryParams`. So, you get type-safety for the params!

`endpoint` and `drupalEndpoint` both default this to `always []` (i.e. no params)

-}
withParamsType : (params -> List ( String, String )) -> EndPoint e p k v c -> EndPoint e params k v c
withParamsType encodeParams (EndPoint endpoint) =
    EndPoint { endpoint | encodeParams = encodeParams }


{-| Use the supplied function to encode new `created` values for the endpoint,
for use in POST requests.

This is for cases where some values are supplied by the backend after the
entity is created. So, they are part of your `value` type, but you can't send
them as part of a POST request.

You can just use the same encoder as for `withValueType` if POST is not
special.

-}
withCreatedType : (created -> Value) -> EndPoint e p k v c -> EndPoint e p k v created
withCreatedType encodeCreatedValue (EndPoint endpoint) =
    EndPoint { endpoint | encodeCreatedValue = encodeCreatedValue }


{-| Use the supplied function to convert an `Http.Error` to your desired `error` type.
-}
withErrorType : (Error -> error) -> EndPoint e p k v c -> EndPoint error p k v c
withErrorType mapError (EndPoint endpoint) =
    EndPoint { endpoint | mapError = mapError }


{-| Use the supplied `path` for this endpoint.

The path is appenend to whatever you supply for the `BackendUrl` for a request.

-}
withPath : String -> EndPoint e p k v c -> EndPoint e p k v c
withPath path (EndPoint endpoint) =
    EndPoint { endpoint | path = path }


{-| Use the supplied token strategy for this endpoint.

You can use `tokenHeader` or `tokenUrlParam` to construct a `TokenStrategy.

-}
withTokenStrategy : TokenStrategy -> EndPoint e p k v c -> EndPoint e p k v c
withTokenStrategy tokenStrategy (EndPoint endpoint) =
    EndPoint { endpoint | tokenStrategy = tokenStrategy }


{-| Construct a Drupal-oriented endpoint, with as many defaults filled in as possible.

  - The first parameter is the `path` to the endpoint (which will be appended to the
    `BackendUrl` you provide for requests).
  - The second parameter is a decoder for your `value` type.
  - The third parameter is an encoder for your `value` type.

Yes, just three parameters! We'll supplement that with various Drupal-oriented defaults:

  - The `key` is some kind of `EntityId`, and it can be found in an `id` field in the JSON.
    But you can change that using `withKeyType`.

  - You create values with the full `value` type (not a partial `created` type).
    But you can change that using `withCreatedType`.

  - Multiple values are returned as a JSON array inside a `data` field.
    But you can change that using `withBackend`.

  - Single values are returned as a single-elmeent JSON array, inside a `data` field.
    But you can change that using `withBackend`.

  - Your endpoint doesn't use any URL params.
    But you can change that using `withParamsType`.

  - You're not using a custom error type.
    But you can change that using `withErrorType`.

  - An access token, if provided, will be sent as a URL param named "access_token".
    But you can change that with `withTokenStrategy`.

-}
drupalEndpoint : String -> Decoder value -> (value -> Value) -> EndPoint Error p (EntityId a) value value
drupalEndpoint path decodeValue encodeValue =
    EndPoint
        { decodeKey = decodeDrupalId toEntityId
        , decodeMultiple = decodeDrupalList
        , decodeSingle = decodeDrupalSingle
        , decodeValue = decodeValue
        , encodeCreatedValue = encodeValue
        , encodeParams = always []
        , encodeValue = encodeValue
        , keyToUrlPart = fromEntityId >> toString
        , mapError = identity
        , path = path
        , tokenStrategy = TokenUrlParam "access_token"
        }


{-| Produces an `EndPoint` with very basic defaults ... it will need
customization to actually work with your endpoint.

  - The first parameter is the `path` to the endpoint (which will be appended to the
    `BackendUrl` you provide for requests).
  - The second parameter is a decoder for your `value` type.
  - The third parameter is an encoder for your `value` type.

-}
endpoint : String -> Decoder value -> (value -> Value) -> EndPoint Error p Int value value
endpoint path decodeValue encodeValue =
    EndPoint
        { decodeKey = decodeDrupalId identity
        , decodeMultiple = list
        , decodeSingle = identity
        , decodeValue = decodeValue
        , encodeCreatedValue = encodeValue
        , encodeParams = always []
        , encodeValue = encodeValue
        , keyToUrlPart = toString
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


{-| Send an `AccessToken` to the backend using the specified HTTP header.
-}
tokenHeader : String -> TokenStrategy
tokenHeader =
    TokenHeader


{-| Send an `AccessToken` to the backend using the specified parameter in the URL.
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


expectMultiple : EndPoint e p key value c -> RequestBuilder a -> RequestBuilder (List ( key, value ))
expectMultiple (EndPoint endpoint) =
    map2 (,) endpoint.decodeKey endpoint.decodeValue
        |> endpoint.decodeMultiple
        |> expectJson
        |> withExpect


expectSingle : EndPoint e p key value c -> RequestBuilder a -> RequestBuilder ( key, value )
expectSingle (EndPoint endpoint) =
    map2 (,) endpoint.decodeKey endpoint.decodeValue
        |> endpoint.decodeSingle
        |> expectJson
        |> withExpect


{-| We could avoid this if Elm had Rank-N types, because in that case
`EndPoint.decodeSingle` could remember that it is a polymorphic function.
Without that, we need to fulfill the more specific type signature in
`Endpoint.decodeSingle` ... fortunately, in the cases we need that, we
actually know the key!
-}
expectSingleWithKey : EndPoint e p key value c -> key -> RequestBuilder a -> RequestBuilder value
expectSingleWithKey (EndPoint endpoint) key =
    map2 (,) (succeed key) endpoint.decodeValue
        |> endpoint.decodeSingle
        |> map Tuple.second
        |> expectJson
        |> withExpect


{-| Select entities from an endpoint.

What we hand you is a `Result` with a list of entities, since that is the most
"natural" thing to hand back. You can convert it to a `RemoteData` easily with
a `RemoteData.fromResult` if you like.

-}
select : BackendUrl -> Maybe AccessToken -> EndPoint error params key value created -> params -> (Result error (List ( key, value )) -> msg) -> Cmd msg
select backendUrl accessToken ((EndPoint endpoint) as ep) params tagger =
    HttpBuilder.get (backendUrl </> endpoint.path)
        |> withQueryParams (endpoint.encodeParams params)
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectMultiple ep
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Gets a entity from the backend via its ID.

If we get a 404 error, we'll give you an `Ok Nothing`, rather than an error,
since the request essentially succeeded ... there merely was no entity with
that ID. If you'd prefer an error in that situation, you can use `get404`
instead.

-}
get : BackendUrl -> Maybe AccessToken -> EndPoint error params key value created -> key -> (Result error (Maybe ( key, value )) -> msg) -> Cmd msg
get backendUrl accessToken ((EndPoint endpoint) as ep) key tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.get
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingle ep
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
get404 : BackendUrl -> Maybe AccessToken -> EndPoint error params key value created -> key -> (Result error ( key, value ) -> msg) -> Cmd msg
get404 backendUrl accessToken ((EndPoint endpoint) as ep) key tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.get
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingle ep
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Sends a `POST` request to create the specified value.
-}
post : BackendUrl -> Maybe AccessToken -> EndPoint error params key value created -> value -> (Result error ( key, value ) -> msg) -> Cmd msg
post backendUrl accessToken ((EndPoint endpoint) as ep) value tagger =
    (backendUrl </> endpoint.path)
        |> HttpBuilder.post
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingle ep
        |> withJsonBody (endpoint.encodeValue value)
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Sends a `PUT` request to create the specified value.

Assumes that the backend will respond with the full value. If that's not true, you
can use `put_` instead.

-}
put : BackendUrl -> Maybe AccessToken -> EndPoint error params key value created -> key -> value -> (Result error value -> msg) -> Cmd msg
put backendUrl accessToken ((EndPoint endpoint) as ep) key value tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.put
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingleWithKey ep key
        |> withJsonBody (endpoint.encodeValue value)
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Like `put`, but ignores any value sent by the backend back ... just interprets errors.
-}
put_ : BackendUrl -> Maybe AccessToken -> EndPoint error params key value created -> key -> value -> (Result error () -> msg) -> Cmd msg
put_ backendUrl accessToken ((EndPoint endpoint) as ep) key value tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.put
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> withJsonBody (endpoint.encodeValue value)
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Sends a `PATCH` request for the specified key and value.

Now, the point of a `PATCH` request is that you're not sending the **full** value,
but some subset. So, you supply your own JSON value, rather than using the one that
the endpoint would create use for PUT or POST. (We could have a separate config for
each kind of PATCH, which would contribute to type-safety, but is possibly overkill).

This function assumes that the backend will send the full value back. If it won't, then
you can use `patch_` instead.

-}
patch : BackendUrl -> Maybe AccessToken -> EndPoint error params key value created -> key -> Value -> (Result error value -> msg) -> Cmd msg
patch backendUrl accessToken ((EndPoint endpoint) as ep) key value tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.patch
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> expectSingleWithKey ep key
        |> withJsonBody value
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Like `patch`, but doesn't try to decode the response ... just reports errors.
-}
patch_ : BackendUrl -> Maybe AccessToken -> EndPoint error params key value created -> key -> Value -> (Result error () -> msg) -> Cmd msg
patch_ backendUrl accessToken ((EndPoint endpoint) as ep) key value tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.patch
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> withJsonBody value
        |> send (Result.mapError endpoint.mapError >> tagger)


{-| Delete entity.
-}
delete : BackendUrl -> Maybe AccessToken -> EndPoint error params key value created -> key -> (Result error () -> msg) -> Cmd msg
delete backendUrl accessToken ((EndPoint endpoint) as ep) key tagger =
    urlForKey backendUrl ep key
        |> HttpBuilder.delete
        |> withAccessToken endpoint.tokenStrategy accessToken
        |> send (Result.mapError endpoint.mapError >> tagger)


decodeDrupalId : (Int -> a) -> Decoder a
decodeDrupalId wrapper =
    map wrapper (field "id" decodeInt)


decodeDrupalData : Decoder a -> Decoder a
decodeDrupalData =
    field "data"


decodeDrupalSingle : Decoder a -> Decoder a
decodeDrupalSingle =
    decodeDrupalData << index 0


decodeDrupalList : Decoder a -> Decoder (List a)
decodeDrupalList =
    decodeDrupalData << list


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


urlForKey : BackendUrl -> EndPoint error params key value created -> key -> String
urlForKey backendUrl (EndPoint endpoint) key =
    backendUrl </> endpoint.path </> endpoint.keyToUrlPart key
