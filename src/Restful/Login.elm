module Restful.Login
    exposing
        ( AppConfig
        , Authenticated
        , Config
        , Credentials
        , LoginError(..)
        , LoginMethod(..)
        , LoginProgress(..)
        , Msg
        , UserStatusAndData(..)
        , accessTokenAccepted
        , accessTokenRejected
        , checkCachedCredentials
        , drupalConfig
        , getData
        , getError
        , hasAccessToken
        , hasValidAccessToken
        , isProgressing
        , loggedOut
        , logout
        , mapAnonymousData
        , mapAuthenticatedData
        , mapBoth
        , maybeAnonymousData
        , maybeAuthenticatedData
        , tryLogin
        , update
        )

{-| This module models the state associated with the login process,
but not the UI -- the idea is that the UI will vary more than the basic logic
of logging in does.

Of course, this model will probably be supplied as a parameter to functions
that manipulate the UI, and those functions will probably return messages that
can be handled here.


## Types

@docs UserStatusAndData, Credentials, Authenticated, LoginProgress, LoginMethod, LoginError


## Initialization

@docs loggedOut, checkCachedCredentials


## Actions

@docs tryLogin, logout, accessTokenRejected, accessTokenAccepted


## Integration with your app

@docs Config, AppConfig, drupalConfig, Msg, update


## Accessing the data associated with the login

@docs maybeAnonymousData, maybeAuthenticatedData, getData
@docs mapAnonymousData, mapAuthenticatedData, mapBoth
@docs hasAccessToken, hasValidAccessToken, getError, isProgressing

-}

import Base64
import Http exposing (Error(..), expectJson)
import HttpBuilder exposing (withExpect, withHeader, withQueryParams)
import Json.Decode as JD exposing (Decoder, field)
import Json.Encode exposing (Value)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing ((</>), AccessToken, BackendUrl)
import Task


{-| What a successful login ultimately results in is:

-- an access token
-- which is valid for a particular backend URL
-- and some information about the logged-in user

We parameterize the user information, since it will vary from app to app.

You'll have to supply the backendUrl, of course, but it's handy to group it
here with the credentials, since you'll often want to supply the backend URL
and access token together as a parameter. And, this accessToken is valid for
this backendUrl.

-}
type alias Credentials user =
    { accessToken : AccessToken
    , backendUrl : BackendUrl
    , user : user
    }


{-| We don't necessarily cache the full credentials record ... we only cache
the user if our config has an `encodeUser`. So, this represents what we might
get from the cache.
-}
type alias CachedCredentials user =
    { accessToken : AccessToken
    , backendUrl : BackendUrl
    , user : Maybe user
    }


maybeCredentials : CachedCredentials user -> Maybe (Credentials user)
maybeCredentials cached =
    Maybe.map
        (\user ->
            { accessToken = cached.accessToken
            , backendUrl = cached.backendUrl
            , user = user
            }
        )
        cached.user


{-| Models the state of the login process, from beginning to end.

  - `user` is the type we use to model information about the user
    (e.g. name etc.). If we have a cached access token, we check whether
    it's still valid by making a request for user data.

  - `authenticatedData` is a type for data that we only keep for logged-in users. It
    facilitates forgetting that data when we logout ... it's baked into the type.
    If you don't want to bother with that, you can use a Tuple0 here ... that is, `()`.

  - `anonymousData` is a type for data that we only keep for anonymous users. It
    facilitates forgetting that data when we login ... it's baked into the type.
    If you don't want to bother with that, you can use a Tuple0 here ... that is, `()`.

Note that you would not use `anonymousData` for data that is equally valid
whether you're logged in or logged out. That data you should manage outside of
this structure. It is only for data that you want to throw away when you
successfully login. For instance, you might have an endpoint that returns
one set of things for anonymous users and a different set of things for
logged-in users. In that case, you might want to throw away the data
relevant to anonymous users when you login.

  - AnonymousUser

    We don't have credentials (yet). The `LoginProgress` parameter indicates
    whatever progress we might be making towards having credentials.

    We could parameterize this to add some data that only anonymous users use.
    However, it's probably the case that any such data would also be relevant
    for logged-in users, so it's probably not necessary -- it can be handled
    at a higher level.

  - AuthenticatedUser

    We've got credentials. In addition to the credentials themsevles, we track
    what we know about the validity of the credentials, and any app-specific
    data that only applies where we have credentials (i.e. that should be
    thrown away upon logout).

-}
type UserStatusAndData user anonymousData authenticatedData
    = AnonymousUser (Maybe (LoginProgress user)) anonymousData
    | AuthenticatedUser (Authenticated user authenticatedData)


{-| Is there currently something in progress to advance the login process, or
are we at rest?
-}
isProgressing : UserStatusAndData user anonymous authenticated -> Bool
isProgressing model =
    case model of
        AnonymousUser progress data ->
            Maybe.map loginProgressIsProgressing progress
                |> Maybe.withDefault False

        AuthenticatedUser login ->
            Maybe.map loginProgressIsProgressing login.relogin
                |> Maybe.withDefault False


loginProgressIsProgressing : LoginProgress user -> Bool
loginProgressIsProgressing loginProgress =
    case loginProgress of
        Checking _ ->
            True

        LoginError _ ->
            False


{-| Do we have an error to report?
-}
getError : UserStatusAndData user anonymous authenticated -> Maybe (LoginError user)
getError model =
    Maybe.andThen loginProgressToError <|
        case model of
            AnonymousUser progress data ->
                progress

            AuthenticatedUser login ->
                login.relogin


loginProgressToError : LoginProgress user -> Maybe (LoginError user)
loginProgressToError loginProgress =
    case loginProgress of
        Checking _ ->
            Nothing

        LoginError err ->
            Just err


{-| Extract the authenticated data as a Maybe, which will be `Just` if the user is logged in.
-}
maybeAuthenticatedData : UserStatusAndData user anonymous authenticated -> Maybe authenticated
maybeAuthenticatedData model =
    case model of
        AnonymousUser _ _ ->
            Nothing

        AuthenticatedUser login ->
            Just login.data


{-| Extract the anonymous data as a Maybe, which will be `Just` if the user is not logged in.
-}
maybeAnonymousData : UserStatusAndData user anonymous authenticated -> Maybe anonymous
maybeAnonymousData model =
    case model of
        AnonymousUser _ data ->
            Just data

        AuthenticatedUser login ->
            Nothing


{-| Extract the data by turning either anonymous data or authenticated data
into a common data type. This could be useful if there are some common elements
to your `anonymous` and `authenticated` data types. Though, remember that
you should manage **truly** common data outside of these structures ...
this is only for data which you want to throw away when transitioning from
logged in to logged out and vice versa.

If you only want to deal with `authenticated` or `anonymous` data and leave the
other possibility alone, see the `maybeAuthenticatedData` and `maybeAnonymousData`
functions.

-}
getData : (anonymous -> a) -> (authenticated -> a) -> UserStatusAndData user anonymous authenticated -> a
getData anonFunc authenticatedFunc model =
    case model of
        AnonymousUser _ data ->
            anonFunc data

        AuthenticatedUser login ->
            authenticatedFunc login.data


{-| Map over the authenticated data, if the user is logged in.
-}
mapAuthenticatedData : (authenticated -> authenticated) -> UserStatusAndData user anonymous authenticated -> UserStatusAndData user anonymous authenticated
mapAuthenticatedData func model =
    case model of
        AnonymousUser _ _ ->
            model

        AuthenticatedUser login ->
            AuthenticatedUser { login | data = func login.data }


{-| Map over the anonymous data, if the user is not logged in.
-}
mapAnonymousData : (anonymous -> anonymous) -> UserStatusAndData user anonymous authenticated -> UserStatusAndData user anonymous authenticated
mapAnonymousData func model =
    case model of
        AnonymousUser progress data ->
            AnonymousUser progress (func data)

        AuthenticatedUser login ->
            model


{-| Map over the data, choosing the mapping function depending on whether the
user is logged in or not.
-}
mapBoth : (anonymous -> anonymous) -> (authenticated -> authenticated) -> UserStatusAndData user anonymous authenticated -> UserStatusAndData user anonymous authenticated
mapBoth anonFunc authenticatedFunc model =
    case model of
        AnonymousUser progress data ->
            AnonymousUser progress (anonFunc data)

        AuthenticatedUser login ->
            AuthenticatedUser { login | data = authenticatedFunc login.data }


{-| Represents the status of an attempt to login.

  - Checking

    We have sent a request to the backend and are waiting for a response. The
    `LoginMethod` indicates whether we're checking an acccess token or a
    username/password combination.

  - LoginError

    We got a response from the backend with an error.

-}
type LoginProgress user
    = Checking LoginMethod
    | LoginError (LoginError user)


{-| An error which has occurred in the login process.

  - Rejected

    We got a definite response from the backend rejecting our login request ...
    that is, our access token or username/password was firmly rejected.

  - HttpError

    We got some other HTTP error. That is, the backend did not definitely
    indicate that our credentials are invalid, but some other sort of HTTP
    error occurred. If the error might be transient ... that is, if
    retrying might help ... then we include a `msg` you can send in
    order to retry.

-}
type LoginError user
    = Rejected LoginMethod
    | HttpError LoginMethod Error (Maybe (Msg user))


{-| How are we trying to login? Are we checking an access token, or are we sending
a username and password?
-}
type LoginMethod
    = ByAccessToken
    | ByPassword


{-| Represents the data we have if we're logged in.

  - credentials

    What credentials did we log in with?

  - logout

    Tracks a request-in-progress to logout. In some cases, we need to contact
    the server in order to logout, because it maintains an HTTP-only session
    cookie which we can only delete via an HTTP request.

  - relogin

    Do we need to re-login? If our credentials are rejected, we don't
    transition back to `AnonymousUser` immediately, since that would prematurely
    throw away some information that we may want to keep. Instead, we mark that
    relogin is `Just LoginRequired`. We can then track the relogin process
    without disturbing the other data.

    Note that we shouldn't switch relogin to `Just` if there some kind of
    transient network error ... only if our access token is definitely
    rejected because it is permanently invalid.

  - data

    The app-specific data that only pertains to logged-in users, which we should
    throw away when the user logs out.

-}
type alias Authenticated user data =
    { credentials : Credentials user
    , logout : WebData ()
    , relogin : Maybe (LoginProgress user)
    , data : data
    }


{-| Given that some progress has been made towards login (or re-login),
reflect that progress in our model.

This won't flip you from `AnonymousUser` to `AuthenticatedUser` or vice-versa ... it
only records **partial** progress.

However, if you are `AuthenticatedUser`, this will make `relogin` a `Just` ...
that is, it will record that relogin is necessary.

-}
setProgress : Maybe (LoginProgress user) -> UserStatusAndData user anonymous authenticated -> UserStatusAndData user anonymous authenticated
setProgress progress model =
    case model of
        AnonymousUser _ data ->
            AnonymousUser progress data

        AuthenticatedUser login ->
            AuthenticatedUser
                { login | relogin = progress }


{-| Record successfuly obtained credentials. The config will be used
to generate initial data if we didn't have some already (i.e. if this isn't
a re-login). If it is a re-login, we just keep the data.
-}
setCredentials : Config user anonymous authenticated msg -> Credentials user -> UserStatusAndData user anonymous authenticated -> UserStatusAndData user anonymous authenticated
setCredentials config credentials model =
    case model of
        AnonymousUser _ _ ->
            AuthenticatedUser
                { credentials = credentials
                , logout = NotAsked
                , relogin = Nothing
                , data = config.initialAuthenticatedData credentials.user
                }

        AuthenticatedUser login ->
            AuthenticatedUser
                { credentials = credentials
                , logout = NotAsked
                , relogin = Nothing
                , data = login.data
                }


{-| Use the supplied cached credentials, which may or may not have a cached user.
-}
setCachedCredentials : Config user anonymous authenticated msg -> CachedCredentials user -> UserStatusAndData user anonymous authenticated -> UserStatusAndData user anonymous authenticated
setCachedCredentials config cached model =
    case maybeCredentials cached of
        Just credentials ->
            setCredentials config credentials model

        Nothing ->
            model


{-| A status which represents the state in which the user is logged out,
no progress is currently being made towards login, and we start with the
initial data specified in our config.

This is one possible "starting point" for initializing the UserStatusAndData. The other
main starting point would be `checkCachedCredentials`.

Note that you should use `logout` to actually perform the action of logging
out, since that will also clear the cached credentials.

-}
loggedOut : Config user anonymous authenticated msg -> UserStatusAndData user anonymous authenticated
loggedOut config =
    AnonymousUser Nothing config.initialAnonymousData


{-| Initializes a UserStatusAndData by indicating that we're checking cached credentials
against the backend, and return a `Cmd` that will do that.

  - BackendUrl is the backend to check the cached credentials against.

  - The `String` parameter is the JSON string which your `cacheCredentials`
    function (from Config) has cached. So, it's up to you to fetch that value
    somehow, either via flags at startup, or via ports. If you've cached
    credentials for multiple backends, it's up to you to match your backendURL
    and your credentials.

The UserStatusAndData will start as `AnonymousUser (Just (Checking ByAccessToken))`. At this
point, your UI should treat the login process as unresolved ... it will soon
resolve one way or another. So, you might show a "checking for cached login"
message, or just nothing.

  - If we can decode the credentials, we'll try to use the access token against
    the backend to get updated user information. Whether or not that succeeds,
    we'll be in `AuthenticatedUser` state ... the result of checking the credentials will
    affect whether `relogin` is required.

  - If we can't decode the credentials, we'll be in `AnonymousUser (Just progress)`
    state, where `progress` will indicate the error we received.

-}
checkCachedCredentials : Config user anonymous authenticated msg -> BackendUrl -> String -> ( UserStatusAndData user anonymous authenticated, Cmd msg )
checkCachedCredentials config backendUrl value =
    let
        -- The third return parameter will necessarily be false, since we're
        -- just kicking off the credential check here.
        ( userStatus, cmd, _ ) =
            update config (CheckCachedCredentials backendUrl value) (AnonymousUser (Just (Checking ByAccessToken)) config.initialAnonymousData)
    in
    ( userStatus, cmd )


{-| Some static configuration which we need to integrate with your app.
You should be able to define this once, as a constant, and then use it
where needed.

  - user

    The type you use for your user.

  - data

    The type of data that is only for users ... that is, which you'd like
    to throw away upon logout.

  - msg

    Your Msg type.

  - loginPath

    Relative to a backendUrl, what's the path to the endpoint for getting an
    access token? e.g. "api/login-token"

  - logoutPath

    Relative to a backendUrl, what's the path we can send a GET to in order
    to logout? E.g. to destroy a session cookie, if it's HTTP only, so we can't
    destroy it from Javascript.

  - userPath

    Once we have an access token, what's the path to the endpoint from which we
    can request information about the current user?

  - decodeAccessToken

    A decoder for an access token, given the response from the loginPath

  - decodeUser

    A decoder for the `user` type, as send from userPath.

  - encodeUser

    A function that will produce JSON that `decodeUser` can decode. This is
    used to cache the `user` object in local storage along with the access
    token. This would mainly be useful if you want to remember who was last
    logged in when your app is offline. If you don't need to do that, you could
    supply `Nothing` here.

  - initialAuthenticatedData

    Given the newly logged-in user, what initial data should we start with for that
    user?

  - initialAnonymousData

    If we have no logged in user, or if we log out, what data should we start with?

  - cacheCredentials

    A function which, when given a backendURL and a JSON string, will return a
    command that caches that string. Exactly how you do that is up to you ... it
    will probably be via ports.

    We provide the backendUrl in case you want to store the credentials for multiple
    backends and pick amongst them when needed. But you can ignore it if you like ...
    the important part for us is the JSON string.

    However you store the JSON string, you can provide it to `checkCachedCredentials`
    and we'll use it.

  - tag

    What tag do you use in your `Msg` type to wrap our `Msg` type? This allows
    our `update` function to work in the context of your `Msg` type ... in
    effect, we'll do the mapping, rather than making you do it.

-}
type alias Config user anonymous authenticated msg =
    { loginPath : String
    , logoutPath : Maybe String
    , userPath : String
    , decodeAccessToken : Decoder AccessToken
    , decodeUser : Decoder user
    , encodeUser : Maybe (user -> Value)
    , initialAuthenticatedData : user -> authenticated
    , initialAnonymousData : anonymous
    , cacheCredentials : BackendUrl -> String -> Cmd msg
    , tag : Msg user -> msg
    }


{-| The parts of `Config` that tend to vary from one app to the next.
-}
type alias AppConfig user anonymous authenticated msg =
    { decodeUser : Decoder user
    , encodeUser : Maybe (user -> Value)
    , initialAuthenticatedData : user -> authenticated
    , initialAnonymousData : anonymous
    , cacheCredentials : BackendUrl -> String -> Cmd msg
    , tag : Msg user -> msg
    }


{-| Make a `Config` that uses default values oriented towards Drupal's
restful implementation.
-}
drupalConfig : AppConfig user anonymous authenticated msg -> Config user anonymous authenticated msg
drupalConfig appConfig =
    { loginPath = "api/login-token"
    , logoutPath = Just "user/logout"
    , userPath = "api/me"
    , decodeAccessToken = field "access_token" JD.string
    , decodeUser = appConfig.decodeUser
    , encodeUser = appConfig.encodeUser
    , initialAnonymousData = appConfig.initialAnonymousData
    , initialAuthenticatedData = appConfig.initialAuthenticatedData
    , cacheCredentials = appConfig.cacheCredentials
    , tag = appConfig.tag
    }


{-| An opaque type representing messages we handle. You can create
these messages with various functions (e.g. `tryLogin`, `logout`) and handle
them with the `update` function.
-}
type Msg user
    = CheckCachedCredentials BackendUrl String
    | HandleAccessTokenCheck (Msg user) (CachedCredentials user) (Result Error user)
    | HandleLoginAttempt (Msg user) (Result Error (Credentials user))
    | HandleLogoutAttempt (Result Error ())
    | Logout
    | TryLogin BackendUrl (List ( String, String )) String String


{-| Message which will try logging in against the specified backendUrl

  - The second parameter is a list of query params to add the URL. (Typically,
    you won't need this, so you can supply an empty list.

  - The third parameter is the username.

  - The fourth parameter is the password.

-}
tryLogin : BackendUrl -> List ( String, String ) -> String -> String -> Msg user
tryLogin =
    TryLogin


{-| Message which will log out and clear cached credentials.
-}
logout : Msg user
logout =
    Logout


{-| Specializes an HTTP error to our `LoginError` type.

The first parameter is a `msg` we could send to retry the request.

-}
classifyHttpError : Maybe (Msg user) -> LoginMethod -> Error -> LoginError user
classifyHttpError retry method error =
    case error of
        Http.BadUrl _ ->
            HttpError method error Nothing

        Http.Timeout ->
            HttpError method error retry

        Http.NetworkError ->
            HttpError method error retry

        Http.BadStatus response ->
            if response.status.code == 401 then
                Rejected method
            else
                HttpError method error Nothing

        Http.BadPayload _ _ ->
            HttpError method error Nothing


{-| Our update function. Note that the `Cmd` we return is in terms of
your own msg type. So, you can integrate it into your app roughly
as follows:

    ...

The third return parameter will be `True` at the very moment at which
a successful login has been made. But only at that very moment ... it's
not reflecting state, but instead a kind of notification that we've
just logged in.

-}
update : Config user anonymous authenticated msg -> Msg user -> UserStatusAndData user anonymous authenticated -> ( UserStatusAndData user anonymous authenticated, Cmd msg, Bool )
update config msg model =
    case msg of
        HandleLoginAttempt retry result ->
            case result of
                Err err ->
                    ( setProgress (Just (LoginError (classifyHttpError (Just retry) ByPassword err))) model
                    , Cmd.none
                    , False
                    )

                Ok credentials ->
                    ( setCredentials config credentials model
                    , config.cacheCredentials credentials.backendUrl (encodeCredentials config credentials)
                    , True
                    )

        TryLogin backendUrl params name password ->
            let
                -- TODO: Perhaps the login method ought to be parameterized in the config,
                -- with this as a default?
                credentials =
                    Base64.encode (name ++ ":" ++ password)

                requestAccessToken =
                    HttpBuilder.get (backendUrl </> config.loginPath)
                        |> withHeader "Authorization" ("Basic " ++ credentials)
                        |> withQueryParams params
                        |> withExpect (expectJson config.decodeAccessToken)
                        |> HttpBuilder.toTask

                requestUser accessToken =
                    HttpBuilder.get (backendUrl </> config.userPath)
                        |> withQueryParams [ ( "access_token", accessToken ) ]
                        |> withExpect (expectJson config.decodeUser)
                        |> HttpBuilder.toTask
                        |> Task.map
                            (\user ->
                                { accessToken = accessToken
                                , backendUrl = backendUrl
                                , user = user
                                }
                            )

                cmd =
                    requestAccessToken
                        |> Task.andThen requestUser
                        |> Task.attempt (HandleLoginAttempt msg)
            in
            ( setProgress (Just (Checking ByPassword)) model
            , Cmd.map config.tag cmd
            , False
            )

        HandleAccessTokenCheck retry credentials result ->
            case result of
                Err err ->
                    ( setCachedCredentials config credentials model
                        |> retryAccessTokenRejected (Just retry) err
                    , Cmd.none
                    , True
                    )

                Ok user ->
                    ( setCachedCredentials config { credentials | user = Just user } model
                    , Cmd.none
                    , True
                    )

        CheckCachedCredentials backendUrl cachedValue ->
            case JD.decodeString (decodeCachedCredentials config backendUrl) cachedValue of
                Err _ ->
                    -- If we can't decode the cached credentials, we just
                    -- give up and say that login is needed. This will, for
                    -- instance, happen where we had logged out and cleared
                    -- the cached credentials.
                    ( setProgress Nothing model
                    , Cmd.none
                    , False
                    )

                Ok credentials ->
                    -- If we have credentials, then we will check the access
                    -- token against the backend, to make sure that it is still
                    -- valid. Any error will result in a `relogin` being
                    -- recorded.
                    let
                        cmd =
                            HttpBuilder.get (backendUrl </> config.userPath)
                                |> withQueryParams [ ( "access_token", credentials.accessToken ) ]
                                |> withExpect (expectJson config.decodeUser)
                                |> HttpBuilder.toTask
                                |> Task.attempt (HandleAccessTokenCheck msg credentials)
                                |> Cmd.map config.tag
                    in
                    -- We're still anonymous, until we get the second answer
                    ( model, cmd, False )

        Logout ->
            case model of
                AnonymousUser _ _ ->
                    ( model
                    , Cmd.none
                    , False
                    )

                AuthenticatedUser login ->
                    case config.logoutPath of
                        Just logoutPath ->
                            -- See comment below ... in this case, we can't
                            -- really logout unless we send a request to the
                            -- backend to do so.
                            ( AuthenticatedUser { login | logout = Loading }
                            , HttpBuilder.get (login.credentials.backendUrl </> logoutPath)
                                |> withQueryParams [ ( "access_token", login.credentials.accessToken ) ]
                                |> HttpBuilder.toTask
                                |> Task.attempt HandleLogoutAttempt
                                |> Cmd.map config.tag
                            , False
                            )

                        Nothing ->
                            -- In this case, we can just forget our credentials
                            -- locally.  We'll just call ourselves recursively
                            -- as if the logout request succeeded.
                            update config (HandleLogoutAttempt (Ok ())) model

        -- If there is an HTTP-only session cookie, and you're serving the app
        -- from a sub-path on the Drupal site, then you can only **really**
        -- logout if you're online and we get a successful response here. The
        -- reason is that the session cookie can only be deleted via HTTP, and
        -- that won't have happened unless your request succeeds here. Otherwise,
        -- we'll still have the ssession cookie, and future attempts to login
        -- will simply use the existing session.
        --
        -- For this reason, we only locally record a logout when this request
        -- succeeds ... otherwise, we show an error.
        HandleLogoutAttempt result ->
            let
                -- A 403 Forbidden response is actually success, in this case!
                adjustedResult =
                    case result of
                        Err (BadStatus response) ->
                            if response.status.code == 403 then
                                Ok ()
                            else
                                result

                        _ ->
                            result
            in
            case ( adjustedResult, model ) of
                ( Ok _, AuthenticatedUser login ) ->
                    -- We tell the app to cache credentials consisting of an empty object.
                    -- This is simpler than telling the app to delete credentials.
                    ( loggedOut config
                    , config.cacheCredentials login.credentials.backendUrl "{}"
                    , False
                    )

                ( Err err, AuthenticatedUser login ) ->
                    -- Just record the error
                    ( AuthenticatedUser { login | logout = Failure err }
                    , Cmd.none
                    , False
                    )

                _ ->
                    -- If we weren't logged in anyway, there's nothing to do.
                    ( model, Cmd.none, False )


encodeCredentials : Config user anonymous authenticated msg -> Credentials user -> String
encodeCredentials config credentials =
    -- We only encode the accessToken and the user ... we provide the
    -- backendURL separately, so the app can decide whether to record
    -- this separately for different configured backends etc.
    --
    -- We only encode the user if our config has an `encodeUser` ... otherwise,
    -- we leave it out. So, you can decide whether to store the user in local
    -- storage or not ... if not, you can't get a user until you're online and
    -- can contact the backend.
    let
        encodedAccessToken =
            Just ( "access_token", Json.Encode.string credentials.accessToken )

        encodedUser =
            Maybe.map (\encoder -> ( "user", encoder credentials.user )) config.encodeUser
    in
    [ encodedAccessToken, encodedUser ]
        |> List.filterMap identity
        |> Json.Encode.object
        |> Json.Encode.encode 0


decodeCachedCredentials : Config user anonymous authenticated msg -> BackendUrl -> Decoder (CachedCredentials user)
decodeCachedCredentials config backendUrl =
    let
        decodeAccessToken =
            field "access_token" JD.string

        decodeUser =
            JD.oneOf
                [ JD.map Just <| field "user" config.decodeUser
                , JD.succeed Nothing
                ]
    in
    JD.map2
        (\accessToken user ->
            { backendUrl = backendUrl
            , accessToken = accessToken
            , user = user
            }
        )
        decodeAccessToken
        decodeUser


{-| As far as we know, do we have a still-valid access token?

If we don't know yet, we indicate `False`.

-}
hasValidAccessToken : UserStatusAndData user anonymous authenticated -> Bool
hasValidAccessToken status =
    case status of
        AnonymousUser _ _ ->
            False

        AuthenticatedUser login ->
            login.relogin == Nothing


{-| Do we have an access token, whether or not we think it's valid?

If we're still checking, we say `False`.

-}
hasAccessToken : UserStatusAndData user anonymous authenticated -> Bool
hasAccessToken status =
    case status of
        AnonymousUser _ _ ->
            False

        AuthenticatedUser login ->
            True


{-| Record the fact that our access token was rejected.

If we're in a `AuthenticatedUser` state, we'll stay in that state ... we'll
merely record that re-login is required.

-}
accessTokenRejected : Error -> UserStatusAndData user anonymous authenticated -> UserStatusAndData user anonymous authenticated
accessTokenRejected =
    retryAccessTokenRejected Nothing


{-| Internal version of accessTokenRejected that will keep track of a msg
we can use to retry.
-}
retryAccessTokenRejected : Maybe (Msg user) -> Error -> UserStatusAndData user anonymous authenticated -> UserStatusAndData user anonymous authenticated
retryAccessTokenRejected retry error =
    setProgress (Just <| LoginError <| classifyHttpError retry ByAccessToken error)


{-| If you previously recorded `accessTokenRejected` but it was a transient
problem, and now it has been accepted, you can record that with this function.

You don't need to call this every time the access token is accepted (though
it won't do any harm, either).

Note that this doesn't switch our state from `AnonymousUser` to `AuthenticatedUser` ...
it only resets `AuthenticatedUser` (if that's what we are) to show that `relogin`
is not required.

-}
accessTokenAccepted : UserStatusAndData user anonymous authenticated -> UserStatusAndData user anonymous authenticated
accessTokenAccepted status =
    -- We return `status` unchanged as often as possible, for the sake of
    -- preserving referential equality where we can.
    case status of
        AnonymousUser _ _ ->
            status

        AuthenticatedUser login ->
            case login.relogin of
                Just _ ->
                    AuthenticatedUser { login | relogin = Nothing }

                Nothing ->
                    status
