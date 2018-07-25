module Restful.Login
    exposing
        ( AnonymousUser
        , AppConfig
        , AuthenticatedUser
        , Config
        , Credentials
        , LoginError(..)
        , LoginEvent(..)
        , LoginMethod(..)
        , LoginProgress(..)
        , Msg
        , UserAndData(..)
        , accessTokenAccepted
        , accessTokenRejected
        , checkCachedCredentials
        , drupalConfig
        , getData
        , getError
        , getLoginProgress
        , getUser
        , hasAccessToken
        , hasValidAccessToken
        , isAnonymousUser
        , isAuthenticatedUser
        , isChecking
        , isCheckingAccessToken
        , isCheckingPassword
        , loggedIn
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

@docs UserAndData, Credentials, AnonymousUser, AuthenticatedUser
@docs LoginProgress, LoginEvent, LoginMethod, LoginError


## Initialization

@docs loggedOut, checkCachedCredentials, loggedIn


## Actions

@docs tryLogin, logout, accessTokenRejected, accessTokenAccepted


## Integration with your app

@docs Config, AppConfig, drupalConfig, Msg, update


## Accessing the data associated with the login

@docs maybeAnonymousData, maybeAuthenticatedData, getData
@docs mapAnonymousData, mapAuthenticatedData, mapBoth
@docs hasAccessToken, hasValidAccessToken
@docs getError, getLoginProgress
@docs isAnonymousUser, isAuthenticatedUser
@docs isChecking, isCheckingAccessToken, isCheckingPassword
@docs getUser

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

  - Anonymous

    We don't have credentials (yet). We track any progress we are making
    towards login, as well as data that is specific to anonymous users (which
    will be thrown away when we successfully login).

  - Authenticated

    We've got credentials. In addition to the credentials themsevles, we track
    what we know about the validity of the credentials, and any app-specific
    data that only applies where we have credentials (i.e. that should be
    thrown away upon logout).

-}
type UserAndData anonymousData user authenticatedData
    = Anonymous (AnonymousUser user anonymousData)
    | Authenticated (AuthenticatedUser user authenticatedData)


{-| Is our user anonymous?
-}
isAnonymousUser : UserAndData anonymousData user authenticatedData -> Bool
isAnonymousUser model =
    case model of
        Anonymous _ ->
            True

        Authenticated _ ->
            False


{-| Is our user authenticated?
-}
isAuthenticatedUser : UserAndData anonymousData user authenticatedData -> Bool
isAuthenticatedUser model =
    case model of
        Anonymous _ ->
            False

        Authenticated _ ->
            True


{-| Gets the `user`, if we are authenticated.
-}
getUser : UserAndData anonymousData user authenticatedData -> Maybe user
getUser model =
    case model of
        Anonymous _ ->
            Nothing

        Authenticated { credentials } ->
            Just credentials.user


{-| Gets the progress we are making towards login, if any. (For cases in which
we have cached credentials, this would represent attempts to re-login where our
credentials have expired).
-}
getLoginProgress : UserAndData anonymousData user authenticatedData -> Maybe (LoginProgress user)
getLoginProgress model =
    case model of
        Anonymous { progress } ->
            progress

        Authenticated { relogin } ->
            relogin


{-| Are we waiting for a response from the backend?
-}
isChecking : UserAndData anonymousData user authenticatedData -> Bool
isChecking =
    getLoginProgress
        >> Maybe.map loginProgressIsChecking
        >> Maybe.withDefault False


{-| Are we waiting for the backend to respond to a request to check our access token?
-}
isCheckingAccessToken : UserAndData anonymousData user authenticatedData -> Bool
isCheckingAccessToken =
    getLoginProgress
        >> Maybe.map loginProgressIsCheckingAccessToken
        >> Maybe.withDefault False


{-| Are we waiting for the backend to respond to a request to check a username and password?
-}
isCheckingPassword : UserAndData anonymousData user authenticatedData -> Bool
isCheckingPassword =
    getLoginProgress
        >> Maybe.map loginProgressIsCheckingPassword
        >> Maybe.withDefault False


loginProgressIsCheckingAccessToken : LoginProgress user -> Bool
loginProgressIsCheckingAccessToken loginProgress =
    case loginProgress of
        Checking ByAccessToken ->
            True

        Checking ByPassword ->
            False

        LoginError _ ->
            False


loginProgressIsCheckingPassword : LoginProgress user -> Bool
loginProgressIsCheckingPassword loginProgress =
    case loginProgress of
        Checking ByAccessToken ->
            False

        Checking ByPassword ->
            True

        LoginError _ ->
            False


loginProgressIsChecking : LoginProgress user -> Bool
loginProgressIsChecking loginProgress =
    case loginProgress of
        Checking _ ->
            True

        LoginError _ ->
            False


{-| Do we have an error to report?
-}
getError : UserAndData anonymousData user authenticatedData -> Maybe (LoginError user)
getError =
    getLoginProgress >> Maybe.andThen loginProgressToError


loginProgressToError : LoginProgress user -> Maybe (LoginError user)
loginProgressToError loginProgress =
    case loginProgress of
        Checking _ ->
            Nothing

        LoginError err ->
            Just err


{-| Extract the authenticated data as a Maybe, which will be `Just` if the user is logged in.
-}
maybeAuthenticatedData : UserAndData anonymousData user authenticatedData -> Maybe authenticatedData
maybeAuthenticatedData model =
    case model of
        Anonymous _ ->
            Nothing

        Authenticated { data } ->
            Just data


{-| Extract the anonymous data as a Maybe, which will be `Just` if the user is not logged in.
-}
maybeAnonymousData : UserAndData anonymousData user authenticatedData -> Maybe anonymousData
maybeAnonymousData model =
    case model of
        Anonymous { data } ->
            Just data

        Authenticated _ ->
            Nothing


{-| Extract the data by turning either anonymous data or authenticated data
into a common data type. This could be useful if there are some common elements
to your `anonymous` and `authenticated` data types. Though, remember that
you should manage **truly** common data outside of these structures ...
this is only for data which you want to throw away when transitioning from
logged in to logged out and vice versa.

If you only want to deal with `authenticatedData` or `anonymousData` and leave the
other possibility alone, see the `maybeAuthenticatedData` and `maybeAnonymousData`
functions.

-}
getData : (anonymousData -> a) -> (authenticatedData -> a) -> UserAndData anonymousData user authenticatedData -> a
getData anonFunc authenticatedFunc model =
    case model of
        Anonymous { data } ->
            anonFunc data

        Authenticated { data } ->
            authenticatedFunc data


{-| Map over the authenticated data, if the user is logged in.
-}
mapAuthenticatedData : (authenticatedData -> authenticatedData) -> UserAndData anonymousData user authenticatedData -> UserAndData anonymousData user authenticatedData
mapAuthenticatedData func model =
    case model of
        Anonymous _ ->
            model

        Authenticated authenticated ->
            Authenticated { authenticated | data = func authenticated.data }


{-| Map over the anonymous data, if the user is not logged in.
-}
mapAnonymousData : (anonymousData -> anonymousData) -> UserAndData anonymousData user authenticatedData -> UserAndData anonymousData user authenticatedData
mapAnonymousData func model =
    case model of
        Anonymous anonymous ->
            Anonymous { anonymous | data = func anonymous.data }

        Authenticated _ ->
            model


{-| Map over the data, choosing the mapping function depending on whether the
user is logged in or not.
-}
mapBoth : (anonymousData -> anonymousData) -> (authenticatedData -> authenticatedData) -> UserAndData anonymousData user authenticatedData -> UserAndData anonymousData user authenticatedData
mapBoth anonFunc authenticatedFunc model =
    case model of
        Anonymous _ ->
            mapAnonymousData anonFunc model

        Authenticated _ ->
            mapAuthenticatedData authenticatedFunc model


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


{-| Our `update` method returns a third parameter to indicate moments at which
certain events occur, in case you'd like to trigger some further actions at
that moment.
-}
type LoginEvent
    = LoggedIn
    | LoggedOut


{-| Represents the data we have if we're logged in.

  - credentials

    What credentials did we log in with?

  - logout

    Tracks a request-in-progress to logout. In some cases, we need to contact
    the server in order to logout, because it maintains an HTTP-only session
    cookie which we can only delete via an HTTP request.

  - relogin

    Do we need to re-login? If our credentials are rejected, we don't
    transition back to `Anonymous` immediately, since that would prematurely
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
type alias AuthenticatedUser user data =
    { credentials : Credentials user
    , logout : WebData ()
    , relogin : Maybe (LoginProgress user)
    , data : data
    }


{-| Represents the data we have if we are not logged in.

  - progress

    Represents the progress we are making towards login, if any.

  - data

    The app-specific data that only pertains to anonymous users. This will be thrown
    away when you login, so it should not contain data that is indifferent to
    login status -- you should manage that sort of data elsewhere.

-}
type alias AnonymousUser user data =
    { progress : Maybe (LoginProgress user)
    , data : data
    }


setLoginProgress : Maybe (LoginProgress user) -> UserAndData anonymousData user authenticatedData -> UserAndData anonymousData user authenticatedData
setLoginProgress progress model =
    case model of
        Anonymous anonymous ->
            Anonymous { anonymous | progress = progress }

        Authenticated authenticated ->
            Authenticated { authenticated | relogin = progress }


{-| Record successfuly obtained credentials. The config will be used
to generate initial data if we didn't have some already (i.e. if this isn't
a re-login). If it is a re-login, we just keep the data.
-}
setCredentials : Config anonymousData user authenticatedData msg -> Credentials user -> UserAndData anonymousData user authenticatedData -> UserAndData anonymousData user authenticatedData
setCredentials config credentials model =
    case model of
        Anonymous { data } ->
            Authenticated
                { credentials = credentials
                , logout = NotAsked
                , relogin = Nothing
                , data = config.initialAuthenticatedData data credentials.user
                }

        Authenticated authenticated ->
            Authenticated
                { credentials = credentials
                , logout = NotAsked
                , relogin = Nothing
                , data = authenticated.data
                }


{-| Use the supplied cached credentials, which may or may not have a cached user.
-}
setCachedCredentials : Config anonymousData user authenticatedData msg -> CachedCredentials user -> UserAndData anonymousData user authenticatedData -> UserAndData anonymousData user authenticatedData
setCachedCredentials config cached model =
    case maybeCredentials cached of
        Just credentials ->
            setCredentials config credentials model

        Nothing ->
            model


{-| A starting point which represents an anonymous user.

This is one possible "starting point" for initializing the UserAndData. The other
main starting points would be `checkCachedCredentials` or `loggedIn`.

Note that you should use `logout` to actually perform the action of logging
out, since that will also clear the cached credentials.

-}
loggedOut : anonymousData -> UserAndData anonymousData user authenticatedData
loggedOut data =
    Anonymous
        { progress = Nothing
        , data = data
        }


{-| A starting point which represents an authenticated user.

This is one possible "starting point" for initializing the UserAndData. The
other main starting points would be `checkCachedCredentials` or `loggedOut`.

This is meant for cases where you've received credentials through some other
mechanism.. For checking credentials, you'd use either
`checkCachedCredentials` or `tryLogin`.

-}
loggedIn : Credentials user -> authenticatedData -> UserAndData anonymousData user authenticatedData
loggedIn credentials data =
    Authenticated
        { credentials = credentials
        , logout = NotAsked
        , relogin = Nothing
        , data = data
        }


{-| Initializes a `UserAndData` by indicating that we're checking cached credentials
against the backend, and return a `Cmd` that will do that.

  - BackendUrl is the backend to check the cached credentials against.

  - The `Maybe String` parameter is the JSON string which your `cacheCredentials`
    function (from Config) has cached. So, it's up to you to fetch that value
    somehow, either via flags at startup, or via ports. If you've cached
    credentials for multiple backends, it's up to you to match your backendURL
    and your credentials.

If you supply the cached credentials, then the following sequence of events
will occur:

  - The `UserAndData` will start as `Anonymous`, with its `progress` field set to
    `(Just (Checking ByAccessToken))`. At this point, your UI should treat
    the login process as unresolved ... it will soon resolve one way or
    another. So, you might show a "checking for cached login" message, or
    just nothing.

  - If we can decode the access toekn, we'll use the access token to get
    updated user information from the backend.
      - It that succeeds, we'll switch to `Authenticated` state.

      - If it does not succeed, we'll try to decode a cached `user` (if you have
        cached user information).
          - It that succeeds, we'll be in `Authenticated` state, but we'll record
            the error checking the access token in the `relogin` field.

          - If we don't have a cached user, we'll be in `Anonymous` state, and we'll
            record the error checking the access toekn in the `progress` field.

  - If we can't decode the access token, we'll stay `Anonymous` and show `NotAsked`
    as the progess.

If you don't supply any cached credentials, we'll simply start out as `Anonymous`,
showing no progress.

-}
checkCachedCredentials : Config anonymousData user authenticatedData msg -> BackendUrl -> Maybe String -> ( UserAndData anonymousData user authenticatedData, Cmd msg )
checkCachedCredentials config backendUrl cached =
    let
        model =
            loggedOut config.initialAnonymousData

        ( userStatus, cmd, _ ) =
            case cached of
                Just json ->
                    update config (CheckCachedCredentials backendUrl json) model

                Nothing ->
                    ( model, Cmd.none, Nothing )
    in
    ( userStatus, cmd )


{-| Some static configuration which we need to integrate with your app.
You should be able to define this once, as a constant, and then use it
where needed.

The type variables have the following meanings.

  - anonymousData

    The type of data that is only for anonymous users ... that is, which
    you'd like to throw away upon login.

  - user

    The type you use for your user.

  - authenticatedData

    The type of data that is only for users ... that is, which you'd like
    to throw away upon logout.

  - msg

    Your Msg type.

The fields have the following meanings.

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

    Given the newly logged-in user, and the anonymous data which we already had
    before login, what initial data should we start with for the user?

    In many cases, you may want to ignore one or both of the parameters ... that
    is, the initial data may well be a constant. You can preserve as much or
    as little of the anonymousData as you wish upon login. Whatever you don't
    use will be thrown away.

  - initialAnonymousData

    If we have no logged in user, or if we log out, what data should we start with
    for an anonymous user.

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
type alias Config anonymousData user authenticatedData msg =
    { loginPath : String
    , logoutPath : Maybe String
    , userPath : String
    , decodeAccessToken : Decoder AccessToken
    , decodeUser : Decoder user
    , encodeUser : Maybe (user -> Value)
    , initialAuthenticatedData : anonymousData -> user -> authenticatedData
    , initialAnonymousData : anonymousData
    , cacheCredentials : BackendUrl -> String -> Cmd msg
    , tag : Msg user -> msg
    }


{-| The parts of `Config` that tend to vary from one app to the next.
-}
type alias AppConfig anonymousData user authenticatedData msg =
    { decodeUser : Decoder user
    , encodeUser : Maybe (user -> Value)
    , initialAuthenticatedData : anonymousData -> user -> authenticatedData
    , initialAnonymousData : anonymousData
    , cacheCredentials : BackendUrl -> String -> Cmd msg
    , tag : Msg user -> msg
    }


{-| Make a `Config` that uses default values oriented towards Drupal's
restful implementation.
-}
drupalConfig : AppConfig anonymousData user authenticatedData msg -> Config anonymousData user authenticatedData msg
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

    loginConfig : Config AnonymousData User AuthenticatedData Msg
    loginConfig =
        { ...
        , tag = LoginMsg
        }

    type alias Model =
        { ...
        , userAndData : UserAndData AnonymousData User AuthenticatedData
        }

    emptyModel : Model
    emptyModel =
        { ...
        , userAndData = loggedOut initialAnonymousData
        }

    type Msg
        = ...
        | LoginMsg (Restful.Login.Msg User)

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        case msg of
            ...

            LoginMsg subMsg ->
                let
                    ( subModel, cmd, event ) =
                        Restful.Login.update loginConfig subMsg model.userAndData
                in
                    -- Possibly do something additional depending on the `event`,
                    -- if you need to trigger some action on login or logout
                    ( { model | userAndData = subModel }
                    , cmd
                    )

The third return parameter will be `Just` at the very moment at which a
successful login or logout has occurred. But only at that very moment ... it's
not reflecting state, but instead a kind of notification that we've just logged
in or logged out, in case there is some further action you'd like to
trigger at that moment.

-}
update : Config anonymousData user authenticatedData msg -> Msg user -> UserAndData anonymousData user authenticatedData -> ( UserAndData anonymousData user authenticatedData, Cmd msg, Maybe LoginEvent )
update config msg model =
    case msg of
        HandleLoginAttempt retry result ->
            case result of
                Err err ->
                    ( setLoginProgress (Just (LoginError (classifyHttpError (Just retry) ByPassword err))) model
                    , Cmd.none
                    , Nothing
                    )

                Ok credentials ->
                    ( setCredentials config credentials model
                    , config.cacheCredentials credentials.backendUrl (encodeCredentials config credentials)
                    , Just LoggedIn
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
            ( setLoginProgress (Just (Checking ByPassword)) model
            , Cmd.map config.tag cmd
            , Nothing
            )

        HandleAccessTokenCheck retry credentials result ->
            case result of
                Err err ->
                    ( setCachedCredentials config credentials model
                        |> retryAccessTokenRejected (Just retry) err
                    , Cmd.none
                      -- If we have a cached user, then we're logged in even if
                      -- our access token was rejected
                    , Maybe.map (always LoggedIn) credentials.user
                    )

                Ok user ->
                    ( setCachedCredentials config { credentials | user = Just user } model
                    , Cmd.none
                    , Just LoggedIn
                    )

        CheckCachedCredentials backendUrl cachedValue ->
            case JD.decodeString (decodeCachedCredentials config backendUrl) cachedValue of
                Err _ ->
                    -- If we can't decode the cached credentials, we just
                    -- give up and say that login is needed. This will, for
                    -- instance, happen where we had logged out and cleared
                    -- the cached credentials.
                    ( setLoginProgress Nothing model
                    , Cmd.none
                    , Nothing
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
                    ( setLoginProgress (Just (Checking ByAccessToken)) model
                    , cmd
                    , Nothing
                    )

        Logout ->
            case model of
                Anonymous _ ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

                Authenticated authenticated ->
                    case config.logoutPath of
                        Just logoutPath ->
                            -- See comment below ... in this case, we can't
                            -- really logout unless we send a request to the
                            -- backend to do so.
                            ( Authenticated { authenticated | logout = Loading }
                            , HttpBuilder.get (authenticated.credentials.backendUrl </> logoutPath)
                                |> withQueryParams [ ( "access_token", authenticated.credentials.accessToken ) ]
                                |> HttpBuilder.toTask
                                |> Task.attempt HandleLogoutAttempt
                                |> Cmd.map config.tag
                            , Nothing
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
                ( Ok _, Authenticated login ) ->
                    -- We tell the app to cache credentials consisting of an empty object.
                    -- This is simpler than telling the app to delete credentials.
                    ( loggedOut config.initialAnonymousData
                    , config.cacheCredentials login.credentials.backendUrl "{}"
                    , Nothing
                    )

                ( Err err, Authenticated login ) ->
                    -- Just record the error
                    ( Authenticated { login | logout = Failure err }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    -- If we weren't logged in anyway, there's nothing to do.
                    ( model, Cmd.none, Nothing )


encodeCredentials : Config anonymousData user authenticatedData msg -> Credentials user -> String
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


decodeCachedCredentials : Config anonymousData user authenticated msg -> BackendUrl -> Decoder (CachedCredentials user)
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
hasValidAccessToken : UserAndData anonymousData user authenticatedData -> Bool
hasValidAccessToken status =
    case status of
        Anonymous _ ->
            False

        Authenticated login ->
            login.relogin == Nothing


{-| Do we have an access token, whether or not we think it's valid?

If we're still checking, we say `False`.

-}
hasAccessToken : UserAndData anonymousData user authenticatedData -> Bool
hasAccessToken status =
    case status of
        Anonymous _ ->
            False

        Authenticated login ->
            True


{-| Record the fact that our access token was rejected.

If we're in a `Authenticated` state, we'll stay in that state ... we'll
merely record that re-login is required.

-}
accessTokenRejected : Error -> UserAndData anonymousData user authenticatedData -> UserAndData anonymousData user authenticatedData
accessTokenRejected =
    retryAccessTokenRejected Nothing


{-| Internal version of accessTokenRejected that will keep track of a msg
we can use to retry.
-}
retryAccessTokenRejected : Maybe (Msg user) -> Error -> UserAndData anonymousData user authenticatedData -> UserAndData anonymousData user authenticatedData
retryAccessTokenRejected retry error =
    setLoginProgress (Just <| LoginError <| classifyHttpError retry ByAccessToken error)


{-| If you previously recorded `accessTokenRejected` but it was a transient
problem, and now it has been accepted, you can record that with this function.

You don't need to call this every time the access token is accepted (though
it won't do any harm, either).

Note that this doesn't switch our state from `Anonymous` to `Authenticated` ...
it only resets `Authenticated` (if that's what we are) to show that `relogin`
is not required.

-}
accessTokenAccepted : UserAndData anonymousData user authenticatedData -> UserAndData anonymousData user authenticatedData
accessTokenAccepted status =
    -- We return `status` unchanged as often as possible, for the sake of
    -- preserving referential equality where we can.
    case status of
        Anonymous _ ->
            status

        Authenticated login ->
            case login.relogin of
                Just _ ->
                    Authenticated { login | relogin = Nothing }

                Nothing ->
                    status
