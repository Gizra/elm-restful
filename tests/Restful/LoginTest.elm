module Restful.LoginTest exposing (..)

import Expect
import Json.Decode as JD exposing (Decoder, field)
import Json.Encode as JE exposing (Value)
import Restful.Login exposing (..)
import Test exposing (Test, describe, test)


type alias User =
    { userId : String
    , name : String
    }


type alias AnonymousData =
    ()


type alias AuthenticatedData =
    ()


decodeUser : Decoder User
decodeUser =
    JD.map2 User
        (field "userId" JD.string)
        (field "name" JD.string)


encodeUser : User -> Value
encodeUser user =
    JE.object
        [ ( "userId", JE.string user.userId )
        , ( "name", JE.string user.name )
        ]


exampleUser : User
exampleUser =
    { userId = "example"
    , name = "Example User"
    }


exampleCredentials : Credentials User
exampleCredentials =
    { accessToken = "adkfja;lskdj28d7skd"
    , backendUrl = "https:/example.com/"
    , user = exampleUser
    }


type AppMsg
    = MsgLogin (Msg User)


config : Config AnonymousData User AuthenticatedData AppMsg
config =
    drupalConfig
        { decodeUser = decodeUser
        , encodeUser = Just encodeUser
        , initialAuthenticatedData = \_ _ -> ()
        , initialAnonymousData = ()
        , cacheCredentials = \_ _ -> Cmd.none
        , tag = MsgLogin
        }


exampleAuthenticated : UserAndData AnonymousData User AuthenticatedData
exampleAuthenticated =
    loggedIn exampleCredentials (config.initialAuthenticatedData config.initialAnonymousData exampleUser)


exampleAnonymous : UserAndData AnonymousData User AuthenticatedData
exampleAnonymous =
    loggedOut config.initialAnonymousData


authenticatedCheckingAccessToken : UserAndData AnonymousData User AuthenticatedData
authenticatedCheckingAccessToken =
    exampleAuthenticated
        |> update config (tryAccessToken "http://gizra.com" "aldkfjbsldfj")
        |> (\( model, _, _ ) -> model)


anonymousCheckingAccessToken : UserAndData AnonymousData User AuthenticatedData
anonymousCheckingAccessToken =
    exampleAnonymous
        |> update config (tryAccessToken "http://gizra.com" "aldkfjbsldfj")
        |> (\( model, _, _ ) -> model)


authenticatedCheckingPassword : UserAndData AnonymousData User AuthenticatedData
authenticatedCheckingPassword =
    exampleAuthenticated
        |> update config (tryLogin "http://gizra.com" [] "fred" "password")
        |> (\( model, _, _ ) -> model)


anonymousCheckingPassword : UserAndData AnonymousData User AuthenticatedData
anonymousCheckingPassword =
    exampleAnonymous
        |> update config (tryLogin "http://gizra.com" [] "fred" "password")
        |> (\( model, _, _ ) -> model)


isAnonymousUserTest : Test
isAnonymousUserTest =
    describe "isAnonymousUser"
        [ test "true" <|
            \_ ->
                isAnonymousUser exampleAnonymous
                    |> Expect.equal True
        , test "false" <|
            \_ ->
                isAnonymousUser exampleAuthenticated
                    |> Expect.equal False
        ]


isAuthenticatedUserTest : Test
isAuthenticatedUserTest =
    describe "isAuthenticatedUser"
        [ test "true" <|
            \_ ->
                isAuthenticatedUser exampleAuthenticated
                    |> Expect.equal True
        , test "false" <|
            \_ ->
                isAuthenticatedUser exampleAnonymous
                    |> Expect.equal False
        ]


getUserTest : Test
getUserTest =
    describe "getUser"
        [ test "authenticated" <|
            \_ ->
                getUser exampleAuthenticated
                    |> Expect.equal (Just exampleUser)
        , test "anonymous" <|
            \_ ->
                getUser exampleAnonymous
                    |> Expect.equal Nothing
        ]


getLoginProgressTest : Test
getLoginProgressTest =
    describe "getLoginProgress"
        [ test "authenticated none" <|
            \_ ->
                exampleAuthenticated
                    |> getLoginProgress
                    |> Expect.equal Nothing
        , test "authenticated access token" <|
            \_ ->
                authenticatedCheckingAccessToken
                    |> getLoginProgress
                    |> Expect.equal (Just (Checking ByAccessToken))
        , test "authenticated password" <|
            \_ ->
                authenticatedCheckingPassword
                    |> getLoginProgress
                    |> Expect.equal (Just (Checking ByPassword))
        , test "anonymous none" <|
            \_ ->
                exampleAnonymous
                    |> getLoginProgress
                    |> Expect.equal Nothing
        , test "anonymous access token" <|
            \_ ->
                anonymousCheckingAccessToken
                    |> getLoginProgress
                    |> Expect.equal (Just (Checking ByAccessToken))
        , test "anonymous password" <|
            \_ ->
                anonymousCheckingPassword
                    |> getLoginProgress
                    |> Expect.equal (Just (Checking ByPassword))
        ]


isCheckingTest : Test
isCheckingTest =
    describe "isChecking"
        [ test "authenticated false" <|
            \_ ->
                exampleAuthenticated
                    |> isChecking
                    |> Expect.equal False
        , test "authenticated access token" <|
            \_ ->
                authenticatedCheckingAccessToken
                    |> isChecking
                    |> Expect.equal True
        , test "authenticated password" <|
            \_ ->
                authenticatedCheckingPassword
                    |> isChecking
                    |> Expect.equal True
        , test "anonymous false" <|
            \_ ->
                exampleAnonymous
                    |> isChecking
                    |> Expect.equal False
        , test "anonymous checking access token" <|
            \_ ->
                anonymousCheckingAccessToken
                    |> isChecking
                    |> Expect.equal True
        , test "anonymous checking password" <|
            \_ ->
                anonymousCheckingPassword
                    |> isChecking
                    |> Expect.equal True
        ]


isCheckingAccessTokenTest : Test
isCheckingAccessTokenTest =
    describe "isCheckingAccessToken"
        [ test "authenticated false" <|
            \_ ->
                exampleAuthenticated
                    |> isCheckingAccessToken
                    |> Expect.equal False
        , test "authenticated access token" <|
            \_ ->
                authenticatedCheckingAccessToken
                    |> isCheckingAccessToken
                    |> Expect.equal True
        , test "authenticated password" <|
            \_ ->
                authenticatedCheckingPassword
                    |> isCheckingAccessToken
                    |> Expect.equal False
        , test "anonymous false" <|
            \_ ->
                exampleAnonymous
                    |> isCheckingAccessToken
                    |> Expect.equal False
        , test "anonymous access token" <|
            \_ ->
                anonymousCheckingAccessToken
                    |> isCheckingAccessToken
                    |> Expect.equal True
        , test "anonymous password" <|
            \_ ->
                anonymousCheckingPassword
                    |> isCheckingAccessToken
                    |> Expect.equal False
        ]


isCheckingPasswordTest : Test
isCheckingPasswordTest =
    describe "isCheckingPassword"
        [ test "authenticated false" <|
            \_ ->
                exampleAuthenticated
                    |> isCheckingPassword
                    |> Expect.equal False
        , test "authenticated access token" <|
            \_ ->
                authenticatedCheckingAccessToken
                    |> isCheckingPassword
                    |> Expect.equal False
        , test "authenticated password" <|
            \_ ->
                authenticatedCheckingPassword
                    |> isCheckingPassword
                    |> Expect.equal True
        , test "anonymous false" <|
            \_ ->
                exampleAnonymous
                    |> isCheckingPassword
                    |> Expect.equal False
        , test "anonymous access token" <|
            \_ ->
                anonymousCheckingAccessToken
                    |> isCheckingPassword
                    |> Expect.equal False
        , test "anonymous password" <|
            \_ ->
                anonymousCheckingPassword
                    |> isCheckingPassword
                    |> Expect.equal True
        ]
