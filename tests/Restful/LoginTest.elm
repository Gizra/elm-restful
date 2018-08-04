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
    , backendUrl = "http:/gizra.com/"
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


exampleLoggedIn : UserAndData AnonymousData User AuthenticatedData
exampleLoggedIn =
    loggedIn exampleCredentials (config.initialAuthenticatedData config.initialAnonymousData exampleUser)


exampleLoggedOut : UserAndData AnonymousData User AuthenticatedData
exampleLoggedOut =
    loggedOut config.initialAnonymousData


isAnonymousUserTest : Test
isAnonymousUserTest =
    describe "isAnonymousUser"
        [ test "true" <|
            \_ ->
                isAnonymousUser exampleLoggedOut
                    |> Expect.equal True
        , test "false" <|
            \_ ->
                isAnonymousUser exampleLoggedIn
                    |> Expect.equal False
        ]


isAuthenticatedUserTest : Test
isAuthenticatedUserTest =
    describe "isAuthenticatedUser"
        [ test "true" <|
            \_ ->
                isAuthenticatedUser exampleLoggedIn
                    |> Expect.equal True
        , test "false" <|
            \_ ->
                isAuthenticatedUser exampleLoggedOut
                    |> Expect.equal False
        ]
