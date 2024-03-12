module Codegen.SimplifyTest exposing (all)

import Codegen.Simplify exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Codegen.Simplify"
        [ test "should not report an error when Elm.value is used with a nonconstant module name" <|
            \() ->
                """module A exposing (..)
a = Elm.value { }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "REPLACEME"
                            }
                        ]
        ]
