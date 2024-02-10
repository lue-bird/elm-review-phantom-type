module Tests exposing (tests)

import Review.PhantomType
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test)


tests : Test
tests =
    Test.describe "elm-review-phantom-type"
        [ Test.test "does not report when type arguments are used directly"
            (\() ->
                """module A exposing (..)

type alias Two a b =
    ( a, b )
"""
                    |> Review.Test.run Review.PhantomType.forbid
                    |> Review.Test.expectNoErrors
            )
        , Test.test "does not report when type arguments are used indirectly with module-local type"
            (\() ->
                """module A exposing (..)

type alias Three a b c =
    ( a, Two b c )

type alias Two a b =
    ( a, b )
"""
                    |> Review.Test.run Review.PhantomType.forbid
                    |> Review.Test.expectNoErrors
            )
        , Test.test "does not report when type arguments are used indirectly with project-local type"
            (\() ->
                [ """module A exposing (..)

import Two exposing (Two)

type alias Three a b c =
    ( a, Two b c )
"""
                , """module Two exposing (Two)

type alias Two a b =
    ( a, b )
"""
                ]
                    |> Review.Test.runOnModules Review.PhantomType.forbid
                    |> Review.Test.expectNoErrors
            )
        , Test.test "does not report when type arguments are used indirectly in dependency"
            (\() ->
                """module A exposing (..)

import Set exposing (Set)

type alias SetWithHead head tailElement =
    ( head, Set tailElement )
"""
                    |> Review.Test.run
                        Review.PhantomType.forbid
                    |> Review.Test.expectNoErrors
            )
        , Test.test "reports direct phantom type"
            (\() ->
                """module A exposing (..)

type Two a b
    = Two a
"""
                    |> Review.Test.run Review.PhantomType.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "variable not used in the definition"
                            , details =
                                [ """This is often called "phantom type" because the type argument has no effect on values that can be constructed using its variants.
These types are usually used to provide more information on specific constructed values in order to restrict their use."""
                                , """These types can be somewhat complex and tricky to use well and they don't give you the usual rewards of type-safety either."""
                                , """If you were not aware of this feature when writing this type, it's safe to remove the parameter or use it somewhere.
If you intentionally used this phantom type, I suggest looking at [these alternatives](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-phantom-type/latest#but-what-are-the-alternatives)."""
                                ]
                            , under = "b"
                            }
                        ]
            )
        , Test.test "reports self-recursive phantom type"
            (\() ->
                """module A exposing (..)

type IntList a
    = Empty
    | Filled { head : Int, tail : IntList a }
"""
                    |> Review.Test.run Review.PhantomType.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "variable not used in the definition"
                            , details =
                                [ """This is often called "phantom type" because the type argument has no effect on values that can be constructed using its variants.
These types are usually used to provide more information on specific constructed values in order to restrict their use."""
                                , """These types can be somewhat complex and tricky to use well and they don't give you the usual rewards of type-safety either."""
                                , """If you were not aware of this feature when writing this type, it's safe to remove the parameter or use it somewhere.
If you intentionally used this phantom type, I suggest looking at [these alternatives](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-phantom-type/latest#but-what-are-the-alternatives)."""
                                ]
                            , under = "a"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } }
                        ]
            )
        , Test.test "reports mutually recursive phantom type with one being a type alias"
            (\() ->
                """module A exposing (..)

type IntList a
    = Empty
    | Filled (IntListFilled a)

type alias IntListFilled a =
    { head : Int, tail : IntList a }
"""
                    |> Review.Test.run Review.PhantomType.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "variable not used in the definition"
                            , details =
                                [ """This is often called "phantom type" because the type argument has no effect on values that can be constructed using its variants.
These types are usually used to provide more information on specific constructed values in order to restrict their use."""
                                , """These types can be somewhat complex and tricky to use well and they don't give you the usual rewards of type-safety either."""
                                , """If you were not aware of this feature when writing this type, it's safe to remove the parameter or use it somewhere.
If you intentionally used this phantom type, I suggest looking at [these alternatives](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-phantom-type/latest#but-what-are-the-alternatives)."""
                                ]
                            , under = "a"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } }
                        ]
            )
        ]
