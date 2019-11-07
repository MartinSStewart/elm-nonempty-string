module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Nonempty
import String.Nonempty as Nonempty
import Test exposing (..)


suite : Test
suite =
    describe "String.Nonempty tests"
        [ test "concat" <|
            \_ ->
                let
                    expected =
                        String.concat [ Nonempty.toString a, Nonempty.toString b, Nonempty.toString c ]

                    a =
                        Nonempty.Nonempty 'E' "xpected"

                    b =
                        Nonempty.Nonempty ' ' "test"

                    c =
                        Nonempty.Nonempty ' ' "result"
                in
                List.Nonempty.Nonempty a [ b, c ]
                    |> Nonempty.concat
                    |> Nonempty.toString
                    |> Expect.equal expected
        , fuzz2 Fuzz.string fuzzNonemptyString "append" <|
            \a b ->
                let
                    expected =
                        a ++ Nonempty.toString b
                in
                Nonempty.append a b |> Nonempty.toString |> Expect.equal expected
        , fuzz2 fuzzNonemptyString Fuzz.string "append_" <|
            \a b ->
                let
                    expected =
                        Nonempty.toString a ++ b
                in
                Nonempty.append_ a b |> Nonempty.toString |> Expect.equal expected
        , fuzz Fuzz.int "fromInt" <|
            \int ->
                Nonempty.fromInt int |> Nonempty.toInt |> Expect.equal (Just int)
        , fuzz Fuzz.float "fromFloat" <|
            \float ->
                case Nonempty.fromFloat float |> Nonempty.toFloat of
                    Just parsedFloat ->
                        parsedFloat |> Expect.within (Expect.AbsoluteOrRelative 0.00001 0.00001) float

                    Nothing ->
                        Expect.fail "Failed to parse float"
        ]


fuzzNonemptyList fuzzer =
    Fuzz.map2 List.Nonempty.Nonempty fuzzer (Fuzz.list fuzzer)


fuzzNonemptyString =
    Fuzz.map2 Nonempty.Nonempty Fuzz.char Fuzz.string
