module String.Nonempty exposing
    ( Nonempty(..), length, reverse
    , append, append_, concat
    , slice, left, right, dropLeft, dropRight
    , toInt, fromInt
    , toFloat, fromFloat
    , fromChar, cons, uncons
    , toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight
    , map, filter, foldl, foldr, any, all
    , fromNonemptyList, fromString, head, tail, toNonemptyList, toString
    )

{-| A string that cannot be empty. The head and tail can be accessed without Maybes. Most other string functions are
available.


# Strings

@docs Nonempty, length, reverse


# Building and Splitting

@docs append, append_, concat


# Get Substrings

@docs slice, left, right, dropLeft, dropRight


# Check for Substrings

@docs contains, startsWith, endsWith, indexes, indices


# Int Conversions

@docs toInt, fromInt


# Float Conversions

@docs toFloat, fromFloat


# Char Conversions

@docs fromChar, cons, uncons


# List Conversions

@docs toList, fromList


# Formatting

Cosmetic operations such as padding with extra characters or trimming whitespace.

@docs toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight


# Higher-Order Functions

@docs map, filter, foldl, foldr, any, all

-}

import List.Nonempty


{-| A string with at least one character.
-}
type Nonempty
    = Nonempty Char String


{-| Get the length of a nonempty string.
-}
length : Nonempty -> Int
length =
    toString >> String.length


{-| Reverse a string.
-}
reverse : Nonempty -> Nonempty
reverse text =
    toString text |> String.reverse |> unsafeFromString


{-| Append a string onto the beginning of a nonempty string.
-}
append : String -> Nonempty -> Nonempty
append first second =
    case fromString first of
        Just firstNonempty ->
            append_ firstNonempty (toString second)

        Nothing ->
            second


{-| Append a string onto the end of a nonempty string.
-}
append_ : Nonempty -> String -> Nonempty
append_ (Nonempty head_ tail_) second =
    Nonempty head_ (tail_ ++ second)


{-| Concatenate many nonempty strings into one.
-}
concat : List.Nonempty.Nonempty Nonempty -> Nonempty
concat texts =
    List.Nonempty.foldl1 (\a b -> append_ b (toString a)) texts


{-| Take a substring given a start and end index. Negative indexes
are taken starting from the _end_ of the list.

    text = Nonempty 's' "nakes on a plane!"

    slice  7  9 text == "on"
    slice  0  6 text == "snakes"
    slice  0 -7 text == "snakes on a"
    slice -6 -1 text == "plane"

-}
slice : Int -> Int -> Nonempty -> String
slice a b =
    toString >> String.slice a b


{-| Take _n_ characters from the left side of a nonempty string.
-}
left : Int -> Nonempty -> String
left n =
    toString >> String.left n


{-| Take _n_ characters from the right side of a nonempty string.
-}
right : Int -> Nonempty -> String
right n =
    toString >> String.right n


{-| Drop _n_ characters from the left side of a nonempty string.
-}
dropLeft : Int -> Nonempty -> String
dropLeft n =
    toString >> String.dropLeft n


{-| Drop _n_ characters from the right side of a nonempty string.
-}
dropRight : Int -> Nonempty -> String
dropRight n =
    toString >> String.dropRight n


{-| Try to convert a string into an int, failing on improperly formatted nonempty strings.
-}
toInt : Nonempty -> Maybe Int
toInt =
    toString >> String.toInt


{-| Convert an `Int` to a `Nonempty`.
-}
fromInt : Int -> Nonempty
fromInt =
    String.fromInt >> unsafeFromString


{-| Try to convert a string into a float, failing on improperly formatted nonempty strings.
-}
toFloat : Nonempty -> Maybe Float
toFloat =
    toString >> String.toFloat


{-| Convert a `Float` to a `Nonempty`.
-}
fromFloat : Float -> Nonempty
fromFloat =
    String.fromFloat >> unsafeFromString


{-| Create a string from a given character.

    fromChar 'a' == Nonempty 'a' ""

-}
fromChar : Char -> Nonempty
fromChar char =
    Nonempty char ""


{-| Add a character to the beginning of a nonempty string.
-}
cons : Char -> Nonempty -> Nonempty
cons char (Nonempty head_ tail_) =
    Nonempty char (String.cons head_ tail_)


{-| Split a nonempty string into its head and tail. This lets you pattern match on strings exactly as you would with lists.
-}
uncons : Nonempty -> ( Char, String )
uncons (Nonempty head_ tail_) =
    ( head_, tail_ )


{-| Convert a nonempty string to a nonempty list of characters.
-}
toNonemptyList : Nonempty -> List.Nonempty.Nonempty Char
toNonemptyList (Nonempty head_ tail_) =
    List.Nonempty.Nonempty head_ (String.toList tail_)


{-| Convert a nonempty list of characters into a nonempty string.
-}
fromNonemptyList : List.Nonempty.Nonempty Char -> Nonempty
fromNonemptyList (List.Nonempty.Nonempty head_ tail_) =
    Nonempty head_ (String.fromList tail_)


{-| Convert a string to all upper case. Useful for case-insensitive comparisons and VIRTUAL YELLING.
-}
toUpper : Nonempty -> Nonempty
toUpper (Nonempty head_ tail_) =
    Nonempty (Char.toUpper head_) (String.toUpper tail_)


{-| Convert a string to all lower case. Useful for case-insensitive comparisons.
-}
toLower : Nonempty -> Nonempty
toLower (Nonempty head_ tail_) =
    Nonempty (Char.toLower head_) (String.toLower tail_)


{-| Pad a nonempty string on both sides until it has a given length.
-}
pad : Int -> Char -> Nonempty -> Nonempty
pad n char =
    toString >> String.pad n char >> unsafeFromString


{-| Pad a nonempty string on the left until it has a given length.
-}
padLeft : Int -> Char -> Nonempty -> Nonempty
padLeft n char =
    toString >> String.padLeft n char >> unsafeFromString


{-| Pad a nonempty string on the right until it has a given length.
-}
padRight : Int -> Char -> Nonempty -> Nonempty
padRight n char =
    toString >> String.padRight n char >> unsafeFromString


{-| Get rid of whitespace on both sides of a nonempty string.
-}
trim : Nonempty -> String
trim =
    toString >> String.trim


{-| Get rid of whitespace on the left of a nonempty string.
-}
trimLeft : Nonempty -> String
trimLeft =
    toString >> String.trimLeft


{-| Get rid of whitespace on the right of a nonempty string.
-}
trimRight : Nonempty -> String
trimRight =
    toString >> String.trimRight


{-| Transform every character in a nonempty string
-}
map : (Char -> Char) -> Nonempty -> Nonempty
map charFunc (Nonempty head_ tail_) =
    Nonempty (charFunc head_) (String.map charFunc tail_)


{-| Keep only the characters that pass the test.
-}
filter : (Char -> Bool) -> Nonempty -> String
filter filterFunc =
    toString >> String.filter filterFunc


{-| Reduce a nonempty string from the left.
-}
foldl : (Char -> b -> b) -> b -> Nonempty -> b
foldl foldFunc value =
    toString >> String.foldl foldFunc value


{-| Reduce a nonempty string from the right.
-}
foldr : (Char -> b -> b) -> b -> Nonempty -> b
foldr foldFunc value =
    toString >> String.foldr foldFunc value


{-| Determine whether _any_ characters pass the test.
-}
any : (Char -> Bool) -> Nonempty -> Bool
any anyFunc =
    toString >> String.any anyFunc


{-| Determine whether _all_ characters pass the test.
-}
all : (Char -> Bool) -> Nonempty -> Bool
all allFunc =
    toString >> String.all allFunc


{-| Create a nonempty string from an ordinary string, failing on the empty string.
-}
fromString : String -> Maybe Nonempty
fromString text =
    case String.uncons text of
        Just ( head_, tail_ ) ->
            Nonempty head_ tail_ |> Just

        Nothing ->
            Nothing


{-| Convert to an ordinary string.
-}
toString : Nonempty -> String
toString (Nonempty head_ tail_) =
    String.cons head_ tail_


{-| Get the first character in the nonempty string.
-}
head : Nonempty -> Char
head (Nonempty head_ _) =
    head_


{-| Return all the character after the first one.
-}
tail : Nonempty -> String
tail (Nonempty _ tail_) =
    tail_


{-| To be used internally when we are certain that the string is already nonempty.
-}
unsafeFromString : String -> Nonempty
unsafeFromString =
    fromString >> Maybe.withDefault (Nonempty ' ' "")
