module String.Nonempty exposing
    ( NonemptyString(..), length, reverse
    , append, append_, concat, fromString, toString
    , slice, left, right, dropLeft, dropRight, head, tail
    , contains, startsWith, endsWith, indexes, indices
    , toInt, fromInt
    , toFloat, fromFloat
    , fromChar, cons, uncons
    , toNonemptyList, fromNonemptyList
    , toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight
    , map, filter, foldl, foldr, any, all
    )

{-| A string that cannot be empty. The head and tail can be accessed without Maybes. Most other string functions are
available.


# Strings

@docs NonemptyString, length, reverse


# Building

@docs append, append_, concat, fromString, toString


# Get Substrings

@docs slice, left, right, dropLeft, dropRight, head, tail


# Check for Substrings

@docs contains, startsWith, endsWith, indexes, indices


# Int Conversions

@docs toInt, fromInt


# Float Conversions

@docs toFloat, fromFloat


# Char Conversions

@docs fromChar, cons, uncons


# List Conversions

@docs toNonemptyList, fromNonemptyList


# Formatting

Cosmetic operations such as padding with extra characters or trimming whitespace.

@docs toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight


# Higher-Order Functions

@docs map, filter, foldl, foldr, any, all

-}

import List.Nonempty


{-| A string with at least one character.
-}
type NonemptyString
    = NonemptyString Char String


{-| Get the length of a nonempty string.
-}
length : NonemptyString -> Int
length =
    toString >> String.length


{-| Reverse a string.
-}
reverse : NonemptyString -> NonemptyString
reverse text =
    toString text |> String.reverse |> unsafeFromString


{-| Append a string onto the beginning of a nonempty string.
-}
append : String -> NonemptyString -> NonemptyString
append first second =
    case fromString first of
        Just firstNonempty ->
            append_ firstNonempty (toString second)

        Nothing ->
            second


{-| Append a string onto the end of a nonempty string.
-}
append_ : NonemptyString -> String -> NonemptyString
append_ (NonemptyString head_ tail_) second =
    NonemptyString head_ (tail_ ++ second)


{-| Concatenate many nonempty strings into one.
-}
concat : List.Nonempty.Nonempty NonemptyString -> NonemptyString
concat texts =
    List.Nonempty.foldl1 (\a b -> append_ b (toString a)) texts


{-| Take a substring given a start and end index. Negative indexes
are taken starting from the _end_ of the list.

    text = NonemptyString 's' "nakes on a plane!"

    slice  7  9 text == "on"
    slice  0  6 text == "snakes"
    slice  0 -7 text == "snakes on a"
    slice -6 -1 text == "plane"

-}
slice : Int -> Int -> NonemptyString -> String
slice a b =
    toString >> String.slice a b


{-| Take _n_ characters from the left side of a nonempty string.
-}
left : Int -> NonemptyString -> String
left n =
    toString >> String.left n


{-| Take _n_ characters from the right side of a nonempty string.
-}
right : Int -> NonemptyString -> String
right n =
    toString >> String.right n


{-| Drop _n_ characters from the left side of a nonempty string.
-}
dropLeft : Int -> NonemptyString -> String
dropLeft n =
    toString >> String.dropLeft n


{-| Drop _n_ characters from the right side of a nonempty string.
-}
dropRight : Int -> NonemptyString -> String
dropRight n =
    toString >> String.dropRight n


{-| Try to convert a string into an int, failing on improperly formatted nonempty strings.
-}
toInt : NonemptyString -> Maybe Int
toInt =
    toString >> String.toInt


{-| Convert an `Int` to a `NonemptyString`.
-}
fromInt : Int -> NonemptyString
fromInt =
    String.fromInt >> unsafeFromString


{-| Try to convert a string into a float, failing on improperly formatted nonempty strings.
-}
toFloat : NonemptyString -> Maybe Float
toFloat =
    toString >> String.toFloat


{-| Convert a `Float` to a `NonemptyString`.
-}
fromFloat : Float -> NonemptyString
fromFloat =
    String.fromFloat >> unsafeFromString


{-| Create a string from a given character.

    fromChar 'a' == NonemptyString 'a' ""

-}
fromChar : Char -> NonemptyString
fromChar char =
    NonemptyString char ""


{-| Add a character to the beginning of a nonempty string.
-}
cons : Char -> NonemptyString -> NonemptyString
cons char (NonemptyString head_ tail_) =
    NonemptyString char (String.cons head_ tail_)


{-| Split a nonempty string into its head and tail. This lets you pattern match on strings exactly as you would with lists.
-}
uncons : NonemptyString -> ( Char, String )
uncons (NonemptyString head_ tail_) =
    ( head_, tail_ )


{-| Convert a nonempty string to a nonempty list of characters.
-}
toNonemptyList : NonemptyString -> List.Nonempty.Nonempty Char
toNonemptyList (NonemptyString head_ tail_) =
    List.Nonempty.Nonempty head_ (String.toList tail_)


{-| Convert a nonempty list of characters into a nonempty string.
-}
fromNonemptyList : List.Nonempty.Nonempty Char -> NonemptyString
fromNonemptyList (List.Nonempty.Nonempty head_ tail_) =
    NonemptyString head_ (String.fromList tail_)


{-| Convert a string to all upper case. Useful for case-insensitive comparisons and VIRTUAL YELLING.
-}
toUpper : NonemptyString -> NonemptyString
toUpper (NonemptyString head_ tail_) =
    NonemptyString (Char.toUpper head_) (String.toUpper tail_)


{-| Convert a string to all lower case. Useful for case-insensitive comparisons.
-}
toLower : NonemptyString -> NonemptyString
toLower (NonemptyString head_ tail_) =
    NonemptyString (Char.toLower head_) (String.toLower tail_)


{-| Pad a nonempty string on both sides until it has a given length.
-}
pad : Int -> Char -> NonemptyString -> NonemptyString
pad n char =
    toString >> String.pad n char >> unsafeFromString


{-| Pad a nonempty string on the left until it has a given length.
-}
padLeft : Int -> Char -> NonemptyString -> NonemptyString
padLeft n char =
    toString >> String.padLeft n char >> unsafeFromString


{-| Pad a nonempty string on the right until it has a given length.
-}
padRight : Int -> Char -> NonemptyString -> NonemptyString
padRight n char =
    toString >> String.padRight n char >> unsafeFromString


{-| Get rid of whitespace on both sides of a nonempty string.
-}
trim : NonemptyString -> String
trim =
    toString >> String.trim


{-| Get rid of whitespace on the left of a nonempty string.
-}
trimLeft : NonemptyString -> String
trimLeft =
    toString >> String.trimLeft


{-| Get rid of whitespace on the right of a nonempty string.
-}
trimRight : NonemptyString -> String
trimRight =
    toString >> String.trimRight


{-| Transform every character in a nonempty string
-}
map : (Char -> Char) -> NonemptyString -> NonemptyString
map charFunc (NonemptyString head_ tail_) =
    NonemptyString (charFunc head_) (String.map charFunc tail_)


{-| Keep only the characters that pass the test.
-}
filter : (Char -> Bool) -> NonemptyString -> String
filter filterFunc =
    toString >> String.filter filterFunc


{-| Reduce a nonempty string from the left.
-}
foldl : (Char -> b -> b) -> b -> NonemptyString -> b
foldl foldFunc value =
    toString >> String.foldl foldFunc value


{-| Reduce a nonempty string from the right.
-}
foldr : (Char -> b -> b) -> b -> NonemptyString -> b
foldr foldFunc value =
    toString >> String.foldr foldFunc value


{-| Determine whether _any_ characters pass the test.
-}
any : (Char -> Bool) -> NonemptyString -> Bool
any anyFunc =
    toString >> String.any anyFunc


{-| Determine whether _all_ characters pass the test.
-}
all : (Char -> Bool) -> NonemptyString -> Bool
all allFunc =
    toString >> String.all allFunc


{-| Create a nonempty string from an ordinary string, failing on the empty string.
-}
fromString : String -> Maybe NonemptyString
fromString text =
    case String.uncons text of
        Just ( head_, tail_ ) ->
            NonemptyString head_ tail_ |> Just

        Nothing ->
            Nothing


{-| Convert to an ordinary string.
-}
toString : NonemptyString -> String
toString (NonemptyString head_ tail_) =
    String.cons head_ tail_


{-| Get the first character in the nonempty string.
-}
head : NonemptyString -> Char
head (NonemptyString head_ _) =
    head_


{-| Return all the characters after the first one.
-}
tail : NonemptyString -> String
tail (NonemptyString _ tail_) =
    tail_


{-| See if the second string contains the first one.
-}
contains : String -> NonemptyString -> Bool
contains substring =
    toString >> String.contains substring


{-| See if the second string starts with the first one.
-}
startsWith : String -> NonemptyString -> Bool
startsWith prefix =
    toString >> String.startsWith prefix


{-| See if the second string ends with the first one.
-}
endsWith : String -> NonemptyString -> Bool
endsWith prefix =
    toString >> String.endsWith prefix


{-| Get all of the indexes for a substring in another string.

    indexes "i" (NonemptyString 'M' "ississippi") == [ 1, 4, 7, 10 ]

    indexes "ss" (NonemptyString 'M' "ississippi") == [ 2, 5 ]

-}
indexes : String -> NonemptyString -> List Int
indexes text =
    toString >> String.indexes text


{-| Alias for `indexes`.
-}
indices : String -> NonemptyString -> List Int
indices text =
    toString >> String.indices text


{-| To be used internally when we are certain that the string is already nonempty.
-}
unsafeFromString : String -> NonemptyString
unsafeFromString =
    fromString >> Maybe.withDefault (NonemptyString ' ' "")
