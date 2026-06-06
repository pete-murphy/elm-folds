module Fold exposing
    ( Fold
    , unfoldFold, unfoldFold_, succeed
    , foldList, scanList, stepFold
    , head, last, null, length
    , and, or, all, any
    , sum, product
    , maximum, minimum
    , elem, notElem
    , map, map2, map3, map4, map5, map6, andMap
    , extend
    , groupBy, prefilter
    )

{-| This module provides a type [`Fold`](#Fold) for left folds, which can be
combined using [`map2`](#map2) (or, more generally, [`succeed`](#succeed) and
[`andMap`](#andMap) in pipeline style):

    average : Fold Float Float
    average =
        map2 (/) sum length

A `Fold` can be used to fold a list ([`foldList`](#foldList)) or to scan a list
([`scanList`](#scanList)):

    finalAverage : Float
    finalAverage =
        foldList [ 1, 2, 3 ] average

    movingAverage : List Float
    movingAverage =
        scanList [ 1, 2, 3 ] average

This library is based on the [`foldl`][foldl] library by Gabriella Gonzalez.

[foldl]: http://hackage.haskell.org/package/foldl


# Type

@docs Fold


# Constructors

@docs unfoldFold, unfoldFold_, succeed


# Destructors

@docs foldList, scanList, stepFold


# Common folds

@docs head, last, null, length
@docs and, or, all, any
@docs sum, product
@docs maximum, minimum
@docs elem, notElem


# Combinators

@docs map, map2, map3, map4, map5, map6, andMap
@docs extend
@docs groupBy, prefilter

-}

import Dict exposing (Dict)
import List.Extra



-- TYPE


{-| A left fold, which takes zero or more values of type `a` as input and
produces output of type `b`.
-}
type Fold a b
    = Fold
        { step : a -> Fold a b
        , finish : () -> b
        }



-- CONSTRUCTORS


{-| Create a `Fold` by providing an initial state, a function which updates
that state, and a function which produces output from a state.
-}
unfoldFold : s -> (a -> s -> s) -> (s -> b) -> Fold a b
unfoldFold s0 step finish =
    let
        go s =
            Fold { step = \a -> go (step a s), finish = \_ -> finish s }
    in
    go s0


{-| Create a `Fold` by providing an initial state and a function which updates
that state. This is a variant of [`unfoldFold`](#unfoldFold) where the output
is the state itself.
-}
unfoldFold_ : b -> (a -> b -> b) -> Fold a b
unfoldFold_ s0 step =
    unfoldFold s0 step identity


{-| A `Fold` which ignores its input and always produces the given value.

This is useful as the starting point of an [`andMap`](#andMap) pipeline.

-}
succeed : b -> Fold a b
succeed b =
    Fold { step = \_ -> succeed b, finish = \_ -> b }



-- DESTRUCTORS


{-| Run a `Fold` on a list of inputs, and then generate a single output. This
is analogous to [`List.foldl`](https://package.elm-lang.org/packages/elm/core/latest/List#foldl).
-}
foldList : List a -> Fold a b -> b
foldList xs fold =
    extract (List.foldl (\a (Fold o) -> o.step a) fold xs)


{-| Run a `Fold` on a list of inputs, generating an output for each input.
-}
scanList : List a -> Fold a b -> List b
scanList xs fold =
    List.map extract (List.Extra.scanl (\a (Fold o) -> o.step a) fold xs)


{-| Step a fold by providing a single input.
-}
stepFold : a -> Fold a b -> Fold a b
stepFold a (Fold o) =
    o.step a


extract : Fold a b -> b
extract (Fold o) =
    o.finish ()



-- COMMON FOLDS


{-| A `Fold` which remembers the first input.
-}
head : Fold a (Maybe a)
head =
    unfoldFold_ Nothing
        (\a m ->
            case m of
                Just _ ->
                    m

                Nothing ->
                    Just a
        )


{-| A `Fold` which keeps the last input.
-}
last : Fold a (Maybe a)
last =
    unfoldFold_ Nothing (\a _ -> Just a)


{-| A `Fold` which tests whether any inputs were seen.
-}
null : Fold a Bool
null =
    unfoldFold_ True (\_ _ -> False)


{-| A `Fold` which counts its inputs.
-}
length : Fold a number
length =
    unfoldFold_ 0 (\_ n -> n + 1)


{-| A `Fold` which tests if _all_ of its inputs were true.
-}
and : Fold Bool Bool
and =
    unfoldFold_ True (&&)


{-| A `Fold` which tests if _any_ of its inputs were true.
-}
or : Fold Bool Bool
or =
    unfoldFold_ False (||)


{-| A `Fold` which tests if _all_ of its inputs satisfy some predicate.
-}
all : (a -> Bool) -> Fold a Bool
all pred =
    unfoldFold_ True (\a b -> b && pred a)


{-| A `Fold` which tests if _any_ of its inputs satisfy some predicate.
-}
any : (a -> Bool) -> Fold a Bool
any pred =
    unfoldFold_ False (\a b -> b || pred a)


{-| A `Fold` which computes the sum of its inputs.
-}
sum : Fold number number
sum =
    unfoldFold_ 0 (+)


{-| A `Fold` which computes the product of its inputs.
-}
product : Fold number number
product =
    unfoldFold_ 1 (*)


{-| A `Fold` which computes the maximum of its inputs, or `Nothing` if there
were no inputs.
-}
maximum : Fold comparable (Maybe comparable)
maximum =
    unfoldFold_ Nothing
        (\a m ->
            case m of
                Nothing ->
                    Just a

                Just b ->
                    Just (max a b)
        )


{-| A `Fold` which computes the minimum of its inputs, or `Nothing` if there
were no inputs.
-}
minimum : Fold comparable (Maybe comparable)
minimum =
    unfoldFold_ Nothing
        (\a m ->
            case m of
                Nothing ->
                    Just a

                Just b ->
                    Just (min a b)
        )


{-| A `Fold` which tests if a specific value appeared as an input.
-}
elem : a -> Fold a Bool
elem a =
    any ((==) a)


{-| A `Fold` which tests if a specific value did not appear as an input.
-}
notElem : a -> Fold a Bool
notElem a =
    all ((/=) a)



-- COMBINATORS


{-| Transform the output of a `Fold`.
-}
map : (b -> c) -> Fold a b -> Fold a c
map f (Fold o) =
    Fold
        { step = \a -> map f (o.step a)
        , finish = \_ -> f (o.finish ())
        }


{-| Combine the results of two `Fold`s run in parallel over the same input.

    average : Fold Float Float
    average =
        map2 (/) sum length

-}
map2 : (b -> c -> d) -> Fold a b -> Fold a c -> Fold a d
map2 f fb fc =
    succeed f |> andMap fb |> andMap fc


{-| Combine the results of three `Fold`s run in parallel over the same input.
-}
map3 :
    (b -> c -> d -> e)
    -> Fold a b
    -> Fold a c
    -> Fold a d
    -> Fold a e
map3 f fb fc fd =
    succeed f |> andMap fb |> andMap fc |> andMap fd


{-| Combine the results of four `Fold`s run in parallel over the same input.
-}
map4 :
    (b -> c -> d -> e -> g)
    -> Fold a b
    -> Fold a c
    -> Fold a d
    -> Fold a e
    -> Fold a g
map4 f fb fc fd fe =
    succeed f |> andMap fb |> andMap fc |> andMap fd |> andMap fe


{-| Combine the results of five `Fold`s run in parallel over the same input.
-}
map5 :
    (b -> c -> d -> e -> g -> h)
    -> Fold a b
    -> Fold a c
    -> Fold a d
    -> Fold a e
    -> Fold a g
    -> Fold a h
map5 f fb fc fd fe fg =
    succeed f
        |> andMap fb
        |> andMap fc
        |> andMap fd
        |> andMap fe
        |> andMap fg


{-| Combine the results of six `Fold`s run in parallel over the same input.
-}
map6 :
    (b -> c -> d -> e -> g -> h -> i)
    -> Fold a b
    -> Fold a c
    -> Fold a d
    -> Fold a e
    -> Fold a g
    -> Fold a h
    -> Fold a i
map6 f fb fc fd fe fg fh =
    succeed f
        |> andMap fb
        |> andMap fc
        |> andMap fd
        |> andMap fe
        |> andMap fg
        |> andMap fh


{-| Chain a `Fold` producing a function onto a `Fold` producing its argument.
Designed for pipeline style, when [`map2`](#map2)–[`map6`](#map6) aren't enough:

    succeed (\a b c -> ( a, b, c ))
        |> andMap foldA
        |> andMap foldB
        |> andMap foldC

-}
andMap : Fold a x -> Fold a (x -> y) -> Fold a y
andMap (Fold x) (Fold f) =
    Fold
        { step = \a -> andMap (x.step a) (f.step a)
        , finish = \_ -> f.finish () (x.finish ())
        }


{-| The comonadic extend operation. Given a function that turns a `Fold` into
an output, produce a `Fold` that emits that output at every step, taking into
account all input seen so far.
-}
extend : (Fold a b -> c) -> Fold a b -> Fold a c
extend f =
    map f << dup


dup : Fold a b -> Fold a (Fold a b)
dup ((Fold o) as fold) =
    Fold
        { step = \a -> dup (o.step a)
        , finish = \_ -> fold
        }


{-| Perform a `Fold` while grouping the data according to a specified group
projection function. Returns the folded result grouped as a `Dict` keyed by
the group.
-}
groupBy : (a -> comparable) -> Fold a r -> Fold a (Dict comparable r)
groupBy grouper f1 =
    let
        combine a m =
            Dict.update (grouper a)
                (\maybeFold ->
                    Just (stepFold a (Maybe.withDefault f1 maybeFold))
                )
                m
    in
    unfoldFold Dict.empty combine (Dict.map (\_ f -> extract f))


{-| `prefilter pred f` returns a new `Fold` based on `f` but where inputs will
only be included if they satisfy the predicate `pred`.
-}
prefilter : (a -> Bool) -> Fold a b -> Fold a b
prefilter pred f =
    let
        maybeStep a s =
            if pred a then
                stepFold a s

            else
                s
    in
    unfoldFold f maybeStep extract
