module Fold exposing
    ( Fold
    , foldList
    , scanList
    , stepFold
    , unfoldFold
    , unfoldFold_
    )

{-| -}

import List.Extra


{-| A left fold, which takes zero or more values of type `a` as input
and produces output of type `b`.
-}
type Fold a b
    = Fold
        { step : a -> Fold a b
        , finish : () -> b
        }


{-| Step a fold by providing a single input.
-}
stepFold : a -> Fold a b -> Fold a b
stepFold a (Fold o) =
    o.step a


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
that state. This is a variant of `unfoldFold` where the output is the state
itself.
-}
unfoldFold_ : b -> (a -> b -> b) -> Fold a b
unfoldFold_ s0 step =
    unfoldFold s0 step identity


{-| Run a `Fold` on a list of inputs, and then generate a single output. This is
analogous to the `List.foldl` function.
-}
foldList : List a -> Fold a b -> b
foldList xs fold =
    extract (List.foldl (\a (Fold o) -> o.step a) fold xs)


{-| Run a `Fold` on a list of inputs, generating an output for each input.
-}
scanList : List a -> Fold a b -> List b
scanList xs fold =
    List.map extract (List.Extra.scanl (\a (Fold o) -> o.step a) fold xs)


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


extract : Fold a b -> b
extract (Fold o) =
    o.finish ()
