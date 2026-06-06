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


extract : Fold a b -> b
extract (Fold o) =
    o.finish ()
