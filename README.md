# `elm-folds`

This library is based on Gabriella Gonzalez' [`foldl` library (in Haskell)](http://hackage.haskell.org/package/foldl).

## Usage

A `Fold` is a "left fold". You can either construct a `Fold` from other `Fold`s using combinators (`map`, `andMap`, etc.) or manually from the `unfoldFold` which takes an initial state, an update function, and a function to produce output from state. Then pass it and an input list to `foldList` to run it and produce a single output, or `scanList` to produce output for each step through the list.

```elm
> foldList (List.range 0 10) sum
55
> avg = map2 (/) sum length
> range = map2 (Maybe.map2 Tuple.pair) minimum maximum
> map2 (\a r -> { avg = a, range = r }) avg range |> foldList [5,1,9,99]
{ avg = 28.5, range = Just (1,99) }
```

## Is it fast?

No, sadly, it is relatively slow. There's a trick with an existential type to make a more efficient encoding (`forall s. (a -> s -> s) s (s -> b)`) in languages that support it, but in the current implementation we are producing a `Fold` at each step through the input list.
