# `elm-folds`

This library is an experiment in porting Gabriella Gonzalez' [`foldl` library (in Haskell)](http://hackage.haskell.org/package/foldl) to Elm.

## Usage

A `Fold` is a "left fold", the arguments to `List.foldl` plus an extraction function bottled up as a data type. You can either construct a `Fold` from other `Fold`s using combinators (`map`, `andMap`, etc.) or manually from the `unfoldFold` which takes an initial state, an update function, and the extraction function to produce output from state (similar to `init`, `update`, `view` in Elm—TEA and `Fold` are Moore machines). Then pass it and an input list to `foldList` to run it and produce a single output, or `scanList` to produce output for each step through the list.

```elm
import Fold exposing (..)

foldList (List.range 0 10) sum
--> 55

avg : Fold Float Float
avg = map2 (/) sum length

range : Fold comparable (Maybe ( comparable, comparable ))
range = map2 (Maybe.map2 Tuple.pair) minimum maximum

map2 (\a r -> { avg = a, range = r }) avg range
    |> foldList [ 5, 1, 9, 99 ]
--> { avg = 28.5, range = Just ( 1, 99 ) }
```

## Is it fast?

No, sadly, it is relatively slow. There's a trick with an existential state `s` type to make a more efficient encoding (`forall s. (a -> s -> s) s (s -> b)`) in languages that support it, which would defer computing `s -> b` as a last step in `foldList`, but in the current implementation we are producing a `Fold` at each step through the input list.
