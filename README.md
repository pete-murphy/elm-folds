# `elm-folds`

This library is based on Gabriella Gonzalez' [`foldl` library (in Haskell)](http://hackage.haskell.org/package/foldl).

A `Fold` can either be constructed from combining other `Fold`s using applicative methods (`succeed` & `andMap`) or manually from the low-level `unfoldFold` which takes an initial state, an update function, and a done function (reminiscent of Elm architecture's `init`, `update`, `view`—`Fold` and TEA are both Moore machines).

Here's an example, getting some summary statistics from the [Iris dataset](https://archive.ics.uci.edu/dataset/53/iris):

```elm
type alias Stats =
    { length : Int
    , average : Float
    , variance : Maybe Float
    , stddev : Maybe Float
    , range : Maybe ( Float, Float )
    }


stats : Fold Float Stats
stats =
    Fold.succeed Stats
        |> Fold.andMap Fold.length
        |> Fold.andMap Fold.average
        |> Fold.andMap Fold.variance
        |> Fold.andMap Fold.standardDeviation
        |> Fold.andMap (Fold.map2 (Maybe.map2 Tuple.pair) Fold.minimum Fold.maximum)


type alias Iris =
    { sepalLength : Float
    , sepalWidth : Float
    , petalLength : Float
    , petalWidth : Float
    , species : String
    }


sepalLengthStatsBySpecies : Dict String Stats
sepalLengthStatsBySpecies =
    stats
        |> Fold.premap .sepalLength
        |> Fold.groupBy .species
        -- irisDataset : List Iris
        |> Fold.foldList irisDataset

-- Dict.fromList [("setosa",{ average = 5.005999999999999, length = 50, range = Just (4.3,5.8), stddev = Just 0.3489469873777391, variance = Just 0.121764 }),("versicolor",{ average = 5.936, length = 50, range = Just (4.9,7), stddev = Just 0.5109833656783751, variance = Just 0.26110400000000006 }),("virginica",{ average = 6.587999999999998, length = 50, range = Just (4.9,7.9), stddev = Just 0.6294886813914925, variance = Just 0.396256 })]
```
