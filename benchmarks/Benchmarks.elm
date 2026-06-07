module Benchmarks exposing (main)

{-| Benchmarks for `elm-folds`.

Each suite compares a composed `Fold` (which traverses the list once,
regardless of how many summaries it produces) against equivalent dedicated
single-pass implementations and the naive multi-pass approach.

Run with `elm-benchmark` (e.g. `elm reactor` and open `Main.elm`, or
`elm make src/Main.elm --output=bench.html`).

-}

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict exposing (Dict)
import Fold exposing (Fold)



-- DATA


floats : List Float
floats =
    -- A deterministic pseudo-random sequence so the input is realistic
    -- (not just `List.range`) without depending on `elm/random` at runtime.
    List.range 1 1000
        |> List.map
            (\i ->
                let
                    x =
                        toFloat (modBy 9973 (i * 2654435761))
                in
                x / 9973
            )


floatsSmall : List Float
floatsSmall =
    List.take 100 floats


floatsLarge : List Float
floatsLarge =
    -- 10k items, by tiling the 1k-sequence
    List.concat (List.repeat 10 floats)


grouped : List ( String, Float )
grouped =
    let
        bucket i =
            case modBy 4 i of
                0 ->
                    "a"

                1 ->
                    "b"

                2 ->
                    "c"

                _ ->
                    "d"
    in
    floats
        |> List.indexedMap (\i x -> ( bucket i, x ))



-- HAND-WRITTEN BASELINES


sumLengthAvgFold : Fold ( Float, Int, ( Float, Float ) ) Float ( Float, Int, Float )
sumLengthAvgFold =
    Fold.map3 (\s n a -> ( s, n, a )) Fold.sum Fold.length Fold.average


sumLengthAvgManual : List Float -> ( Float, Int, Float )
sumLengthAvgManual xs =
    let
        ( s, n ) =
            List.foldl (\x ( a, b ) -> ( a + x, b + 1 )) ( 0, 0 ) xs
    in
    ( s, n, s / toFloat n )


sumLengthAvgNaive : List Float -> ( Float, Int, Float )
sumLengthAvgNaive xs =
    let
        s =
            List.sum xs

        n =
            List.length xs
    in
    ( s, n, s / toFloat n )


minMaxFold : Fold ( Maybe Float, Maybe Float ) Float ( Maybe Float, Maybe Float )
minMaxFold =
    Fold.map2 Tuple.pair Fold.minimum Fold.maximum


minMaxManual : List Float -> ( Maybe Float, Maybe Float )
minMaxManual xs =
    List.foldl
        (\x ( mn, mx ) ->
            ( Just (Maybe.withDefault x (Maybe.map (min x) mn))
            , Just (Maybe.withDefault x (Maybe.map (max x) mx))
            )
        )
        ( Nothing, Nothing )
        xs


minMaxNaive : List Float -> ( Maybe Float, Maybe Float )
minMaxNaive xs =
    ( List.minimum xs, List.maximum xs )


type alias Stats =
    { length : Int
    , sum : Float
    , avg : Float
    , min : Maybe Float
    , max : Maybe Float
    }


-- Annotation omitted: an `andMap` pipeline accumulates a nested-tuple state
-- type which is noisy to spell out by hand. Elm infers it perfectly.
statsFold =
    Fold.succeed Stats
        |> Fold.andMap Fold.length
        |> Fold.andMap Fold.sum
        |> Fold.andMap Fold.average
        |> Fold.andMap Fold.minimum
        |> Fold.andMap Fold.maximum


statsNaive : List Float -> Stats
statsNaive xs =
    let
        s =
            List.sum xs

        n =
            List.length xs
    in
    { length = n
    , sum = s
    , avg = s / toFloat n
    , min = List.minimum xs
    , max = List.maximum xs
    }



-- SUITES


suite : Benchmark
suite =
    Benchmark.describe "elm-folds"
        [ Benchmark.describe "sum + length + average (1k Floats)"
            [ Benchmark.benchmark "Fold (composed, 1 pass)" <|
                \_ -> Fold.foldList floats sumLengthAvgFold
            , Benchmark.benchmark "manual foldl tuple (1 pass)" <|
                \_ -> sumLengthAvgManual floats
            , Benchmark.benchmark "List.sum + List.length (2 pass)" <|
                \_ -> sumLengthAvgNaive floats
            ]
        , Benchmark.describe "min + max (1k Floats)"
            [ Benchmark.benchmark "Fold (composed, 1 pass)" <|
                \_ -> Fold.foldList floats minMaxFold
            , Benchmark.benchmark "manual foldl (1 pass)" <|
                \_ -> minMaxManual floats
            , Benchmark.benchmark "List.minimum + List.maximum (2 pass)" <|
                \_ -> minMaxNaive floats
            ]
        , Benchmark.describe "5-field Stats (1k Floats)"
            [ Benchmark.benchmark "Fold (composed, 1 pass)" <|
                \_ -> Fold.foldList floats statsFold
            , Benchmark.benchmark "naive (multiple passes)" <|
                \_ -> statsNaive floats
            ]
        , Benchmark.describe "variance / stddev (Welford, 1k Floats)"
            [ Benchmark.benchmark "Fold.variance" <|
                \_ -> Fold.foldList floats Fold.variance
            , Benchmark.benchmark "Fold.standardDeviation" <|
                \_ -> Fold.foldList floats Fold.standardDeviation
            , Benchmark.benchmark "Fold.mean + variance + stddev (shared)" <|
                \_ ->
                    Fold.foldList floats
                        (Fold.map3 (\m v s -> ( m, v, s ))
                            Fold.mean
                            Fold.variance
                            Fold.standardDeviation
                        )
            ]
        -- , Benchmark.describe "groupBy stats (1k tagged Floats, 4 groups)"
        --     [ Benchmark.benchmark "Fold.groupBy + composed stats" <|
        --         \_ ->
        --             grouped
        --                 |> Fold.foldList
        --                     (statsFold
        --                         |> Fold.premap Tuple.second
        --                         |> Fold.groupBy Tuple.first
        --                     )
        --     , Benchmark.benchmark "manual Dict accumulator" <|
        --         \_ -> manualGroupedStats grouped
        --     ]
        , Benchmark.describe "scanList vs foldList (sum, 1k Floats)"
            [ Benchmark.benchmark "foldList sum" <|
                \_ -> Fold.foldList floats Fold.sum
            , Benchmark.benchmark "scanList sum" <|
                \_ -> Fold.scanList floats Fold.sum
            ]
        , Benchmark.describe "scaling: 5-field Stats"
            [ Benchmark.benchmark "Fold @ 100" <|
                \_ -> Fold.foldList floatsSmall statsFold
            , Benchmark.benchmark "Fold @ 1k" <|
                \_ -> Fold.foldList floats statsFold
            , Benchmark.benchmark "Fold @ 10k" <|
                \_ -> Fold.foldList floatsLarge statsFold
            ]
        ]


manualGroupedStats : List ( String, Float ) -> Dict String Stats
manualGroupedStats xs =
    -- Two-pass per group: collect values into Dict, then run statsNaive.
    -- This is the obvious hand-written approach `groupBy` competes against.
    let
        bucketed : Dict String (List Float)
        bucketed =
            List.foldl
                (\( k, v ) d ->
                    Dict.update k
                        (\m ->
                            case m of
                                Nothing ->
                                    Just [ v ]

                                Just vs ->
                                    Just (v :: vs)
                        )
                        d
                )
                Dict.empty
                xs
    in
    Dict.map (\_ vs -> statsNaive vs) bucketed


main : BenchmarkProgram
main =
    program suite
