module StatisticsTest exposing (suite)

import Expect
import Fold
import Fuzz
import Test exposing (Test, describe, fuzz, test)


{-| Tolerance for floating-point comparisons.
-}
tol : Float
tol =
    1.0e-9


expectMaybeWithin : Float -> Maybe Float -> Maybe Float -> Expect.Expectation
expectMaybeWithin t actual expected =
    case ( actual, expected ) of
        ( Nothing, Nothing ) ->
            Expect.pass

        ( Just a, Just b ) ->
            Expect.within (Expect.AbsoluteOrRelative t t) a b

        _ ->
            Expect.fail
                ("expected " ++ Debug.toString expected ++ " but got " ++ Debug.toString actual)


suite : Test
suite =
    describe "Statistical folds"
        [ describe "average"
            [ test "is NaN for the empty list" <|
                \_ ->
                    Fold.foldList [] Fold.average
                        |> isNaN
                        |> Expect.equal True
            , test "is the single value for a singleton" <|
                \_ ->
                    Fold.foldList [ 42.0 ] Fold.average
                        |> Expect.within (Expect.AbsoluteOrRelative tol tol) 42.0
            , test "computes the arithmetic mean" <|
                \_ ->
                    Fold.foldList [ 1.0, 2.0, 3.0, 4.0 ] Fold.average
                        |> Expect.within (Expect.AbsoluteOrRelative tol tol) 2.5
            , test "agrees with sum / length" <|
                \_ ->
                    let
                        xs =
                            [ 1.0, 2.0, 3.0, 4.0, 5.0 ]
                    in
                    Fold.foldList xs Fold.average
                        |> Expect.within (Expect.AbsoluteOrRelative tol tol)
                            (List.sum xs / toFloat (List.length xs))
            ]
        , describe "mean"
            [ test "is Nothing for the empty list" <|
                \_ ->
                    Fold.foldList [] Fold.mean
                        |> Expect.equal Nothing
            , test "is the single value for a singleton" <|
                \_ ->
                    Fold.foldList [ 42.0 ] Fold.mean
                        |> expectMaybeWithin tol (Just 42.0)
            , test "computes the arithmetic mean" <|
                \_ ->
                    Fold.foldList [ 1.0, 2.0, 3.0, 4.0 ] Fold.mean
                        |> expectMaybeWithin tol (Just 2.5)
            , fuzz (Fuzz.listOfLengthBetween 1 50 (Fuzz.floatRange -1000 1000)) "agrees with sum / length" <|
                \xs ->
                    let
                        naive =
                            List.sum xs / toFloat (List.length xs)
                    in
                    Fold.foldList xs Fold.mean
                        |> expectMaybeWithin 1.0e-6 (Just naive)
            ]
        , describe "variance (population)"
            [ test "is Nothing for the empty list" <|
                \_ ->
                    Fold.foldList [] Fold.variance
                        |> Expect.equal Nothing
            , test "is 0 for a singleton" <|
                \_ ->
                    Fold.foldList [ 7.0 ] Fold.variance
                        |> expectMaybeWithin tol (Just 0.0)
            , test "is 0 for constant inputs" <|
                \_ ->
                    Fold.foldList [ 5.0, 5.0, 5.0, 5.0 ] Fold.variance
                        |> expectMaybeWithin tol (Just 0.0)
            , test "computes population variance of [1,2,3,4,5]" <|
                \_ ->
                    -- mean = 3; squared deviations: 4,1,0,1,4 → sum 10 → /5 = 2
                    Fold.foldList [ 1.0, 2.0, 3.0, 4.0, 5.0 ] Fold.variance
                        |> expectMaybeWithin tol (Just 2.0)
            , test "is shift-invariant" <|
                \_ ->
                    let
                        xs =
                            [ 1.0, 2.0, 3.0, 4.0, 5.0 ]

                        shifted =
                            List.map ((+) 1.0e6) xs
                    in
                    case ( Fold.foldList xs Fold.variance, Fold.foldList shifted Fold.variance ) of
                        ( Just a, Just b ) ->
                            Expect.within (Expect.AbsoluteOrRelative 1.0e-6 1.0e-6) a b

                        _ ->
                            Expect.fail "expected Just values"
            ]
        , describe "sampleVariance"
            [ test "is Nothing for the empty list" <|
                \_ ->
                    Fold.foldList [] Fold.sampleVariance
                        |> Expect.equal Nothing
            , test "is Nothing for a singleton (undefined)" <|
                \_ ->
                    Fold.foldList [ 7.0 ] Fold.sampleVariance
                        |> Expect.equal Nothing
            , test "computes sample variance of [1,2,3,4,5]" <|
                \_ ->
                    -- sum of squared deviations = 10; /(n-1)=4 → 2.5
                    Fold.foldList [ 1.0, 2.0, 3.0, 4.0, 5.0 ] Fold.sampleVariance
                        |> expectMaybeWithin tol (Just 2.5)
            ]
        , describe "standardDeviation (population)"
            [ test "is Nothing for the empty list" <|
                \_ ->
                    Fold.foldList [] Fold.standardDeviation
                        |> Expect.equal Nothing
            , test "is the sqrt of population variance" <|
                \_ ->
                    Fold.foldList [ 1.0, 2.0, 3.0, 4.0, 5.0 ] Fold.standardDeviation
                        |> expectMaybeWithin tol (Just (sqrt 2.0))
            ]
        , describe "sampleStandardDeviation"
            [ test "is Nothing for a singleton" <|
                \_ ->
                    Fold.foldList [ 1.0 ] Fold.sampleStandardDeviation
                        |> Expect.equal Nothing
            , test "is the sqrt of sample variance" <|
                \_ ->
                    Fold.foldList [ 1.0, 2.0, 3.0, 4.0, 5.0 ] Fold.sampleStandardDeviation
                        |> expectMaybeWithin tol (Just (sqrt 2.5))
            ]
        ]
