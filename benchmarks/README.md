# elm-folds benchmarks

Uses [`elm-explorations/benchmark`][bench].

```sh
cd benchmarks
elm reactor          # then open src/Main.elm
# or
elm make src/Main.elm --output=bench.html && open bench.html
```

The suites compare:

- composed `Fold`s (single pass) vs. hand-written `List.foldl` vs. naive
  multi-pass (`List.sum` + `List.length` + …)
- `Fold.groupBy` vs. a manual `Dict` accumulator
- `foldList` vs. `scanList`
- scaling of a 5-field `Stats` fold from 100 → 1k → 10k inputs

[bench]: https://package.elm-lang.org/packages/elm-explorations/benchmark/latest/
