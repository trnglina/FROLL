# froll - Just a dice roller.

**Abandoned as of 04/03/2018**

A dice roller accepting basic dice notation written in Fortran.

## Supported inputs

Basic standard notation is accepted, such as `d8`, `2d4`, `3d12 + 8`, along with multiple rolls, such as `d8 + d12 - 3d6 + 7 - 2d8`.

## Missing features

In its current state, many features are not yet supported, yet support may be added in the future.

- Multiplication and division `1d6 * 6`, `2d8 / 10`

- Percentile shorthand `d%` vs `d100`

- Exploding dice

- Comparisons

- Fudge dice

- Selective results

## TODO

- [ ] Validate input in `froll` module.

- [ ] Build a new command line program

## Why Fortran

This was primarily an exercise in learning Fortran. As a result, the code may not be idiomatic (if idiomatic Fortran exists these days) or optimal.

## License

froll is Public Domain software available under the Creative Commons 0. See [LICENSE](LICENSE) and [COPYING](COPYING) for more information.
