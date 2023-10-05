# Tasty Spectrum 

Tasty Ingredient that created a coverage-based fault-localization-spectrum.

*Note*: It re-runs all tests, which depending on your test-suite can become burdensome. 
Please keep this in mind and only run it on-demand if you are in need of a spectrum.

## Build and Run 

**Requirements** (used by us)

- GHC  9.6.3
- Cabal 3.10.1.0

```
cabal build
cabal test --test-options --get-spectrum
```

*Other available flags*:

```
--hpc-dir .hpc
--spectrum-out hectrum
```

Use like: `cabal test --test-options "--get-spectrum --spectrum-out=.hpc/hectrum.csv"`

## Application to other Projects

We assume you have a cabal project with a tasty test suite, that you can run with `cabal test all`.

1. make sure to compile the project with coverage enabled. We just add the ghc-option `-fhpc` to all relevant modules in the cabal file. You can also do so by specyfing `cabal build all --ghc-options "-fhpc"`, but this will compile *all* dependencies with coverage and might be too much overhead. 
2. 