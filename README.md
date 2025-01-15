# Tasty Spectrum 

Tasty Ingredient that creates a coverage-based fault-localization-spectrum.

*Note*: It re-runs all tests, which depending on your test-suite can become burdensome. 
Please keep this in mind and only run it on-demand if you are in need of a spectrum.

## Build and Run 

**Requirements** (used by us)

- GHC  9.6+
- Cabal 3.10.1.0
- Linux or WSL 

```
cabal build
cabal test tasty-spectrum-tests
cabal test tasty-spectrum-example --test-options --get-spectrum
```

*Other available flags*:

```
--hpc-dir .hpc
--spectrum-out hectrum.csv
--non-sparse-spectrum 
```

Use like: `cabal test tasty-spectrum-example --test-options "--get-spectrum --spectrum-out=./spectrum.csv"`

And to print a tree view of the expressions: `cabal run tasty-sbfl -- spectrum.csv tree`

And to print the expressions as ranked by ALG: `cabal run tasty-sbfl -- spectrum.csv ALG`,
where `ALG` is one of:
+ `tarantula`
+ `ochiai`
+ `dstar K`, where `K` is the number to be used by the dstar algorithm.

To limit the number of results to the top N expressions (as determined by ALG), use `--limit N`.

See `cabal run tasty-sbfl -- --help` for more information.


To run from start to finish:
1. Add the -fhpc flag to the testsuite (and executables if applicable)
2. Write a `parameter` file for your project into `$DP.params`,
   where `$DP` is e.g. `p4`, which should look like this:
    ```
    Params { bug_locs = [("src/Text/Pandoc/Readers/LaTeX.hs",[(350,350),
                                                              (371,371),
                                                              (410,411)])],
              machine_parameters = LP { network = [20,40,40],
                                        chunk_size = 250,
                                        l_r_0 = 0.9}}
    ```
3. Run the pipline:
    ```
    export DP="p4"
    cabal test test-pandoc --test-options "--get-spectrum --spectrum-out $(pwd)/$DP.csv --hpc-dir $(pwd)/.hpc"
    cabal run tasty-sbfl -- $DP.csv rules > $DP.result
    cabal run tasty-sbfl -- $DP.result weights $DP.params
    ```

*Testing*: 

We have a few [shelltests](https://github.com/simonmichael/shelltestrunner/tree/master), which you can run with:
```sh
shelltest test
```

## Application to other Projects

We assume you have a cabal project with a tasty test suite, that you can run with `cabal test all`.

1. make sure to compile the project with coverage enabled. We just add the ghc-option `-fhpc` to all relevant modules in the cabal file. You can alternatevely run `cabal build all --ghc-options "-fhpc"`, but this will compile *all* dependencies with coverage and might be too much overhead. 
2. (Beta Only) copy this project into your targets projects repository, and add it to the cabal.project for building. In later stages this should be surpassed by getting things from hackage. 
3. Add `tasty-spectrum` to the target-projects dependencies.
4. In all relevant target-project test-files, add `import Test.Tasty.Ingredients.Spectrum` and change the `defaultMain` of tasty to e.g. `defaultMainWithIngredients (testSpectrum : defaultIngredients) tests`. If you already run a custom defaultMain, make sure to add the testSpectrum too.
5. Run the project with `cabal test --test-options "--get-spectrum --spectrum-out=$(pwd)/hectrum.csv --hpc-dir=$(pwd)/.hpc"`. Adjust paths accordingly for non-default projects.

We also opened a little [Troubleshooting](./KNOWN_ISSUES.md) and some [design decisions](./DESIGN_NOTES.md)

*Important*: The resulting spectrum might take some disk space. Our run on pandoc produces a ~500mb spectrum. 

## Filtering

To filter, you use expressions:

```sh
tasty-sbfl example.csv filter --expr "rOchiai >= 0.5"
```

Expressions have the following grammar:

```
<expr> ::= <comp>
        | not (<expr>)
        | (<expr>) && (<expr>)
        | (<expr>) || (<expr>)

<comp> ::= <rule> <op> <Double>
<op>   ::= ">=" | "<=" | ">" | "<" | "=="
<rule> ::= "rTFail"
         | "rTPass"
         | "rPropFail"
         | "rPropPass"
         | "rUnitFail"
         | "rUnitPass"
         | "rGoldenFail"
         | "rGoldenPass"
         | "rOtherTestFail"
         | "rOtherTestPass"
         | "rTFailFreq"
         | "rTPassFreq"
         | "rTFailUniqueBranch"
         | "rJaccard"
         | "rHamming"
         | "rOptimal"
         | "rOptimalP"
         | "rTarantula"
         | "rOchiai"
         | "rDStar2"
         | "rDStar3"
         | "rRogot1"
         | "rASTLeaf"
         | "rTFailFreqDiffParent"
         | "rDistToFailure"
         | "rIsIdentifier"
         | "rTypeLength"
         | "rTypeArity"
         | "rTypeOrder"
         | "rTypeFunArgs"
         | "rTypeConstraints"
         | "rTypePrimitives"
         | "rTypeSubTypes"
         | "rTypeArrows"
         | "rTarantulaQuantile"
         | "rOchiaiQuantile"
         | "rDStar2Quantile"
         | "rDStar3Quantile"
         | "rNumIdFails"
         | "rNumTypeFails"
         | "rNumSubTypeFails"
```

Examples (note we need a lot of parenthesis, but *not* if we only have one expression):

```
  rOchiai >= 0.1

  (rOchiai >= 0.2) || (not (rASTLeaf == 0.0))

  (rTarantula < 0.7) && (rIsIdentifier == 1.0)

```
