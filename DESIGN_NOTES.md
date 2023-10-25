# Design Notes 

This markdown contains some of the relevant thoughts about tasty-spectrum and the overall project. 

## Why is the spectrum creation seperate?

We intentionally split the project into spectrum-creation and spectrum-evaluation. 

This is primarily meant to ease the development of different spectrum-evaluation tools and approaches. 
We hope the provided spectrum is language-agnostic enough that also an python developer can pick it up for their research and projects.

We also hope that it will help with maintaining and publishing the different artifacts. 
E.g. if there is a change to tasty ingredients, we will have to rewrite the spectrum creation, but the remainder of the pipeline is fine. 

## How to read a spectrum.csv?

Example (part) of a spectrum: 

```
test_name,test_result,"test\\Main.hs:36:8-36:19","test\\Main.hs:36:22-36:22","test\\Main.hs:36:7-36:26",...
"sort == sort . reverse",True,1,0,0, ...
"other test",False,0,10,100, ...
...
```

1) The header contains all expressions, matching to their locations of HPC. This means, a single spectrum can contain elements from multiple hpc-modules
2) Each Row resembles (exactly) one test. Properties or other elements also have exactly one row. 
3) a test-result "True" means the test passed. "False" means it failed or threw an error. 
4) an expression follows this standard format: `path/to/file.hs:start_row:start_column-end_row:end_column` 
5) An entry of 0 means "this expression was not executed by this test", a number of 1 means a single execution, 100 translates to 100 executions and so on.

## "Sparse" Spectrum as default

With a sparse spectrum we denote a spectrum where non-touched statements are *completely removed*. 
This means, any row of the `.csv` which contains only `0s` are striked out. 

This brings the primary benefit of saving us disk space. 
When prototyping with the Hasbugs Dataset, the Pandoc-4 datapoint had ~1gb of "non-sparse" spectrum and ~500mb of sparse spectrum. 

We currently do not expect too many down-sides of it, as many FL techniques either ignore untouched statements inherently in their formulas, 
and if we come accross a follow up problem when encountering an un-known expression we can just assume a row of 0s. 

There is an argument that some FL formulas compare vectors or are dependent on the amount of (untouched) expressions. 
We aknowledge this, but we expect that a statement that is not touched by any test maybe affects the output of the formulas, but should not alter the produced ranking. 

## "Rich" .Csv vs. seperate files

Other approaches, namely [GZoltar](https://github.com/GZoltar/gzoltar) produces different files that constitute the complete spectrum. 
Namely, Gzoltar lists tests and statements separate and the provided matrix is only 0s, 1s, and +/- for test success. 
The approach by Gzoltar is favorable for certain compression algorithms such as `zip`.

We currently favor an approach of a *fully complete* .csv with human-readable identifiers. 
This is mostly due to us checking if things look all-right, and we ease the later use in other tools. 
Iff we'd outsource the locations by referring to the mix files, follow up tools might require access to HPCs library. 
This would hinder writing a new tool or evaluation in e.g. Python. 

We hope to provide the spectrums in the most-reusable way for other researchers, which is why we try to avoid the "`.mix`-barrier". 

## Legacy: Ideas of different Spectrum Layout 

Extracted from `Spectrum.hs`:

```Haskell
{-
Legacy Comment: Ideas on CSV Design

               test status (t_res). A string? Leave it bool for now.
                       ↓
         test_name, test_result  , e1 , e2 <- mix_file location
 test_name <-   t1,   y (1/1)    , 1  , 27 <- number of evals
                t2,   y          , 0  , 17
                t3,   n (0/1)    , 17 , 5
                t4,   n          , 5  , 0
                q1,   n (4/5)    , 7  , 5
                        ↑
                      from amount of tests, if < 1 then fail
think about data format..?
-}
```

There are some elements we might want to represent that are currently (18-10-2023) not represented: 

- Categories of tests: unit-tests, properties, golden tests ... 
- More explicit test_results: How many counterexamples were found for a property ? Was the test failing, erroring, timeouting? 
- Info on the expression, highlighting some relevant AST info
