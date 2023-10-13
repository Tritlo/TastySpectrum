# Design Notes 

This markdown contains some of the relevant thoughts about tasty-spectrum and the overall project. 

## Why is the spectrum creation seperate?

We intentionally split the project into spectrum-creation and spectrum-evaluation. 

This is primarily meant to ease the development of different spectrum-evaluation tools and approaches. 
We hope the provided spectrum is language-agnostic enough that also an python developer can pick it up for their research and projects.

We also hope that it will help with maintaining and publishing the different artifacts. 
E.g. if there is a change to tasty ingredients, we will have to rewrite the spectrum creation, but the remainder of the pipeline is fine. 

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