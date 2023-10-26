# Test Notes 

## Example Spectrums 

These spectrums are taken from their respective papers and the exampels they provide for how their algorithms work. 

Formulas are taken from [Should I follow this fault localization tool’s output?
](https://link.springer.com/article/10.1007/s10664-014-9349-1)

[dstar](dstar.csv) is adjusted from [their paper](https://doi.org/10.1109/TR.2013.2285319) 

s3 is the faulty statement. 

expected values (extracted from the paper, Table1): 

| statement | D^2  | D^2 Rank | D^3  | D^3 Rank |
|-----------|------|----------|------|----------|
| s1        | 2    | 4        | 8    | 4        |
| s2        | 2.0  | 4        | 8.0  | 4        |
| s3        | 2.29 | 2        | 9.14 | 1        |
| s4        | 2.29 | 2        | 9.14 | 1        |
| s5        | 0.13 | 8        | 0.13 | 8        |
| s6        | 3.00 | 1        | 9.00 | 3        |
| s7        | 0.25 | 7        | 0.25 | 7        |
| s8        | 1.33 | 5        | 2.67 | 6        |
| s9        | 0.0  | 9        | 0.0  | 9        |

[tarantula](tarantula.csv) is also from [the respective paper](https://dl.acm.org/doi/abs/10.1145/1101908.1101949) but the tests had to be spelled out. 

s7 is the faulty statement.

| statement | suspiciousness | rank |
|-----------|----------------|------|
| 1         | 0.5            | 7    |
| 2         | 0.5            | 7    |
| 3         | 0.5            | 7    |
| 4         | 0.63           | 3    |
| 5         | 0.0            | 13   |
| 6         | 0.71           | 2    |
| 7         | 0.83           | 1    |
| 8         | 0.0            | 13   |
| 9         | 0.0            | 13   |
| 10        | 0.0            | 13   |
| 11        | 0.0            | 13   |
| 12        | 0.0            | 13   |
| 13        | 0.5            | 7    |
