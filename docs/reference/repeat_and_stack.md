# Repeat an expression multiple times and (optionally) stack the results.

Repeat an expression (usually involving random number generation)
multiple times. Optionally, organize the results into a `data.frame`
that stacks the output from all replications of the expression.

## Usage

``` r
repeat_and_stack(n, expr, id = NULL, stack = TRUE)
```

## Arguments

- n:

  Number of times to repeat the expression

- expr:

  An expression to be evaluated.

- id:

  Character string to use for creating a variable with a unique
  identifier for each repetition. If set to `NULL` (the default), then
  no identifier is created.

- stack:

  Logical value indicating whether to organize the results into a
  `data.frame`.

## Value

If `stack = TRUE` (the default), the results of each evaluation of
`expr` will be stacked together using `rbind` and a unique identifier
will be stored in the variable `id` (if specified). If `stack = FALSE`,
a list of length `n` with entries corresponding to the output of each
replication of `expr`, with names corresponding to the unique identifier
(if specified)

## Examples

``` r
repeat_and_stack(n = 3, data.frame(x = rexp(2)))
#>           x
#> 1 1.7272566
#> 2 4.3115028
#> 3 1.2542130
#> 4 0.7333867
#> 5 0.4364060
#> 6 2.9640214
repeat_and_stack(n = 3, data.frame(x = rexp(2)), id = "ID")
#>   ID          x
#> 1  1 2.68590474
#> 2  1 0.51607648
#> 3  2 0.43494982
#> 4  2 1.25056845
#> 5  3 0.04620374
#> 6  3 0.34424903

repeat_and_stack(n = 3, data.frame(x = rexp(2)), stack = FALSE)
#> [[1]]
#>          x
#> 1 1.237698
#> 2 1.406672
#> 
#> [[2]]
#>          x
#> 1 1.829160
#> 2 1.101649
#> 
#> [[3]]
#>          x
#> 1 0.234430
#> 2 1.332338
#> 
repeat_and_stack(n = 3, data.frame(x = rexp(2)), id = "ID", stack = FALSE)
#> $`1`
#>           x
#> 1 0.4213795
#> 2 1.2648624
#> 
#> $`2`
#>           x
#> 1 0.2104247
#> 2 0.4283194
#> 
#> $`3`
#>            x
#> 1 1.39053525
#> 2 0.08336016
#> 
```
