# Summarize the sums of squares attributable to each factor in a full factorial experimental design

Computes an analysis of variance for a set of factors in a full
factorial experimental design. For each term order within the design
(i.e., main effects, two-way interactions, three-way interactions), the
total sum of squares attributable to each factor is computed. For terms
beyond order

1.  Optionally, also computes the total sum of squares for each order
    (across all factors) and the total sum of squares attributable to
    each factor.

## Usage

``` r
factorial_summary(
  data,
  y,
  factors,
  sum_orders = TRUE,
  include_total = TRUE,
  check_balance = TRUE
)
```

## Arguments

- data:

  data.frame or tibble containing simulation results. Each row should
  correspond to a unique set of parameter values.

- y:

  character string corresponding to the outcome variable in `data`.

- factors:

  character vector containing the names of two or more variables in
  `data` that correspond to factors in the experimental design.

- sum_orders:

  logical indicating whether to compute the total sum of squares
  attributable to each factor, with a default of `TRUE`.

- include_total:

  logical indicating whether to compute the total sum of squares for
  each term order (across all factors), with a default of `TRUE`.

- check_balance:

  logical indicating whether to check that the experimental design is
  balanced, with a default of `TRUE`.

## Value

A data.frame

## Examples

``` r

data("Chen_Pusto")
dat <- subset(Chen_Pusto, method == "PET-PEESE")

factorial_summary(dat, "bias", c("k","mu","tau","cor_mu","wts"))
#>   factor d.f.      Order 1     Order 2     Order 3     Order 4     Order 5
#> 1      k    3 0.1389360239 0.095743716 0.062559309 0.007415054 0.003567425
#> 2     mu    3 0.8150849247 0.929389444 0.231529831 0.008377466 0.003567425
#> 3    tau    3 0.9466150845 1.111121964 0.215655348 0.007856071 0.003567425
#> 4 cor_mu    2 0.0004559467 0.003466843 0.004117043 0.005308448 0.003567425
#> 5    wts    5 0.7150778461 1.547949574 0.209064274 0.008403422 0.003567425
#> 6  Total   NA 2.6161698260 1.843835770 0.240975268 0.009340115 0.003567425
#>          Sum
#> 1 0.30822153
#> 2 1.98794909
#> 3 2.28481589
#> 4 0.01691571
#> 5 2.48406254
#> 6 4.71388840
factorial_summary(dat, "bias", c("k","mu","tau","wts"), include_total = FALSE)
#>      factor d.f.    Order 1   Order 2    Order 3     Order 4        Sum
#> 1         k    3 0.13893602 0.0956514 0.06103523 0.004031667 0.29965433
#> 2        mu    3 0.81508492 0.9285014 0.22907022 0.004031667 1.97668817
#> 3       tau    3 0.94661508 1.1104292 0.21368031 0.004031667 2.27475626
#> 4       wts    5 0.71507785 1.5461559 0.20678891 0.004031667 2.47205432
#> 5 Residuals  768 0.01691571 0.0000000 0.00000000 0.000000000 0.01691571
```
