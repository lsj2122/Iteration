Simulation
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

``` r
library(tidyverse)
```

Set seed for reproducibility

``` r
set.seed(1)
```

## Simulate sample mean and sd

Here’s an old function:

``` r
sim_mean_sd = function(n_obs, mu = 5, sigma = 1) {
  
  x_vec = rnorm(n = n_obs, mean = mu, sd = sigma)

  tibble(
    mean = mean(x_vec), 
    sd = sd(x_vec)
  )
  
}
```

Let’s see what this does

``` r
sim_mean_sd(n_obs = 30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.08 0.924

Let’s iterate to see how this works UNDER REPEAT SAMPLING

``` r
output = vector("list", length = 100)

for (i in 1:100) {
  
  output[[i]] = sim_mean_sd(n_obs = 30)
  
}

sim_results = 
  bind_rows(output)

sim_results |> 
  ggplot(aes(x = mean)) + geom_density()
```

<img src="Simulation_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
sim_results |> 
  summarize(
    mu_hat = mean(mean),
    sd_hat = sd(mean)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sd_hat
    ##    <dbl>  <dbl>
    ## 1   4.99  0.192

use a map function

``` r
sim_result_df = 
  expand_grid(
    sample_size = c(30, 60, 120, 240),
    iter = 1:1000
  ) |> 
  mutate(estimate_df = map(sample_size, sim_mean_sd)) |> 
  unnest(estimate_df)

sim_result_df |> 
  mutate(
    sample_size = str_c("n = ", sample_size),
    sample_size = fct_inorder(sample_size)
  ) |> 
  ggplot(aes(x = sample_size, y = mean)) + 
  geom_boxplot()
```

<img src="Simulation_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

## SLR

Goal is to write a function that simulates data and then fits a
regression; then repeat to look at the distribution of estimated
coefficients.

``` r
beta_0 = 2
beta_1 = 3

sim_data = 
  tibble(
    x = rnorm(n = 30, mean = 1, sd = 1),
    y = beta_0 + beta_1 * x + rnorm(30, 0, 1)
  )

ls_fit = lm(y ~ x, data = sim_data)
ls_fit
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = sim_data)
    ## 
    ## Coefficients:
    ## (Intercept)            x  
    ##       1.993        3.100

``` r
sim_data |> 
  ggplot(aes(x = x, y = y)) +
  geom_point()
```

<img src="Simulation_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

Let’s wrap this in a function

``` r
sim_slr = function(n_obs, beta_0 = 2, beta_1 = 3) {
  
  sim_data = 
    tibble(
      x = rnorm(n = 30, mean = 1, sd = 1),
      y = beta_0 + beta_1 * x + rnorm(30, 0, 1)
  )

  ls_fit = lm(y ~ x, data = sim_data)
  
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
  
  
}

sim_slr(n_obs = 30)
```

    ## # A tibble: 1 × 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      1.69      2.83

run this a whole bunch of times

``` r
sim_result_df = 
  expand_grid(
    sample_size = 30,
    iter = 1:1000
  ) |> 
  mutate(estimate_df = map(sample_size, sim_slr)) |> 
  unnest(estimate_df)
```

Let’s look at results

``` r
sim_result_df |> 
  summarize(
    mean_b0_hat = mean(beta0_hat),
    mean_b1_hat = mean(beta1_hat)
  )
```

    ## # A tibble: 1 × 2
    ##   mean_b0_hat mean_b1_hat
    ##         <dbl>       <dbl>
    ## 1        2.00        3.00

``` r
sim_result_df |> 
  ggplot(aes(x = beta0_hat)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="Simulation_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />

``` r
sim_result_df |> 
  ggplot(aes(x = beta0_hat, y = beta1_hat)) +
  geom_point()
```

<img src="Simulation_files/figure-gfm/unnamed-chunk-11-2.png" width="90%" />
