Bayesian Difference-in-differences (DiD)
================
Omkar Shetty
2021-10-19

In another notebook, we explored the idea of using
difference-in-differences to measure the impact of a trial. In this
notebook, we will attempt a Bayesian version of the DiD calculation.
Setting it up to run on multiple cores - although it may not be
necessary for this example.

``` r
options(mc.cores = parallel::detectCores())
```

Loading libraries

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.6.3

``` r
library(ggplot2)
library(optiRum)
```

    ## Warning: package 'optiRum' was built under R version 3.6.3

``` r
library(rstanarm)
```

    ## Warning: package 'rstanarm' was built under R version 3.6.3

    ## Loading required package: Rcpp

    ## This is rstanarm version 2.21.1

    ## - See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!

    ## - Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.

    ## - For execution on a local, multicore CPU with excess RAM we recommend calling

    ##   options(mc.cores = parallel::detectCores())

## Trial Data and Setup

This is exactly the same data as used in the comparison of measurement
methods.

``` r
config <- list(
  ttl_customers = 10000,
  trial_smp_size = 1000,
  prop_ctrl_grp = 0.5,
  num_trials = 1,
  sales_param_rng = c(10,150),
  sd_sales_param = 5,
  trial_duration_wks = 10,
  pre_trial_weeks = 5
)

cust_dt <- data.table(cust_id = paste0('CN',1:config$ttl_customers),
                      sales_param = runif(min = config$sales_param_rng[1],
                                          max = config$sales_param_rng[2],
                                          n = config$ttl_customers))

trial_cust <- sample(cust_dt[,unique(cust_id)], config$trial_smp_size, replace = F)
trial_cust_dt <- cust_dt[cust_id %in% trial_cust,]

trial_cust_dt[,treatment := ifelse(cust_id %in% sample(unique(trial_cust_dt$cust_id),
                                                       size = config$prop_ctrl_grp*nrow(trial_cust_dt),replace = F),0,1)]

ttl_dt <- CJ.dt(trial_cust_dt,data.table(week = c(1:config$trial_duration_wks)))
ttl_dt[,sales := rnorm(sales_param,mean = sales_param, sd = config$sd_sales_param)]
ttl_dt[,post := ifelse(week > config$pre_trial_weeks,1,0)]

head(ttl_dt)
```

    ##    cust_id sales_param treatment week     sales post
    ## 1:     CN2   111.16014         1    1 111.81922    0
    ## 2:     CN3    75.72116         1    1  73.02846    0
    ## 3:    CN15   145.21497         1    1 153.49124    0
    ## 4:    CN16    50.66424         0    1  48.08899    0
    ## 5:    CN17    99.42235         1    1 104.84542    0
    ## 6:    CN26    77.67860         0    1  79.05878    0

## Adding An Uplift

``` r
config[['uplift_mean']] = 10
config[['uplift_sd']] = 5

act_uplift <- rnorm(ttl_dt[post ==1 & treatment ==1,.N],config$uplift_mean, config$uplift_sd)
ttl_dt[, uplift := 0]
ttl_dt[post == 1 & treatment == 1, uplift := act_uplift]
ttl_dt[,sales := sales + uplift]
```

Fitting a Bayesian Linear Model

``` r
b_fit = rstanarm::stan_glm(formula = 'sales ~ treatment + post + treatment:post', 
                          data = ttl_dt)
```
