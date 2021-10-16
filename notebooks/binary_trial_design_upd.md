Binary Trial Design
================
shett
2021-10-16

Testing a few approaches to compare and contrast different methodologies
for trials with a binary outcome (success/failure trials).

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.6.3

``` r
library(oddsratio)
```

    ## Warning: package 'oddsratio' was built under R version 3.6.3

``` r
library(ggplot2)
```

Defining a config to generate a trial data Defining trial parameters

``` r
trial_config <- list(
  groups = LETTERS[1:2],
  time_units = 30,
  group_a = 0.8,
  group_b = 0.9,
  n = 100
)
```

For this toy example, lets assume that a retail chain has a new process
to improve on-shelf availability of products. One way of testing if the
new process is significantly better is by trialling two different groups
- one with the new process and the control group with the regular
process.

The questions that we want to answer are : \* Is the new process
*significantly* better than the original ? \* If yes, what is the
difference between the groups ? \* Is the trial enough for us to arrive
at a conclusion ?

``` r
# Calculating the odds ratio based on the specified frequencies

actual_odds_ratio = ((trial_config$group_b)/(1 - trial_config$group_b))/((trial_config$group_a)/(1 - trial_config$group_a))

actual_odds_ratio
```

    ## [1] 2.25

THe next step is to generate some simulated data and undee

``` r
## Generating Simulated Trial Data

set.seed(123)

trial_data <- data.table(
  GROUP = rep(trial_config$groups, each = trial_config$time_units * trial_config$n),
  TIME = rep(1:trial_config$time_units, each = trial_config$n * length(trial_config$groups)),
  OUTCOME = c(rbinom(n = trial_config$n * trial_config$time_units, size = 1, prob = trial_config$group_a),rbinom(n = trial_config$n * trial_config$time_units, size = 1, prob = trial_config$group_b))
)

head(trial_data)
```

    ##    GROUP TIME OUTCOME
    ## 1:     A    1       1
    ## 2:     A    1       1
    ## 3:     A    1       1
    ## 4:     A    1       0
    ## 5:     A    1       0
    ## 6:     A    1       1

``` r
## Confusion Matrix of the outcome
```

What are the counts of the successes and failures within each group.

``` r
table(trial_data$GROUP, trial_data$OUTCOME)
```

    ##    
    ##        0    1
    ##   A  592 2408
    ##   B  295 2705

The odds ratio here is (2722/278) / (2378/622) = 2.56 which implies a
positive effect for group B (as the odds ratio \> 1) Also Visualizing
the split

``` r
ggplot(trial_data) +
  aes(x = GROUP, fill = as.factor(OUTCOME)) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(fill = 'Outcome')
```

![](binary_trial_design_upd_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#Using a logistic regression

fit1 <- glm(formula = 'OUTCOME ~ GROUP', family = binomial, data = trial_data)
fit1
```

    ## 
    ## Call:  glm(formula = "OUTCOME ~ GROUP", family = binomial, data = trial_data)
    ## 
    ## Coefficients:
    ## (Intercept)       GROUPB  
    ##      1.4030       0.8128  
    ## 
    ## Degrees of Freedom: 5999 Total (i.e. Null);  5998 Residual
    ## Null Deviance:       5027 
    ## Residual Deviance: 4909  AIC: 4913

Estimating the proprtion for each group

``` r
pB = exp(1.3411 + 0.9404)/(1 + exp(1.3411 + 0.9404))
pB
```

    ## [1] 0.9073332

``` r
pA = exp(1.3411)/(1 + exp(1.3411))
pA
```

    ## [1] 0.7926708

Calculating the relative difference (RD) over the study population

``` r
trial_data[,TREAT := ifelse(GROUP == 'A',0,1)]
trial_data[,pB_est := exp(coef(fit1)["(Intercept)"] + TREAT*coef(fit1)["GROUPB"])]
trial_data[,pA_est := exp(coef(fit1)["(Intercept)"])]
trial_data[,pA_diff := pB_est - pA_est]

RD_est = trial_data[,sum(pA_diff)]/trial_data[,.N]
RD_est
```

    ## [1] 2.550962

Calculating the confidence interval of the odds ratio

``` r
or_glm(data = trial_data, model = fit1)
```

    ##   predictor oddsratio ci_low (2.5) ci_high (97.5)          increment
    ## 1    GROUPB     2.254        1.942          2.622 Indicator variable

Using a chi-square test to re-evaluate the difference

``` r
trial_data[,OUTCOME := as.factor(OUTCOME)]
chisq.test(table(trial_data$GROUP, trial_data$OUTCOME))
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  table(trial_data$GROUP, trial_data$OUTCOME)
    ## X-squared = 115.91, df = 1, p-value < 2.2e-16

So far using a few methods, we could confirm that there is indeed a
significant difference between the two groups.
