#'---
#'title: "Bayesian Difference-in-differences (DiD)"
#'author: Omkar Shetty
#'output: github_document
#'---
#'  
#'  In another notebook, we explored the idea of using difference-in-differences to measure the 
#'  impact of a trial. In this notebook, we will attempt a Bayesian version of the DiD calculation.

#' Setting it up to run on multiple cores - although it may not be necessary for this example.
options(mc.cores = parallel::detectCores())

#' Loading libraries

library(data.table)
library(ggplot2)
library(optiRum)
library(rstanarm)

#' ## Trial Data and Setup  
#'  This is exactly the same data as used in the comparison of measurement methods.

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

#' ## Adding An Uplift

config[['uplift_mean']] = 10
config[['uplift_sd']] = 5

act_uplift <- rnorm(ttl_dt[post ==1 & treatment ==1,.N],config$uplift_mean, config$uplift_sd)
ttl_dt[, uplift := 0]
ttl_dt[post == 1 & treatment == 1, uplift := act_uplift]
ttl_dt[,sales := sales + uplift]

#' Fitting a Bayesian Linear Model with a R2 prior

b_fit = rstanarm::stan_lm(formula = 'sales ~ treatment + post + treatment:post', 
                          data = ttl_dt,
                          prior = R2(location = 0.5, what = "mean"))
summary(b_fit)
plot(b_fit, prob = 0.9, pars = 'treatment:post')

#' Extracting samples from the fit 

x = as.data.frame(b_fit)
ggplot(data = x) +
  geom_histogram(aes(x = `treatment:post`), bins = 20) +
  geom_vline(aes(xintercept = ))
  theme_bw()
