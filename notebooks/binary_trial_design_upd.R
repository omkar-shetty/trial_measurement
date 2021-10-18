#'---
#'title: "Binary Trial Design [WIP]"
#'author: Omkar Shetty
#'output: github_document
#'---
#'  
#'  Testing a few approaches to compare and contrast different methodologies for trials with a binary outcome (success/failure trials).


library(data.table)
library(oddsratio)
library(ggplot2)

#' ## Setup Trial Data and Configuration
#' Defining trial parameters

trial_config <- list(
  groups = LETTERS[1:2],
  time_units = 30,
  group_a = 0.8,
  group_b = 0.9,
  n = 100
)

#'For this toy example, lets assume that a retail chain has a new process to improve on-shelf availability of products.
#'One way of testing if the new process is significantly better is by trialling two different groups i.e., one with the new process and the control group with the regular process.
#'
#'The questions that we want to answer are :  
#'  - Is the new process *significantly* better than the original ?  
#'  - If yes, what is the difference between the groups ?  
#'  - Is the trial enough for us to arrive at a conclusion ?  
  
  
#' Calculating the odds ratio based on the specified frequencies

actual_odds_ratio = ((trial_config$group_b)/(1 - trial_config$group_b))/((trial_config$group_a)/(1 - trial_config$group_a))
actual_odds_ratio

#' ## Generating Simulated Trial Data
#' Here, the outcome column represents whether or not a particular item was available in the 
#' specified group on the specified time.

set.seed(123)

trial_data <- data.table(
  GROUP = rep(trial_config$groups, each = trial_config$time_units * trial_config$n),
  TIME = rep(1:trial_config$time_units, each = trial_config$n * length(trial_config$groups)),
  OUTCOME = c(rbinom(n = trial_config$n * trial_config$time_units, size = 1, prob = trial_config$group_a),rbinom(n = trial_config$n * trial_config$time_units, size = 1, prob = trial_config$group_b))
)

head(trial_data)

## Confusion Matrix of the outcome  
#'What are the counts of the successes and failures within each group.

table(trial_data$GROUP, trial_data$OUTCOME)

#' The odds ratio here is about 2.25 which implies a positive effect for group B (as the odds ratio > 1) 
#' (As a reminder, the odds ratio in this case is the ratio of the odds of Group B to the odds of Group A ).
#' Based on these figures, the odds are 2.25 times higher that items from Group B will be available.  
#' However, we still don't know if the odds ratio is statisticlly significant.

# Visualizing the split between the groups

ggplot(trial_data) +
  aes(x = GROUP, fill = as.factor(OUTCOME)) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(fill = 'Outcome')

#' ## Chi Squared Test
#'Using a chi-square test is a standard way to calculate the p-value. The Chi-square test 
#'compares the observed values of the outcome to the expected values - assuming that there 
#'isn't a relation between the group and the outcome.

trial_data[,OUTCOME := as.factor(OUTCOME)]
chisq.test(table(trial_data$GROUP, trial_data$OUTCOME))

#' The p-value does seem to indicate a statistically significant result.  
#'  

#' ## Logistic Regression
#' Another approach to estimate the odds ratio is by using logistic regression.  
#' Here, the GroupB coefficient can be used to calculate the odds ratio.

fit1 <- glm(formula = 'OUTCOME ~ GROUP', family = binomial, data = trial_data)
summary(fit1)

#' Calculating the odds ratio

exp(coef(summary(fit1))["GROUPB","Estimate"])

#' As in the previous case, using Chi square, the value of the odds ratio matches the 
#' original as well as the effect is statistically significant.  

#' Using the odds ratio package, the confidence interval of the estimated odds ratio 
#' can also be calculated.

or_glm(data = trial_data, model = fit1)

#' ## Additional Metrics
#' In additon to the odds ratio, metrics such as the relative difference can also be useful in
#' estimating the effect.

#' Estimating the proprtion for each group

pB = exp(1.403 + 0.8128)/(1 + exp(1.403 + 0.8128))
pB

pA = exp(1.403)/(1 + exp(1.403))
pA


#' Calculating the relative difference (RD) over the study population


trial_data[,TREAT := ifelse(GROUP == 'A',0,1)]
trial_data[,pB_est := exp(coef(fit1)["(Intercept)"] + TREAT*coef(fit1)["GROUPB"])]
trial_data[,pA_est := exp(coef(fit1)["(Intercept)"])]
trial_data[,pA_diff := pB_est - pA_est]

RD_est = trial_data[,sum(pA_diff)]/trial_data[,.N]
RD_est

#' So far using a few methods, we could confirm that there is indeed a significant difference between the two groups.
