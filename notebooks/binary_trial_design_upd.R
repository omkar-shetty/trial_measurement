#'---
#'title: "Binary Trial Design"
#'output: github_document
#'---
#'  
#'  Testing a few approaches to compare and contrast different methodologies for trials with a binary outcome (success/failure trials).


library(data.table)
library(oddsratio)
library(ggplot2)

#'Defining a config to generate a trial data


#' Defining trial parameters

trial_config <- list(
  groups = LETTERS[1:2],
  time_units = 30,
  group_a = 0.8,
  group_b = 0.9,
  n = 100
)

#'For this toy example, lets assume that a retail chain has a new process to improve on-shelf availability of products.
#'One way of testing if the new process is significantly better is by trialling two different groups - one with the new process and the control group with the regular process.
#'
#'The questions that we want to answer are :
#'  * Is the new process *significantly* better than the original ?
#'  * If yes, what is the difference between the groups ?
#'  * Is the trial enough for us to arrive at a conclusion ?
  
  
# Calculating the odds ratio based on the specified frequencies

actual_odds_ratio = ((trial_config$group_b)/(1 - trial_config$group_b))/((trial_config$group_a)/(1 - trial_config$group_a))

actual_odds_ratio


#'THe next step is to generate some simulated data and undee


## Generating Simulated Trial Data

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

#' The odds ratio here is (2722/278) / (2378/622) = 2.56 which implies a positive effect for group B (as the odds ratio > 1) 

#' Also Visualizing the split

ggplot(trial_data) +
  aes(x = GROUP, fill = as.factor(OUTCOME)) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(fill = 'Outcome')

#Using a logistic regression

fit1 <- glm(formula = 'OUTCOME ~ GROUP', family = binomial, data = trial_data)
fit1

#' Estimating the proprtion for each group

pB = exp(1.3411 + 0.9404)/(1 + exp(1.3411 + 0.9404))
pB

pA = exp(1.3411)/(1 + exp(1.3411))
pA


#' Calculating the relative difference (RD) over the study population


trial_data[,TREAT := ifelse(GROUP == 'A',0,1)]
trial_data[,pB_est := exp(coef(fit1)["(Intercept)"] + TREAT*coef(fit1)["GROUPB"])]
trial_data[,pA_est := exp(coef(fit1)["(Intercept)"])]
trial_data[,pA_diff := pB_est - pA_est]

RD_est = trial_data[,sum(pA_diff)]/trial_data[,.N]
RD_est

#' Calculating the confidence interval of the odds ratio

or_glm(data = trial_data, model = fit1)

#'Using a chi-square test to re-evaluate the difference

trial_data[,OUTCOME := as.factor(OUTCOME)]
chisq.test(table(trial_data$GROUP, trial_data$OUTCOME))


#' So far using a few methods, we could confirm that there is indeed a significant difference between the two groups.
