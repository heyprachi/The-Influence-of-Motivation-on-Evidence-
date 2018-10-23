# --------------------------------------------------------------------------------------------------
# The influence of motivation on evidence assimilation in a repeated judgment task
# --------------------------------------------------------------------------------------------------
# Authors: Prachi Solanki and Zach Horne
# Experiment: Experiment 2
# Subject pool: Mturk
# --------------------------------------------------------------------------------------------------

require(brms)
require(rstan)
require(Rcpp)
require(ggplot2)
require(scales)
require(gridExtra)
require(tidyverse)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# --------------------------------------------------------------------------------------------------
# 1. Excluding adults who indicated they weren't paying attention
# --------------------------------------------------------------------------------------------------



data1 <- data %>%
  filter(Attention == "Yes")

# --------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------


model1 <- brm(Judgment ~ Condition*Motivation + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

summary(model1)

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

model2 <- brm(Judgment ~ Condition*Memory*Motivation + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

summary(model2)

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

model3 <- brm(MemoryAccuracy ~ Condition*Motivation + (1|Subject),
              data=data1,family="bernoulli", chains=4, 
              iter=4000, warmup=2000, control = list(adapt_delta = .90), save_all_pars = TRUE,
              prior = c(set_prior("normal(0,1)", class = "b")))

summary(model3)