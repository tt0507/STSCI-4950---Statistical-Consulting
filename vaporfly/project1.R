#This assignment has three tasks:

#1. Read the paper in the Vaporfly module and use the information to reproduce the table of parameter estimates.

#2. Do an additional analysis to answer the question: "Is the Vaporfly effect the same for men and women?"

#3. Explore one additional aspect of your choosing. This can be a further analysis or a checking of the 
# robustness of the results to model assumptions or outliers. For example:
  
#  * Are all model assumptions reasonable? Should any be relaxed or changed?
  
#  * Are there any additional variables that could be accounted for?
  
#  * Are there any outliers that have undue influence on the results?
  
#  * Are there any other interesting findings in this dataset?
  
#  Please turn in a report, no longer than 5 pages, including tables and figures with results from your analysis. 
#The report should contain a mix of explanatory text, tables, and figures, with some limited R code. 
#You  should not include all of your R code in the report, 
#but a few snippets can be included if they are useful for explaining your analysis. 
#Turn the report into a pdf and submit it on canvas. 
#Students have found Rmarkdown and latex useful for generating report documents.

#The report is due Monday, September 14 at noon.

# library
library(lme4)

# raw data
men = read.csv("men_sampled_shoe.csv", as.is = TRUE)
women = read.csv("women_sampled_shoe.csv", as.is = TRUE)

parameter_estimates = function(data, log_transform=FALSE){
  not_missing = !is.na(data$vaporfly) # find non-NA values
  
  vaporfly = as.numeric(data$vaporfly[not_missing]) # define vaporfly variable
  
  # runner effect
  runner_effect = as.factor(data$match_name[not_missing])
  
  # race effect
  race_effect = as.factor(paste(data$marathon, data$year)[not_missing])
  
  # dependent variable
  y = data$time_minutes[not_missing]
  
  if (log_transform == FALSE){
    fit_model = lme4::lmer(y ~ vaporfly + (1|runner_effect) + (1|race_effect), REML = TRUE)
  } else {
    fit_model = lme4::lmer(log(y) ~ vaporfly + (1|runner_effect) + (1|race_effect), REML = TRUE)
  }
  
  return (fit_model)
}

men_vaporfly = parameter_estimates(men)
women_vaporfly = parameter_estimates(women)
log_men_vaporfly = parameter_estimates(men, TRUE)
log_women_vaporfly = parameter_estimates(women, TRUE)

summary(men_vaporfly)

# confidence interval
confint_men = round( men_vaporfly@beta[2] + qnorm(0.95)*sqrt(vcov(men_vaporfly)[2,2])*c(-1,1), 3 )
confint_women = round( women_vaporfly@beta[2] + qnorm(0.95)*sqrt(vcov(women_vaporfly)[2,2])*c(-1,1), 3 )
confint_log_men = round( log_men_vaporfly@beta[2] + qnorm(0.95)*sqrt(vcov(log_men_vaporfly)[2,2])*c(-1,1), 3 )
confint_log_women = round( log_women_vaporfly@beta[2] + qnorm(0.95)*sqrt(vcov(log_women_vaporfly)[2,2])*c(-1,1), 3 )


#2. Do an additional analysis to answer the question: "Is the Vaporfly effect the same for men and women?"

library(lsmeans)

vaporfly_effect = function(data, log_transform=FALSE){
  not_missing = !is.na(data$vaporfly) # find non-NA values
  
  vaporfly = as.numeric(data$vaporfly[not_missing]) # define vaporfly variable
  
  # runner effect
  runner_effect = as.factor(data$match_name[not_missing])
  
  # race effect
  race_effect = as.factor(paste(data$marathon, data$year)[not_missing])
  
  # dependent variable
  y = data$time_minutes[not_missing]
  
  if (log_transform == FALSE){
    fit_model = lme4::lmer(y ~ vaporfly + (1|runner_effect) + (1|race_effect), REML = TRUE)
  } else {
    fit_model = lme4::lmer(log(y) ~ vaporfly + (1|runner_effect) + (1|race_effect), REML = TRUE)
  }
  
  vaporfly.fit = lsmeans(fit_model, pairwise ~ vaporfly, adjust='tukey')
  # tukey = TukeyHSD(fit_model)
  
  return (vaporfly.fit)
}

vap_effect_men = vaporfly_effect(men)
vap_effect_women = vaporfly_effect(women)


anomaly_check = function(data, log_transform=FALSE){
  not_missing = !is.na(data$vaporfly) # find non-NA values
  
  vaporfly = as.numeric(data$vaporfly[not_missing]) # define vaporfly variable
  
  # runner effect
  runner_effect = as.factor(data$match_name[not_missing])
  
  # race effect
  race_effect = as.factor(paste(data$marathon, data$year)[not_missing])
  
  # dependent variable
  y = data$time_minutes[not_missing]
  
  if (log_transform == FALSE){
    fit_model = lme4::lmer(y ~ vaporfly + (1|runner_effect) + (1|race_effect), REML = TRUE)
  } else {
    fit_model = lme4::lmer(log(y) ~ vaporfly + (1|runner_effect) + (1|race_effect), REML = TRUE)
  }
  
  return (fit_model)
}

plot(cooks.distance(anomaly_check(men)), ylab="Cook's Distance", main = "Cook's Distance Plot for Men Time")
plot(cooks.distance(anomaly_check(women)), ylab="Cook's Distance", main = "Cook's Distance Plot for Women Time")
















