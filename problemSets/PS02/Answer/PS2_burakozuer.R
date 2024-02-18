#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))


library(tidyverse)  


# View the data structure  
str(climateSupport)

climateSupport

 # Fit the logistic regression model  
model <- glm(choice ~ countries + sanctions, family = binomial(link="logit"), data = climateSupport)  

# summary output  
summary(model)  

#fitted logistic regression model

logit(p) = -0.005665 + 0.458452 * countries.L - 0.009950 * countries.Q - 0.276332 * sanctions.L - 0.181086 * sanctions.Q + 0.150207 * sanctions.C  

#H0: there is no association between the number of participating countries or the level of sanctions and the likelihood of an individual supporting the policy.
#The p-value for countries.L is < 2e-16. This vert small pvalue suggests strong proof against the null hypothesis and shows that the number of participating countries (countries.L) has a significant association with the likelihood of supporting the policy.


#Question2

#A: We can check to the coefficient estimate for sanctions.L in the regression model
#sanctions.L -0.276332   0.043925  -6.291 3.15e-10 ***  
#The coefficient estimate for sanctions.L is -0.276332. This means that for a one-unit increase in the level of sanctions (from 5% to 15%), the log-odds of supporting the policy decrease by 0.276332, keeping other variables constant.

#odds ratio
odds_ratio = exp(-0.276332) 
odds_ratio
1-odds_ratio

#The odds ratio of 0.7583312 indicates that increasing sanctions from 5% to 15% reduces the odds of an individual supporting the policy by approximately 24.17%


#B


# Coefficients  
intercept <- -0.005665  
beta <- 0.458452  


# Calculate estimated probability  
probability <- 1 / (1 + exp(-(-0.005665 + 0.458452)))  

 
probability  

#Therefore, the estimated probability that an individual will support the policy when there are 80 out of 192 countries participating with no sanctions is approximately 0.6 This means that the probability of support is around 60%.
