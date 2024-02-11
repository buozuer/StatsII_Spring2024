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

set.seed(123)
# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))

kolmogorov_smirnov_test <- function(data) {  
  # Create empirical distribution of observed data  
  ECDF <- ecdf(data)  
  empiricalCDF <- ECDF(data)  
  
  # Generate test statistic  
  D <- max(abs(empiricalCDF - pnorm(data)))  
  
  # Calculate p-value  
  p_value <- 2 * sum(exp(-(2*(1:length(data))-1)^2 / (8*D^2)))  
  
  return(p_value)  
}  


 
cauchy_data <- rcauchy(1000, location = 0, scale = 1)
cauchy_data

 
p_value <- kolmogorov_smirnov_test(cauchy_data)  
p_value  



#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)



ols_obj_func <- function(beta, x, y) {  
  residuals <- y - beta[1] - beta[2] * x  
  sum(residuals^2)  
}  


result <- optim(c(0, 0), ols_obj_func, x = data$x, y = data$y, method = "BFGS")  

 
estimated_coeffs <- result$par  

 
lm_result <- lm(y ~ x, data = data)  
lm_coeffs <- coef(lm_result)  

estimated_coeffs  
lm_coeffs  
