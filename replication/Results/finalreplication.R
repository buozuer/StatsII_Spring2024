getwd()
setwd("C:/Users/buozuer/OneDrive - Microsoft/Documents 1/Stats Replication")

df <- read.csv("deidentified_responses.csv")
View(df)
str(df)

summary(df)

#This is a replacement neededed for the intial descriptive analysis
#One of the problem with the following variables is they're not clearly defined in the 
#article or suplimentary documents
#I needed to play with the data to discover the following transformation methods


df$YDQ.1[df$YDQ.1 == 2] <- 0
df$YDQ.2[df$YDQ.2 == 2] <- 0 
df$YDQ.3[df$YDQ.3 == 2] <- 0
df$YDQ.4[df$YDQ.4 == 2] <- 0 
df$YDQ.5[df$YDQ.5 == 2] <- 0
df$YDQ.6[df$YDQ.6 == 2] <- 0 
df$YDQ.7[df$YDQ.7 == 2] <- 0
df$YDQ.8[df$YDQ.8 == 2] <- 0 


df$ICQ_Ctan <- (df$ICQ_01_t-1)+(df$ICQ_07_t-1)+(df$ICQ_13_t-1)+(df$ICQ_19_t-1)+(df$ICQ_25_t-1)
df$ICQ_Att <- (df$ICQ_02_a-1)+(df$ICQ_08_a-1)+(df$ICQ_14_a-1)+(df$ICQ_20_a-1)+(df$ICQ_26_a-1)
df$ICQ_Sens <- (df$ICQ_03_se-1)+(df$ICQ_09_se-1)+(df$ICQ_15_se-1)+(df$ICQ_21_se-1)+(df$ICQ_27_se-1)
df$ICQ_Demand <- (df$ICQ_04_d-1)+(df$ICQ_10_d-1)+(df$ICQ_16_d-1)+(df$ICQ_22_d-1)+(df$ICQ_28_d-1)
df$ICQ_SocAvoid <- (df$ICQ_05_so-1)+(df$ICQ_11_so-1)+(df$ICQ_17_so-1)+(df$ICQ_23_so-1)+(df$ICQ_29_so-1)
df$ICQ_CogAvoid <- (df$ICQ_06_c-1)+(df$ICQ_12_c-1)+(df$ICQ_18_c-1)+(df$ICQ_24_c-1)+(df$ICQ_30_c-1)
df$ICQ_TOTAL <- df$ICQ_Ctan+df$ICQ_Att+df$ICQ_Sens+df$ICQ_Demand+df$ICQ_SocAvoid+df$ICQ_CogAvoid
df$YDQ <- df$YDQ.1+df$YDQ.2+df$YDQ.3+df$YDQ.4+df$YDQ.5+df$YDQ.6+df$YDQ.7+df$YDQ.8

# Create a new attribute "YDQ_bin" based on the condition df$YDQ >= 5  
df$YDQ_bin <- ifelse(df$YDQ >= 5, 1, 0)

# Internet usage label was needed to perform Levene's test

df$IU_W2R_F <- ifelse(df$IU_W2R == 1, "Y", "N") 

# Created a new data frame with the condition IU_W2R = 1  
df1 <- df[df$IU_W2R == 1, c("IU_W2R", "ICQ_TOTAL", "ICQ_Ctan", "ICQ_Att", "ICQ_Sens", "ICQ_Demand", "ICQ_SocAvoid", "ICQ_CogAvoid", "YDQ", "YDQ_bin", "IU_W2R_F")]  

# Created a new data frame with the condition IU_W2R = 2  
df2 <- df[df$IU_W2R == 2, c("IU_W2R", "ICQ_TOTAL", "ICQ_Ctan", "ICQ_Att", "ICQ_Sens", "ICQ_Demand", "ICQ_SocAvoid", "ICQ_CogAvoid", "YDQ", "YDQ_bin", "IU_W2R_F")]  


summary(df1)
summary(df2)

# Created a new data frame for the alternative glm model 
dfglm <- df[c("IU_W2R", "ICQ_TOTAL", "ICQ_Ctan", "ICQ_Att", "ICQ_Sens", "ICQ_Demand", "ICQ_SocAvoid", "ICQ_CogAvoid", "YDQ", "YDQ_bin")]  
dfglm$IU_W2R[df$IU_W2R == 2] <- 0
summary(dfglm)


#Descriptive Statistics(ICQP_W2R_jasp_output.pdf page 1 replication)

# Calculate descriptive statistics for each variable  
statistics <- apply(df1, 2, function(x) {  
  c(Valid = sum(!is.na(x)),  
    Missing = sum(is.na(x)),  
    Mean = mean(x, na.rm = TRUE),  
    StdDev = sd(x, na.rm = TRUE),  
    Minimum = min(x, na.rm = TRUE),  
    Maximum = max(x, na.rm = TRUE))  
})  


 
statistics <- as.data.frame(statistics)  

# Descriptive Statistics in Page 1   
print(statistics)  


#Distribution Plots (ICQP_W2R_jasp_output.pdf page 2 - 5 replication)

#names of attributes in df1  
attr_names <- names(df1)  


# Generate bar charts for all attributes in df1
for (attr in attr_names) {  
  barplot(table(df1[[attr]]), main = paste("Bar Chart of", attr), xlab = attr, ylab = "Counts")  
}  

# Generate bar charts for all attributes in df2 
for (attr in attr_names) {  
  barplot(table(df2[[attr]]), main = paste("Bar Chart of", attr), xlab = attr, ylab = "Counts")  
}  


#Boxplots (ICQP_W2R_jasp_output.pdf page 6-7 replication) 


#plot against IU_W2R

variable <- "IU_W2R"  

# Scatter plots for df1 againist "IU_W2R"
for (attr in attr_names) {  
  if (attr != variable) {  
    plot(df1[[variable]], df1[[attr]] , main = paste("Scatter Plot of", variable, "vs", attr), xlab = variable, ylab = attr)  
  }  
}  


# Scatter plots for df2 againist "IU_W2R"
for (attr in attr_names) {  
  if (attr != variable) {  
    plot(df2[[variable]], df2[[attr]] , main = paste("Scatter Plot of", variable, "vs", attr), xlab = variable, ylab = attr)  
  }  
}  


#Q-Q Plots (ICQP_W2R_jasp_output.pdf page 8 replication)


# Standardized individual residuals  
residuals <- rstandard(lm(ICQ_Ctan ~ 1, data = df1))  

# Q-Q plot generation 
qqplot(qnorm(ppoints(length(residuals))), residuals, main = "Q-Q Plot of ICQ_TOTAL",  xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")  

# Fitted line  
qqline(residuals, distribution = qnorm, col = "red")  

#In the article correlation mentioned as 0.39 but actually there is a negative 
#correlation between IU_W2R and YDQ. However, ICQP_W2R_jasp_output.pdf file page 12 
#shows the correct correlations

correlation <- cor(df$IU_W2R, df$YDQ, method = "spearman")  
print(correlation) 

#Correlation Table

# Install necessary packages
install.packages("psych")  
library(psych)  

# Pearson's correlation  
pearson_cor <- cor.test(df$IU_W2R, df$YDQ, method = "pearson")  
pearson_r <- pearson_cor$estimate  
pearson_p_value <- pearson_cor$p.value  
pearson_ci <- corr.test(df$IU_W2R, df$YDQ, method = "pearson")$ci  

# Spearman's correlation  
spearman_cor <- cor.test(df$IU_W2R, df$YDQ, method = "spearman")  
spearman_rho <- spearman_cor$estimate  
spearman_p_value <- spearman_cor$p.value  
spearman_ci <- corr.test(df$IU_W2R, df$YDQ, method = "spearman")$ci  

# Print the results  
print("Pearson's correlation:")  
print(pearson_r)  
print("p-value:")  
print(pearson_p_value)  
print("Upper 95% CI:")  
print(pearson_ci[1,2])  
print("Lower 95% CI:")  
print(pearson_ci[1,1])  

print("Spearman's correlation:")  
print(spearman_rho)  
print("p-value:")  
print(spearman_p_value)  
print("Upper 95% CI:")  
print(spearman_ci[1,2])  
print("Lower 95% CI:")  
print(spearman_ci[1,1])  

# Chi-Square Hypothesis 1 (ICQP_W2R_jasp_output.pdf page 13 replication) 

# Contingency table  
contingency_table <- table(df$YDQ_bin, df$IU_W2R)  

# chi-squared test  
chi_squared <- chisq.test(contingency_table)  
 
print(chi_squared)  

# Create a contingency table  
contingency_table <- table(df$YDQ_bin, df$IU_W2R)  

# Perform the chi-squared test  
chi_squared <- chisq.test(contingency_table)  
chi_squared_corrected <- chisq.test(contingency_table, correct = TRUE)  
likelihood_ratio <- chisq.test(contingency_table, correct = FALSE, simulate.p.value = TRUE, B = 10000)  

# Print the test statistics, degrees of freedom, p-values, and estimated p-values  
print("Chi-squared Test:")  
print(chi_squared$statistic)  
print(chi_squared$parameter)  
print(chi_squared$p.value)  
print(chi_squared$expected)  

print("Chi-squared Test (with continuity correction):")  
print(chi_squared_corrected$statistic)  
print(chi_squared_corrected$parameter)  
print(chi_squared_corrected$p.value)  
print(chi_squared_corrected$expected)  

print("Likelihood Ratio Test:")  
print(likelihood_ratio$statistic)  
print(likelihood_ratio$parameter)  
print(likelihood_ratio$p.value)  
print(likelihood_ratio$estimate)  


# Calculate the probabilities  
p1 <- contingency_table[1, 1] / sum(contingency_table[1, ])  
p2 <- contingency_table[2, 1] / sum(contingency_table[2, ])  

# Calculate the log odds ratio  
log_odds_ratio <- log(p1 / (1 - p1)) - log(p2 / (1 - p2))  

# Print the log odds ratio  
print(log_odds_ratio)  


# Fisher's exact test (ICQP_W2R_jasp_output.pdf page 13 replication) 
fisher_test <- fisher.test(contingency_table)

# Print the results  
print(fisher_test) 

# Extract the odds ratio from Fisher's test result  
odds_ratio <- fisher_test$estimate  

# Calculate the log odds ratio  
log_odds_ratio <- log(odds_ratio)  

# Print the log odds ratio  
print(log_odds_ratio)  


#The following coefficient calculations don't match with the calcupations showcased in the 
#ICQP_W2R_jasp_output.pdf page 13 Nominal table

# Calculate the contingency coefficient  
contingency_coefficient <- sqrt(chisq.test(contingency_table)$statistic / (sum(contingency_table) + chisq.test(contingency_table)$statistic))  

# Calculate Phi-coefficient  
phi_coefficient <- sqrt(chisq.test(contingency_table)$statistic / sum(contingency_table))  

# Calculate Cramer's V  
cramers_v <- sqrt(chisq.test(contingency_table)$statistic / (sum(contingency_table) * min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)))  

# Print the results  
print(paste("Contingency Coefficient:", contingency_coefficient))  
print(paste("Phi-coefficient:", phi_coefficient))  
print(paste("Cramer's V:", cramers_v))  


#Independent Samples T-Test Hypothesis 2 ICQP_W2R_jasp_output.pdf page 14 replication
#in the article it is mentioned that two sided t test had been applied
#results are different due to lack of information given

# Perform two-tailed Student's t-test  
t_test <- t.test(df$ICQ_TOTAL, mu = 0, alternative = "two.sided")  

# Calculate Vovk-Sellke Maximum p-Ratio  
p_value <- t_test$p.value  
maximum_p_ratio <- 1 / (-exp(1) * p_value * log(p_value))  

# Print the results  
print(t_test)  
 


# Perform Shapiro-Wilk test of normality  (ICQP_W2R_jasp_output.pdf page 14 replication) 
shapiro_test <- shapiro.test(df1$ICQ_TOTAL)  

# Print the results  
print(shapiro_test)  

# Perform Shapiro-Wilk test of normality  (ICQP_W2R_jasp_output.pdf page 14 replication) 
shapiro_test <- shapiro.test(df2$ICQ_TOTAL)  

# Print the results  
print(shapiro_test) 





#As an alternative analysis, I wanted to generate a logistic regression model to see 
#the impact of all the other variables on wanted the reduce internet use

# Set the seed 
set.seed(123)  

# Split the data into train and test sets  - changed the split percentage multiple times
#to build the best performer glm model

train_indices <- sample(1:nrow(dfglm), nrow(dfglm) * 0.7)  
train_set <- dfglm[train_indices, ]  
test_set <- dfglm[-train_indices, ]  


# Build the logistic regression model 
model <- glm(IU_W2R ~ ICQ_TOTAL + ICQ_Ctan + ICQ_Att + ICQ_Sens + ICQ_Demand + ICQ_SocAvoid + ICQ_CogAvoid + YDQ + YDQ_bin, data = train_set, family = binomial)  

# View the model summary  
summary(model)  


# Install and load the pROC package  
install.packages("pROC")  
library(pROC)  


# Predict the probabilities using the logistic regression model on the test set  
predicted_probs <- predict(model, newdata = test_set, type = "response")  

# Create a roc object using the predicted probabilities and the actual target variable from the test set  
roc_obj <- roc(test_set$IU_W2R, predicted_probs)  

# Plot the ROC curve  
plot(roc_obj, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")  

# Calculate the AUC  
auc <- auc(roc_obj)  
print(paste("AUC:", auc))  

# Predict the class labels using the logistic regression model on the test set  and labeled them
predicted_labels <- ifelse(predict(model, newdata = test_set, type = "response") > 0.5, 1, 0)  

# Create a confusion matrix  
confusion_matrix <- table(test_set$IU_W2R, predicted_labels)  

# Print confusion matrix  
print(confusion_matrix)  
