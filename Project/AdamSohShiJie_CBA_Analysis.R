# =============================================================================
# Purpose:      BC2406 Computer-Based Assignment 
# Author:       Adam Soh Shi Jie (U2320112A)
# Topics:       Data Analysis on Hospital Length of Stay Estimation Dataset
# Data Source:  INF002v4.csv
# Packages:     data.table, scales, ggplot2, dplyr, rpart, rpart.plot, corrplot, moments, car, caTools
# =============================================================================

# package installation
install.packages("data.table")
install.packages("scales")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("corrplot")
install.packages("moments")
install.packages("car")
install.packages("caTools")

# implement library
library(data.table)
library(scales)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(corrplot)
library(stringr)
library(moments)
library(car)
library(caTools)

# setting work directory
setwd('/Users/AdamSoh/Downloads/AY24 CBA')

# import csv file
health_data = fread("INF002v4.csv", stringsAsFactors=T)
summary(health_data)

#==============================================================================


# Convert to Categorical Variable
health_data[ ,Discharge.Year := as.factor(Discharge.Year)]
health_data[ ,APR.Severity.of.Illness.Code := as.factor(APR.Severity.of.Illness.Code)]


#==============================================================================
#==================== QUESTION 1 ==============================================
#==============================================================================

# 3 notable findings

# 1) Investigate the distribution of Profit (Total.Charges - Total.Costs) in different Hospital Service Area using box plot
# Replace "," with "" to enable calculation of numbers.
health_data[, Total.Charges := as.numeric(gsub(",", "", Total.Charges))]
health_data[, Total.Costs := as.numeric(gsub(",", "", Total.Costs))]
# We can create a "Profit" column by Total.Charges - Total.Costs
health_data[ ,Profit := Total.Charges - Total.Costs]
# Then, we create the boxplot
ggplot(health_data, aes(x = Hospital.Service.Area, y = Profit)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Box Plot of Profit among Hospital Service Area", 
       x = "Hospital.Service.Area", 
       y = "Profit") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
# We zoom into 'box-and-whisker' to analyse the overall profit differences among different service areas by hiding outliers
ggplot(health_data, aes(x = Hospital.Service.Area, y = Profit)) +
  geom_boxplot( outlier.shape = NA) +
  coord_cartesian(ylim = quantile(health_data$Profit, c(0.0, 0.95))) +
  labs(title = "Box Plot of Profit among Hospital Service Area", 
       x = "Hospital.Service.Area", 
       y = "Profit") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 




# 2) Investigate the relationship among LOS, Total Charges and Total Costs
# Calculate correlation matrix
# we use "complete.obs" to compute rows with complete data for all 3 variables only.
cor_matrix <- cor(health_data[, .(Length.of.Stay, Total.Charges, Total.Costs)], use = "complete.obs")
print(cor_matrix)
# draw out scatter plot to have a visualisation how LOS and Total.Charges are related.
ggplot(health_data, aes(x = Length.of.Stay, y = Total.Charges)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Total Charges against Length of Stay (LoS)")
# draw out scatter plot to have a visualisation how LOS and Total.Costs are related.
ggplot(health_data, aes(x = Length.of.Stay, y = Total.Costs)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Total Costs against Length of Stay (LoS)")



# 3) observe the count of different types of admissions across different hospital service area
admission_summary <- health_data[, .N, by = .(Hospital.Service.Area, Type.of.Admission)]
admission_summary
#display in a bar chart for easier observation
ggplot(admission_summary, aes(x = Hospital.Service.Area, y = N, fill = Type.of.Admission)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = N),
            position = position_dodge(width = 0.9),  
            vjust = -0.5) +
  labs(title = "Bar Chart of different Type of Admission according to  Hospital Service Area", 
       x = "Area", 
       y = "Number of admissions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 




#==================== EXTRA FINDINGS ==========================================

# 4) examine the distribution of length of stay across different type of admission (box plot)
ggplot(health_data, aes(x = Type.of.Admission, y = Length.of.Stay)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Box Plot of Length of Stay by Type of Admission", 
       x = "Type.of.Admission", 
       y = "Length of Stay") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 




# 5) examine the distribution of length of stay across different APR.Severity.of.Illness.Description
# Relevel 'APR.Severity.of.Illness.Description' according to the level below:
health_data[, APR.Severity.of.Illness.Description := factor(APR.Severity.of.Illness.Description, levels = c("Minor", "Moderate", "Major", "Extreme"))]
# plot the boxplot
ggplot(health_data, aes(x = APR.Severity.of.Illness.Description, y = Length.of.Stay)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Box Plot of Length of Stay by APR Severity of Illness", 
       x = "APR.Severity.of.Illness", 
       y = "Length of Stay") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
# we zoom into the 'box-and-whisker' only by hiding outliers to analyse the differences in length of stay
ggplot(health_data, aes(x = APR.Severity.of.Illness.Description, y = Length.of.Stay)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = quantile(health_data$Length.of.Stay, c(0.05, 0.95))) +
  labs(title = "Box Plot of Length of Stay by APR Severity of Illness", 
       x = "APR.Severity.of.Illness", 
       y = "Length of Stay") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 




# 6) examine the distribution of length of stay between Emergency Department Indicator
ggplot(health_data, aes(x = Emergency.Department.Indicator, y = Length.of.Stay)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = quantile(health_data$Length.of.Stay, c(0.05, 0.95))) +
  labs(title = "Box Plot of Length of Stay by Emergency Department Indicator", 
       x = "Emergency.Department.Indicator", 
       y = "Length of Stay")



#==============================================================================
#========================= QUESTION 2 =========================================
#==============================================================================

# list of potential predictor X variables and final dimension of dataset.

# create subset_health_data to observe the changes without modifying original data
subset_health_data <- copy(health_data)

# we notice there's -ve values exist in 'Profit' column, and we deem it as invalid entries.
# since it occupies a very small part of dataset (i.e. 328), we decided to remove them.
count <- subset_health_data[,subset_health_data[,Profit]<0]
sum(count)

subset_health_data<- subset_health_data[!count]
summary(subset_health_data)

# removing  'U' in Gender (invalid entry)
subset_health_data<- subset_health_data[Gender != 'U']

# removing NA in APR.Severity.of.Illness.Description (invalid entry)
subset_health_data<- subset_health_data[APR.Severity.of.Illness.Description != 'NA']

# drop any unnecessary column (especially the same entry for all data such as Discharge.Year)
# Drop multiple columns by name
# "Discharge.Year", "CCSR.Diagnosis.Code", "CCSR.Diagnosis.Description" is 1-level factor
# 'Payment Topology' has nothing to do with Length of Stay
# "Patient.Disposition" & "APR.DRG.Description" is description/statement which has no meaning / related to Length of Stay
# "APR.DRG.Code" mostly are 720 which has no meaning to Length of Stay as well.
# "APR.Severity.of.Illness.Code"  == "APR.Severity.of.Illness.Description"
subset_health_data[, c("Discharge.Year", "CCSR.Diagnosis.Code", "CCSR.Diagnosis.Description",  "APR.DRG.Description", "Patient.Disposition", "APR.DRG.Code", "APR.Severity.of.Illness.Code", "Payment.Typology.1", "Payment.Typology.2", "Payment.Typology.3") := NULL]

# "Profit" is strongly correlated with "Total.Charges" since it is derieved from it
# we can drop "Profit" to prevent multi-collinearity
cor(subset_health_data[, c("Profit", "Total.Charges", "Total.Costs")], use = "complete.obs")
subset_health_data[, c("Profit") := NULL]

# we use test for our subsequent analysis.
test <- copy(subset_health_data)
summary(test)

# List of X predictor variables:
# Hospital.Service.Area
# Age.Group
# Gender
# Race
# Ethnicity
# Length.of.Stay
# Type.of.Admission
# APR.Severity.of.Illness.Description
# APR.Risk.of.Mortality
# APR.Medical.Surgical.Description
# Emergency.Department.Indicator
# Total.Charges
# Total.Costs

# Final dimension of dataset: 27777 rows x 13 columns



#==============================================================================
#===================== QUESTION 3 =============================================
#==============================================================================

# Linear Regression & CART

###### Linear Regression ######

# IMPORTANT!!!
# we could not remove outliers 
# Reasoning: remove outliers could eliminate the potential valuable insights such as unusally expensive charges (Total.Charges) lead to prolonged hospital stay (Length.of.Stay)
# Hence, we can do log transformation instead to reduce the impact of extreme outliers.
# First, we check the skewness of variables if they are right-skewed (positive skewness)

skewness(test[,Length.of.Stay])  # skewness = 4.026
skewness(test[,Total.Charges])   # skewness = 9.604
skewness(test[,Total.Costs])   # skewness = 13.971
# all three numerical columns are right-skewed, therefore we can perform log transformation.

# First, we perform our initial linear regression without modification to observe the analysis.
m1 <- lm(log(Length.of.Stay) ~ . ,data = test)
summary(m1)
vif(m1)
# We found out that:
# "Hospital.Service.Area"
# "Age.Group"
# "APR.Severity.of.Illness.Description"
# "APR.Medical.Surgical.Description"
# "Total.Charges" 
# "Total.Costs" 
# are statistically significant to our linear regression model.

# However, we haven't perform log transformation on "Total.Charges" & "Total.Costs" which outliers can significantly affect our result.
# according to GVIF: 
# "APR.Severity.of.Illness.Description" & "APR.Risk.of.Mortality"  > 5
# "Total.Charges" & "Total.Costs" > 5
# Both cases show strong multicollinearity
# That explains why our adjusted R-squared is low (i.e. 0.4183)

# Considering all conditions above, we construct a new linear regression model.
m1 <-  lm(log(Length.of.Stay) ~ . + log(Total.Costs) - Total.Costs - Total.Charges - Type.of.Admission - APR.Risk.of.Mortality - Race - Ethnicity - Emergency.Department.Indicator - Gender, data = test)
m1 <- step(m1)
summary(m1)
vif(m1)
par(mfrow = c(2,2)) 
plot(m1)
par(mfrow = c(1,1)) 

# set seed to replicate the result
set.seed(1000)

# split into 70% trainset and 30% testset
train <- sample.split(Y = test$Length.of.Stay, SplitRatio = 0.7)
trainset <- subset(test, train == T)
testset <- subset(test, train == F)

m1.train <-  lm(log(Length.of.Stay) ~ . + log(Total.Costs) - Total.Costs - Total.Charges - Type.of.Admission - APR.Risk.of.Mortality - Race - Ethnicity - Emergency.Department.Indicator - Gender, data = trainset)
m1.train <- step(m1.train)
summary(m1.train)

RMSE.m1.train <- sqrt(mean(residuals(m1.train)^2))  # RMSE on trainset based on m1_train model
RMSE.m1.train
exp(RMSE.m1.train)

summary(abs(residuals(m1.train))) 


predict.m1.test <- predict(m1.train, newdata = testset)
testset.error <- testset$Length.of.Stay - exp(predict.m1.test)
RMSE.m1.test <- sqrt(mean(testset.error^2))
RMSE.m1.test

summary(abs(testset.error))


###### Classification and Regression Tree (CART) ######

# set seed to replicate result
set.seed(1000)

# Continuous Y: Set method = 'anova'
# Split into 70% trainset and 30% testset
train <- sample.split(Y = test$Length.of.Stay, SplitRatio = 0.7)
trainset <- subset(test, train == T)
testset <- subset(test, train == F)

# Construct CART model by considering all X predictor variables.
# Reasoning: CART will select the best variable (that produces largest reduction in variance) recursively at each step and split accordingly.
# We also set the max depth to 5 to prevent tree grow too deep and overfitting.
cart1 <- rpart(Length.of.Stay ~ . ,data = trainset, method = 'anova', control = rpart.control(minsplit = 2,cp = 0, maxdepth = 5))

# To further prevent overfitting and unnecessary split, we can prune the tree.
rpart.plot(cart1)
printcp(cart1)
plotcp(cart1)

# Automation to find the optimal cp.                              
# Compute min CV error + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
cp.opt  # cp_opt = 0.0018

# Prune the tree using cp_opt (0.0018)
cart2 <- prune(cart1, cp = cp.opt)
rpart.plot(cart2)
printcp(cart2, digits = 3)
plotcp(cart2)

# predict the Length of Stay (LoS) using the pruned CART model
predictions <- predict(cart2, newdata = testset)

# Evaluate the model by calculating RMSE
residuals <- testset$Length.of.Stay - predictions
mse <- mean(residuals^2)  # Mean Squared Error
rmse <- sqrt(mse)  # Root Mean Squared Error

# Print RMSE
print(paste("RMSE:", rmse))




#==============================================================================
#===================== RESOURCES FOR OWN USE ==================================
#==============================================================================

# function to remove outliers
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  return(x >= lower_bound & x <= upper_bound)
}

#function to count outliers
count_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- x[x < lower_bound | x > upper_bound] # there is outlier if outside the boundary
  return(length(outliers))
}

# Check the summary for any variable (if interested)
summary(health_data[,Hospital.Service.Area])

# Use boxplot check the distribution of data for any variable (if interested)
# Mainly to check the outliers' situation
boxplot(test[,Length.of.Stay], main = "Boxplot for (Any Variable)",  
        ylab = "value", col = "lightblue")

# Count outliers
outliers <- count_outliers(health_data[, Length.of.Stay])
outliers

# Remove outliers
subset_health_data[remove_outliers(subset_health_data[,Length.of.Stay]), ]


# ======================== END OF PROJECT =====================================
