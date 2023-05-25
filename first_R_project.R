# Practical 9

# Q1
states <- as.data.frame(state.x77)
str(states)
# Add the states name as a variable
states$name <- state.name

# Renaming Life Exp and HS Grad variables as 
# these will cause possible issues when referring to
# them since they contain a space.
colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

# Q3a
# Examine initial linearity between variables in the dataset
library(psych)
pairs.panels(states,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Examine linearity in more detail
scatter.smooth(x = states$Population,
               y = states$Murder,
               xlab = "Population (,000)",
               ylab = "Murder %", main = "Correlation of murder ~ population")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(states$Murder, states$Population)

scatter.smooth(x = states$Illiteracy,
               y = states$Murder,
               main = "Correlation of Murder ~ Illiteracy",
               xlab = "Illiteracy %",
               ylab = "Murder %")

# Examining correlation between murder and illiteracy
cor(states$Murder, states$Illiteracy)

# This is a better correlation value between both variables.
# Lets examine murder and frost variables for correlation.
scatter.smooth(x = states$Frost,
               y = states$Murder,
               main = "Correlation of Murder ~ Frost",
               xlab = "Frost",
               ylab = "Murder %")
cor(states$Murder, states$Frost)

# Examining the other variables
paste("Correlation for Murder and Frost: ", cor(states$Murder, states$Frost))
paste("Correlation for Murder and Illiteracy: ", cor(states$Murder, states$Illiteracy))
paste("Correlation for Murder and Population: ", cor(states$Murder, states$Population))
paste("Correlation for Murder and HS Grad: ", cor(states$Murder, states$HS_Grad))
paste("Correlation for Murder and Income: ", cor(states$Murder, states$Income))
paste("Correlation for Murder and Life Exp: ", cor(states$Murder, states$Life_Exp))
paste("Correlation for Murder and Area: ", cor(states$Murder, states$Area))

# It appears that the variable Area has a vary low correlation with Murder. 
# Therefore I am going to remove it from the dataset. 
# Alternatively we can choose to exclude these independent variables when
# we are constructing the MLR model.

# Q3b
# Check for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(states)
boxplot(Murder,
        main = "Murder",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Murder)$out)) # box plot for 'murder'
boxplot(Population,
        main = "Population",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Population)$out)) # box plot for 'Population'
boxplot(states$HS_Grad,
        main = "Graduation",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$HS_Grad)$out)) # box plot for 'HS Grad'
boxplot(Illiteracy,
        main = "Illiteracy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Illiteracy)$out)) # box plot for 'HS Grad'
boxplot(Income,
        main = "Income",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out)) # box plot for 'HS Grad'
boxplot(Frost,
        main = "Frost",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Frost)$out)) # box plot for 'HS Grad'
boxplot(states$Life_Exp,
        main = "Life Exp",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$Life_Exp)$out)) # box plot for 'HS␣Grad'
detach(states)
par(opar)

# Both the population and Income variables contain outliers.
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Population)$out # outlier values.
paste("Population outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Income)$out # outlier values.
paste("Income outliers: ", paste(outlier_values, collapse=", "))

# Remove population outliers
states <- subset(states,
                 states$Population != 21198
                 & states$Population != 11197
                 & states$Population != 18076
                 & states$Population != 11860
                 & states$Population != 12237)
# Remove Income outliers
states <- subset(states, states$Income != 6315)
# Remove Area outliers
states <- subset(states,
                 states$Area != 566432
                 & states$Population != 156361
                 & states$Population != 262134)

# Re-run the box-plots to verify that outliers have now gone.

# Q3c
# Check for normality
# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical
plot(density(states$Population),
     main = "Density plot : Population",
     ylab = "Frequency", xlab = "Population",
     sub = paste("Skewness : ", round(e1071::skewness(states$Population), 2)))
# fill the area under the plot
polygon(density(states$Population), col = "red")

plot(density(states$Murder),
     main = "Density plot : Murder",
     ylab = "Frequency", xlab = "Murder",
     sub = paste("Skewness : ", round(e1071::skewness(states$Murder), 2)))
polygon(density(states$Murder), col = "red")

plot(density(states$HS_Grad),
     main = "Density plot : HS grade",
     ylab = "Frequency", xlab = "HS grade",
     sub = paste("Skewness : ", round(e1071::skewness(states$HS_Grad), 2)))
# fill the area under the plot
polygon(density(states$HS_Grad), col = "red")
plot(density(states$Illiteracy),
     main = "Density plot : Illiteracy",
     ylab = "Frequency", xlab = "Illiteracy",
     sub = paste("Skewness : ", round(e1071::skewness(states$Illiteracy), 2)))
polygon(density(states$Illiteracy), col = "red")
plot(density(states$Income),
     main = "Density plot : Income",
     ylab = "Frequency", xlab = "Income",
     sub = paste("Skewness : ", round(e1071::skewness(states$Income), 2)))
# fill the area under the plot
polygon(density(states$Income), col = "red")
plot(density(states$Frost),
     main = "Density plot : Frost",
     ylab = "Frequency", xlab = "Feost",
     sub = paste("Skewness : ", round(e1071::skewness(states$Frost), 2)))
# fill the area under the plot
polygon(density(states$Frost), col = "red")
par(opar)

# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0-5 = approx symetric.
# Illiteracy = 0.87 = moderatly skewed
# Population = 1.15 = highly skewed
# all others seem okay
paste("Skewness for illiteracy : ", round(e1071::skewness(states$Illiteracy), 2))
paste("Skewness for population : ", round(e1071::skewness(states$Population), 2))
paste("Skewness for murder : ", round(e1071::skewness(states$Murder), 2))
paste("Skewness for HS grad : ", round(e1071::skewness(states$HS_Grad), 2))
paste("Skewness for income : ", round(e1071::skewness(states$Income), 2))
paste("Skewness for frost : ", round(e1071::skewness(states$Frost), 2))

# It seems that population must be converted
# Data is visually skewed to the right
hist(states$Population)

# p-value indices that the data is not normally distributed
shapiro.test(states$Population)

# Check normality of the other variables
shapiro.test(states$Illiteracy)
shapiro.test(states$Murder)
shapiro.test(states$HS_Grad)
shapiro.test(states$Income)
shapiro.test(states$Frost)

# If p-value < 0.05 then variable
# is not normally distributed

# Illiteracy is not normally distributed (p-value = 8.297e-05)
# Murder is normally distributed (p-value = 0.06601)
# HS_grad is not normally distributed (p-value = 0.02194)
# Income is normally distributed (p-value = 0.3246)
# Frost is normally distributed (p-value = 0.0928)

# Need to transform illiteracy, HS_grad and populaiton

library(MASS)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(states$Population ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_population = (states$Population ^ lambda - 1)/lambda
hist(transformed_population)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_population)

# Convert HS_grad
Box = boxcox(states$HS_Grad ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_HS_grad = (states$HS_Grad ^ lambda - 1)/lambda
hist(transformed_HS_grad)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_HS_grad)

# Convert illiteracy
# Data is not normally distributed

# Transform illiteracy as a single vector
# Try values -6 to 6 by 0.1
Box = boxcox(states$Illiteracy ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_illiteracy = (states$Illiteracy ^ lambda - 1)/lambda
hist(transformed_illiteracy)
shapiro.test(transformed_illiteracy)

# Illiteracy is still not normally distributed

# Tukey’s Ladder of Powers transformation
# also indicates that the best value for 
# lambda is -0.6 for illiteracy variable
library(rcompanion)

#transform_tukey_illiteracy = transformTukey(states$Illiteracy, plotit=FALSE)
# -0.6 indaictes that "-1 * x ^ lambda" is required
#transformed_illiteracy <- -1 * states$Illiteracy ^ -0.625
#shapiro.test(transformed_illiteracy)

# Converting data in data frame
states$transformed_Population <- transformed_population
states$transformed_HS_Grad <- transformed_HS_grad
states$transformed_Illiteracy <- transformed_illiteracy

# Practical 10 ---------------------------------------------------------------


attach(states)

# Split the data into training and testing
set.seed(1)
no_rows_data <- nrow(states)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- states[sample, ]
testing_data <- states[-sample, ]

unmodified_model <- lm(Murder ~ Population + Income + Illiteracy + Life_Exp + HS_Grad + Frost, data=training_data)
summary(unmodified_model)

# Examine which combination of independent variables best fits the model
# install.packages("leaps")
library(leaps)
# See https://cran.r-project.org/web/packages/leaps/leaps.pdf
# Regression Subset Selection
MLR_subset_selection <-regsubsets(Murder ~ Population + Income + Illiteracy + Life_Exp + HS_Grad + Frost, data=training_data, nbest=6)
plot(MLR_subset_selection, scale="adjr2")

# I want to use the model to examine whether population, illiteracy
# and high school grade has an impact on murder rate
# There is an adjusted r2 model at 0.7 which does not use income 
# as an independent variable

# Smallest AIC is best
# Seems that Murder ~ Population + illiteracy + Life_Exp + HS_grad 
# is best. I'm dropping frost as it has probably
# a spurious correlation
stepAIC(unmodified_model, direction="backward")

modified_model <- lm(Murder ~ transformed_Population 
                     + Income + transformed_Illiteracy 
                     + Life_Exp + transformed_HS_Grad, 
                     data = training_data)
summary(modified_model)

# We can examine best fitting model using leaps
MLR_subset_selection_modified <-regsubsets(Murder ~ transformed_Population 
                                           + Income + transformed_Illiteracy 
                                           + Life_Exp + transformed_HS_Grad,
                                           data = training_data, nbest=6)
plot(MLR_subset_selection_modified, scale="adjr2")

# It seems that the untransformed model
# could be more accurate than the transformed model 


# Confidence interval shows us that 
# eg population that we can be 95% certain that 
# of murder rate if income is between -0.002 and 1.94 
confint(unmodified_model)

confint(modified_model)

# Examine outliers for the unmodified model
install.packages("car")
library(car)
qqPlot(unmodified_model, 
       labels=row.names(training_data$name), 
       id.method="identify", 
       simulate=TRUE, 
       main="Q-Q Plot for unmodified model")

# Nevada and Maine are identidief as outliers
# Lets examine why

training_data["Nevada",]
training_data["Maine",]

# The fitted() function extracts fitted values 
# from objects returned by modeling functions. It
# returns the predicted murder rate for a particular state.
fitted(unmodified_model)["Nevada"]
fitted(unmodified_model)["Maine"]

# These results tell us that the murder rate in Nevada
# is 11.5%, but the model predicts 8.25%
# murder rate. and for Maine, the murder rate 
# is 2.7% and the model predicts 6.15%.

# We can view these errors using a histogram. This code 
# generates a histogram of the studentized
# residuals and superimposes a normal curve, 
# kernel-density curve, and rug plot

# the errors follow a normal distribution quite well, 
# with the exception of 1 outlier
# on the right of the chart (outside the +2 boundary). 
# The Q-Q plot is probably more informative
# in terms of displaying outliers.
# A rough rule is that standardized residuals 
# that are larger than 2 or less than –2 are worth attention
studentized_fit <- rstudent(unmodified_model)
hist(studentized_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

# Rug plot is used to show the distribution of the data
# Here it is showing the distribution
# of the x-axis data. The term "rug plot" originates
# from the effect that perpendicular markers look like tassles
# on the edge of a rug
rug(jitter(studentized_fit), col="brown")
curve(dnorm(x, mean=mean(studentized_fit), sd=sd(studentized_fit)), add=TRUE, col="blue", lwd=2)
lines(density(studentized_fit)$x, density(studentized_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

# car package also provides a statistical test for outliers. 
# The outlierTest() function reports
# the Bonferroni adjusted p-value for the largest absolute studentized residual
# Only 1 outier identified at a time
outlierTest(unmodified_model)

# As shown earlier, Nevada is an outlier for this test as well as the 
# qq-plot

crplots(unmodified_model)

training_data <- subset(training data, training_data$name != "nevada")

modified_model <- lm(Murder ~ transformed_Population 
                     + Income + transformed_Illiteracy 
                     + Life_Exp + transformed_HS_Grad, 
                     data = training_data)
summary(modified_model)

cutoff <- 4/
  (nrow(training_data)-length(unmodified_model$coefficients)-1)
plot(unmodified_model, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col ="red")

influencePlot(unmodified_model,
              main = "influence plot for unmodified data")
ncvTest(unmodified_model)

spreadLevelPlot(unmodified_model)

sqrt_murder <- sqrt(training_data$Murder)
modified_model <- lm(Murder ~ transformed_Population 
                     + Income + transformed_Illiteracy 
                     + Life_Exp + transformed_HS_Grad, 
                     data = training_data)
summary(modified_model)

#global validTION of the model
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(unmodified_model)
summary(gvmodel)

predicted_murder <- predict(unmodified_model, training_data)
actual_prediction <-data.frame(cbind(actuals = testing_data$Murder,
                                     predicted = predicted_murder))

head(actual_prediction)

cor_accuracy <- cor(actual_prediction)
cor_accuracy

summary(states)
# what i smurder rate
# where population = 1000, income = 4000, illiteracy = 5%, life expectanccy = 70, high school grade = 37%

question <- data.frame(Population = c(1000),
                       Income = c(4000),
                       Illiteracy = c(0.5),
                       Life_Exp = c(70),
                       HS_Grad = c(37))
predicted_murder <- predict(unmodified_model, question)
predicted_murder


# Practical 12
# Data is a simulated dataset containing information 
# on ten thousand customers. The aim is to predict
# which customers will default on their credit card debt

# load dataset
# install.packages("ISLR")
install.packages("ISLR")
library(ISLR)
student_data <- ISLR::Default
write.csv(student_data, "Student data.csv")

# view summary of dataset & total observations
summary(student_data)
nrow(student_data)

# Creating training and testing data
# make this example reproducible
set.seed(1)
no_rows_data <- nrow(student_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
# 70% training, 30% testing
training_data <- student_data[sample, ]
testing_data <- student_data[-sample, ]


# Fit the logistic model
# Note we use glm instead of lm for logistic regression
attach(student_data)
log_model <- glm(default ~ student 
                 + balance 
                 + income, 
                 family="binomial", 
                 data=training_data)

# disable scientific notation for model summary
# to show full extent of powered values
options(scipen=999)

# view model summary
# one unit increase in balance is associated with 
# an average increase of 0.005988 in the log odds of defaulting.
# Income not as important as student status as imortant predictors
# of defaulting
summary(log_model)

# calculate McFadden's R-Squared
# from the pscl library
# Value 0.46 = fairly high and model 
# fits the data well
#install.packages("pscl")
library(pscl)
pscl::pR2(log_model)["McFadden"]


#calculate variable importance
#install.packages("caret")
# Higher values indicate more importance. 
# These results match up nicely with the p-values 
# from the model. Balance is by far the most important 
# predictor variable, followed by student status and then income.
library(caret)
varImp(log_model)

#calculate VIF values
# of each variable in the model to see 
# if multicollinearity is a problem:
#  VIF values above 5 indicate severe multicollinearity. 
# Since none of the  predictor variables in our models have a 
# VIF over 5, we can assume that multicollinearity 
# is not an issue in our model.
library(car)
vif(log_model)




# We will examine the optimal cutoff for the model later on.

# Model diagnostics
# need to install devtools so that
# informationValue can be installed from github
#install.packages('devtools')
devtools::install_github("selva86/InformationValue")

library(InformationValue)
# Evaluate optimum cutoff for dependent variable
# convert dependent variable first
# defaults information from "Yes" and "No" to 1's and 0's
testing_data$will_default <- ifelse(testing_data$default=="Yes", 1, 0)

#calculate probability of default for each individual in test dataset
# to use for optimal calculation
predicted_sample <- predict(log_model, testing_data, type="response")

#find optimal cutoff probability to use to maximise accuracy
# the optimal probability cutoff to use is 0.3295383. 
# Thus, a person with a probability of defaulting of 0.3295383
# or higher will be predicted to default, while anyone
# with a probability less than this number will 
# be predicted to not default.
optimal <- optimalCutoff(testing_data$will_default, predicted_sample)[1]
optimal

# Add a new column into the dataframe
# so that it reflects the optimum cutoff

actuals_predictions <- data.frame(cbind(actuals = testing_data$will_default,
                                        predicted = predicted_sample,
                                        optimum =ifelse(predicted_sample >= optimal, 1, 0)))
head(actuals_predictions, 20)


# define two individuals for initial discussion
# The probability of an individual with a balance 
# of $1,400, an income of $2,000, and a student status of “Yes” 
# has a probability of defaulting of .0273. Conversely, 
# an individual with the same balance and income but 
# with a student status of “No” has a probability of 
# defaulting of 0.0439. 
predicted_data <- data.frame(balance = 1400, income = 2000, student = c("Yes", "No"))
predicted <- predict(log_model, predicted_data, type="response")
predicted

# An individual with a balance of $5000 and and income of 2000
# and is not a student is 99% probability of defaulting
predicted_data <- data.frame(balance = c(1400, 5000), income = 2000, student = c("Yes", "No"))
predicted <- predict(log_model, predicted_data, type="response")
predicted

# This model will have 94% accuracy
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

#create confusion matrix
# to show test data predictions
# versus model predictions
confusionMatrix(testing_data$will_default, actuals_predictions$optimum)

#calculate sensitivity
# true positive rate
sensitivity(testing_data$will_default, actuals_predictions$optimum)

#calculate specificity
specificity(testing_data$default, actuals_predictions$optimum)

#calculate total misclassification error rate
# This is the total misclassification
# error rate for the model.
# Error rate = 2.5%. Lower error rate
# means the better the model can
# predict outcomes
misClassError(testing_data$will_default, actuals_predictions$optimum, threshold=optimal)

#plot the ROC curve
plotROC(testing_data$will_default, actuals_predictions$optimum)
