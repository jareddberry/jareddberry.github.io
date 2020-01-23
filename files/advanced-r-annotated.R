# Set working directory and libraries
setwd('~/data_science/teaching/mief_r/advanced_r/data')
# install.packages(c('ISLR', 'MASS', 'glmnet', 'tree', 'rpart', 'rpart.plot', 'randomForest', 'boot', 'viridisLite', 'ggplot2', 'dplyr'))

library(dplyr)
library(ISLR) # Toy data sets
library(MASS) # Toy data sets
library(boot) # For cv.glm
library(glmnet) # Lasso/Ridge models
library(tree) # Decision tree models
library(rpart) # Decision tree models
library(rpart.plot) # Visualizing decision tree models
library(randomForest) # RandomForest models

# Helpers ---------------------------------------------------------------------

# Compute RMSE to evaluate the performance of regressions out-of-sample
rmse <- function(predicted, observed) {
  return(sqrt(sum((predicted - observed)^2)/length(observed)))
}

# Compute individual predictions of logistic regression models
invlogit <- function(x) {
  return(exp(x)/(1+exp(x)))
}

# Regression ------------------------------------------------------------------

# Datasets
?Boston
salaries_data <- read.csv('Salaries.csv', stringsAsFactors = F)
wages <- haven::read_dta('WAGE1.dta')

# Simple linear regression with the `Boston` dataset
glimpse(Boston)
?Boston

plot(Boston$medv, Boston$lstat)
fit1 <- lm(medv~lstat,data=Boston)
fit1
summary(fit1)

# Best fit lines with `abline`
abline(fit1,col="red")

# Inspecting `fit1`
names(fit1)
confint(fit1)

# Making predictions
predict(fit1, type='response')
predict(fit1, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(fit1, Boston[1:3, 'lstat', drop=F], interval="confidence")
predict(fit1, type='response')[1:3]

# Evaluating fit with our `RMSE` helper function!
preds1 <- predict(fit1, type='response')
obs <- Boston$medv
rmse(predicted = preds1, observed = obs) # Tough to evaluate the quality of this in a vacuum

# Multiple linear regression
fit2 <- lm(medv~lstat + age, data=Boston) # Separate independent variables with `+`
summary(fit2)

# Running everything
fit_all <- lm(medv ~ ., data=Boston)
summary(fit_all)

# Higher-order terms
fit3 <- lm(medv~I(lstat^2), data=Boston)
summary(fit3)

preds3 <- predict(fit3, type='response')
rmse(predicted = preds3, observed = obs)

fit4 <- lm(medv~poly(lstat,3), data=Boston)
summary(fit4)

preds4 <- predict(fit4, type='response')
rmse(predicted = preds4, observed = obs)

# Working with categoricals ---------------------------------------------------
glimpse(wages)
table(wages$female) # Standard dummy variable

fit5 <- lm(wage ~ exper, data=wages)
summary(fit5)

fit6 <- lm(wage ~ exper + female, data=wages)
summary(fit6)


glimpse(salaries_data)

fit7 <- lm(salary ~ yrs.service, data=salaries_data)
summary(fit7)

fit8 <- lm(salary ~ yrs.service + sex, data=salaries_data) # Including a character variable
summary(fit8)

fit9 <- lm(salary ~ yrs.service + sex + rank, data=salaries_data)
summary(fit9)

# `lm` does its best- don't count on this to work every time; alternatives include `model.matrix`, `factor`, and `ifelse`
salaries_data$rank[1:10]
factor(salaries_data$rank[1:10])

salaries_data$rank_fct <- factor(salaries_data$rank)
contrasts(salaries_data$rank_fct) # Shows you how these present when fit

fit10 <- lm(salary ~ yrs.service + rank_fct, data=salaries_data)
summary(fit10) # R is actually coercing to factors in the background

# Safer, but tedious
table(salaries_data$sex)
gender_dummy <- ifelse(salaries_data$sex == 'Female', 1, 0)
table(gender_dummy)

fit11 <- lm(salary ~ yrs.service + gender_dummy, data=salaries_data)
summary(fit11)

# Safer, less tedious - called one-hot encoding in the field
rank_dummies <- model.matrix(~., salaries_data['rank'])
salaries_data <- cbind(salaries_data, rank_dummies[,2:3])

# Lastly, interaction terms
fit12 <- lm(salary ~ yrs.service*sex, data=salaries_data)
summary(fit12) # Places the underlying components in the regression for you! Running a DiD is as simple as this


# Classification --------------------------------------------------------------

# Datasets
?Default
?Smarket

# Replicating some of the work in the slides
glimpse(Default)
pairs(Default, col=Default$default)

Default$default_bin <- as.numeric(Default$default) - 1
plot(Default$balance, Default$default_bin, pch = 3)

# Linear probability models
fit1 <- lm(default_bin ~ balance, data=Default)
summary(fit1)

abline(fit1,col="red")

predicted_probs <- predict(fit1, type='response')
summary(predicted_probs) # Negative probabilities don't really make sense
hist(predicted_probs)

# Logistic (logit) regression
?glm
fit2 <- glm(default ~ balance, data=Default, family = 'binomial')
summary(fit2)

# To generate individual predictions - matrix multiplication
log_odds <- c(1,1000) %*% fit2$coefficients
invlogit(log_odds) # Mapping to an actual predicted probability, no margins required

bals <- seq(500,3000,100)
probs_by_bal <- sapply(bals, function(x) invlogit(c(1,x) %*% fit2$coefficients))
plot(bals, probs_by_bal, type='l', xlab = 'Balance', ylab = 'Probability of Default')
abline(h=0.5, col='red', lty=2)

# Predicted probabilities for all of our observations
predicted_probs <- predict(fit2, type='response')
summary(predicted_probs)
hist(predicted_probs)

# Inspect Smarket data
glimpse(Smarket)
summary(Smarket)
?Smarket

pairs(Smarket,col=Smarket$Direction)

# Logistic regression
fit3 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(fit3)
coef(fit3)
summary(fit3)$coef

# Making predictions about movements in the S&P 500
market_probs <- predict(fit3,type="response")
market_probs[1:10]
summary(market_probs)
hist(market_probs)

# Confusion matrix
market_predictions <- ifelse(market_probs > 0.5, "Up" , "Down")
table(market_predictions, Smarket$Direction)

# Accuracy metrics
in_acc <- mean(market_predictions == Smarket$Direction) # Turns out, making predictions about the stock market is really hard...
in_acc

# Validation sets -------------------------------------------------------------

# In order to ensure our sampling is replicable, set seed
set.seed(1)

# The `sample` function
?sample

# Randomly choose a portion of indices for validation
glimpse(wages)
dim(wages)

train <- sample(nrow(wages), 0.7*nrow(wages))

fit1 <- lm(lwage ~ educ + exper + female + nonwhite + married + tenure + tenursq, data=wages)
summary(fit1)

# Checking our in-sample fit
in_sample_preds <- predict(fit1, type='response')
in_sample_rmse <- rmse(predicted = in_sample_preds, observed = wages$lwage)

# Using a validation set (proper evaluation of fit)
fit2 <- lm(lwage ~ educ + exper + female + nonwhite + married + tenure + tenursq, data=wages, subset = train) # Specifying a subset
summary(fit2)

fit3 <- lm(lwage ~ educ + exper + female + nonwhite + married + tenure + tenursq, data=wages[train,])
summary(fit3)

identical(fit2$coefficients, fit3$coefficients)

out_sample_preds <- predict(fit3, newdata = wages[-train,], type='response')
out_sample_rmse <- rmse(predicted=out_sample_preds, observed = wages$lwage[-train])

out_sample_rmse # Higher, but likely a better indicator of how our model is likely to perform with new data
in_sample_rmse

# WARM-UP ---

# 1) Create a validation set with the `salaries_data` - partition off 80% of the observations for training;
# 2) Fit a linear model to predict salary as a function of `rank`, `sex`, `yrs.since.phd`, and `yrs.service`,
#    using the training indices as a subset in the `subset` argument of `lm` (or index those rows using `[]`);
# 3) Generate predictions using the validation set;
# 4) Report the RMSE of the out-of-sample predictions.

glimpse(salaries_data)

# ---

# Validation sets for time-series data
train <- which(Smarket$Year < 2005) # Need to be *super* careful about data from the future polluting data in the past

fit4 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket[train,], family = binomial)

# Making predictions about movements in the S&P 500
market_probs <- predict(fit4, newdata=Smarket[-train,], type="response")
hist(market_probs)

# Confusion matrix
market_predictions <- ifelse(market_probs > 0.5, "Up" , "Down")
table(market_predictions, Smarket$Direction[-train])

out_acc <- mean(market_predictions == Smarket$Direction[-train]) # Turns out this is *even harder* if we're simulating doing the problem in real life!
in_acc

# Cross-Validation ------------------------------------------------------------

# Doing it manually ---

# A helper function to create indexes (NOT PERFECT)
gen_cv_folds <- function(n, K) {
  idx <- 1:n
  folds <- vector('list', length=K)
  for (i in 1:K) {
    if (i == 1) {
      folds[[i]] <- sample(idx, n/K)
    } else if (i < K) {
      folds[[i]] <- sample(idx[-unlist(folds)], n/K)
    } else if (i == K) {
      folds[[i]] <- idx[-unlist(folds)]
    }
  }
  return(folds)
}

gen_cv_folds(nrow(wages), 5)
gen_cv_folds(nrow(Default), 10)

# Generating cross-validated error metrics ---

# Regression
metrics <- c()
folds <- gen_cv_folds(nrow(wages), 5)
for(i in seq_along(folds)) {
  test <- folds[[i]]
  fit <- lm(lwage ~ educ + exper + female + nonwhite + married + tenure + tenursq, data=wages[-test,])
  preds <- predict(fit, newdata = wages[test,], type='response')
  err <- rmse(predicted = preds, observed = wages$lwage[test])
  metrics <- c(metrics, err)
}

# Cross-validated RMSE
mean(metrics) # Likely an even better indicator of how our model would actually perform!

# Classification
accs <- c()
folds <- gen_cv_folds(nrow(Default), 5)
for(i in seq_along(folds)) {
  test <- folds[[i]]
  fit <- glm(default ~ balance, data=Default[-test,], family = 'binomial')
  probs <- predict(fit, newdata = Default[test,], type='response')
  preds <- ifelse(probs > 0.5, 'Yes', 'No')
  acc <- mean(preds == Default$default[test])
  accs <- c(accs, acc)
}

# Cross-validated accuracy
mean(accs) # High accuracy! But...

# A quick note on imbalanced classes...
1 - mean(Default$default == 'Yes') # If we predicted no one every defaults, we'd have accuracy of 96.7%; this is why classification reports are helpful!

# A more elegant solution...
folds <- sample(rep(1:5, length = nrow(wages))) # Randomly assign each observation a fold number
table(folds)
metrics <- c()
for(i in unique(folds)) {
  test <- which(folds == i)
  fit <- lm(lwage ~ educ + exper + female + nonwhite + married + tenure + tenursq, data=wages[-test,])
  preds <- predict(fit, newdata = wages[test,], type='response')
  err <- rmse(predicted = preds, observed = wages$lwage[test])
  metrics <- c(metrics, err)
}
mean(metrics)

# Using functions others have written ---

?cv.glm # Note that if we don't specify 'binomial', we get OLS using a `glm` model (to be safe, we can pass in 'gaussian')

# Leave-one-out cross-validation (a single test observation)
fit5 <- glm(lwage ~ educ + exper + female + nonwhite + married + tenure + tenursq, data=wages)

# Note that these generate MSE, as opposed to RMSE 
# Note: RMSE has the same units as the quantity plotted on the vertical axis, and is thus more easily interpreted in this framework
loocv_mse <- cv.glm(wages, fit5)$delta[1]
k_fold_mse <- cv.glm(wages, fit5, K=5)$delta[1]

sqrt(loocv_mse)
sqrt(k_fold_mse)
mean(metrics)

fit6 <- glm(default ~ balance, data=Default, family = 'binomial')
k_fold_error_rate <- cv.glm(Default, fit6, K=5)$delta[1] # Misclassification rate

# Using cross-validation to assess the performance of multiple models
?Auto
head(Auto)

degree <- 1:5
cv_error <- rep(0,5)
for (d in degree) {
  glm_fit <- glm(mpg ~ poly(horsepower,d), data=Auto) # Fit polynomial predictors of d-degrees
  cv_error[d] <- cv.glm(Auto, glm_fit, K=10)$delta[1]
}

plot(degree, cv_error, type="b",col="red")

# Lasso/Ridge -----------------------------------------------------------------

# Datasets
wages <- haven::read_dta('WAGE1.dta')
glimpse(wages)

# We can't pass a formula to `glmnet`, instead, we need `model.matrix`
wages_alt <- dplyr::select(wages, lwage, everything())
wages_alt$wage <- NULL
glimpse(wages_alt)

predictors <- model.matrix(lwage ~ . -1, data=wages_alt)
head(predictors)
head(wages_alt[-1])

y <- wages_alt$lwage

# Ridge ---

# Ridge regression ('compresses' coefficients close to 0, but does not drop them!)
ridge <- glmnet(predictors, y, alpha = 0) # alpha = 0 is for ridge, alpha = 1 is for lasso

# Default `plot` function on this puts the coefficients on the y-axis, as a a function of the shrinkage parameter
plot(ridge, xvar='lambda', label=T) # Note that this runs the model for a whole bunch of lambdas, but we can specify this!

# Validate our model's performance with cross-validation and pick an optimal lambda!
cv_ridge <- cv.glmnet(predictors, y, alpha=0, nfolds = 5)
plot(cv_ridge) # dotted line is the one standard error band

# Get optimal parameters
#  - min is the lambda that minimizes out-of-sample error
#  - 1se is the the largest lambda within 1 standard error of the min (more robust to overfitting)
cv_ridge

# min/1se RMSE
sqrt(cv_ridge$cvm[which(cv_ridge$lambda %in% cv_ridge$lambda.min)])
sqrt(cv_ridge$cvm[which(cv_ridge$lambda %in% cv_ridge$lambda.1se)])

# Some cleaner plotting with a little bit of work ---
library(ggplot2)

# Map lambdas to beta matrix variables
lambda_map <- data.frame(lambda = colnames(ridge$beta),
                         lambda_values = ridge$lambda,
                         stringsAsFactors = F)

# Extract coefficients relative to lamba values
ridge_by_lambda <-
  ridge$beta %>%
  matrix(dim(ridge$beta)[1]) %>%
  data.frame(stringsAsFactors = F) %>%
  mutate(predictor = row.names(ridge$beta)) %>%
  tidyr::gather(key = lambda, value = coefficient, -predictor) %>%
  mutate(lambda = paste0("s", as.numeric(gsub("X", "", lambda))-1)) %>%
  left_join(lambda_map, by="lambda")

# Visualize
ggplot(ridge_by_lambda, aes(x=lambda_values, y=coefficient, col=predictor, size=predictor)) +
  geom_line() +
  scale_color_viridis_d() +
  coord_trans(x = "log") +
  scale_size_manual(values = rep(0.5, n_distinct(ridge_by_lambda$predictor))) +
  labs(x = "lambda", y="Coefficients", title = "Coefficient size by lambda") +
  #guides(col = FALSE, size = FALSE) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Lasso ---

# Lasso regression (performs variable selection)
lasso <- glmnet(predictors, y, alpha = 1) # alpha = 0 is for lasso, alpha = 1 is for lasso

# Default `plot` function on this puts the coefficients on the y-axis, as a a function of the shrinkage parameter
plot(lasso, xvar='lambda', label=T) # Note that this runs the model for a whole bunch of lambdas, but we can specify this!

# Validate our model's performance with cross-validation and pick an optimal lambda!
cv_lasso <- cv.glmnet(predictors, y, alpha=1, nfolds = 5)
plot(cv_lasso) # dotted line is the one standard error band

# Get optimal parameters
#  - min is the lambda that minimizes out-of-sample error
#  - 1se is the the largest lambda within 1 standard error of the min (more robust to overfitting)
cv_lasso

# min/1se RMSE
sqrt(cv_lasso$cvm[which(cv_lasso$lambda %in% cv_lasso$lambda.min)])
sqrt(cv_lasso$cvm[which(cv_lasso$lambda %in% cv_lasso$lambda.1se)])

# Within one standard error, the lasso model would dictate that we drop quite a few of the predictors
coef(cv_lasso, s=cv_lasso$lambda.1se)

# Looking at the performance of the full model, it doesn't look like regularization is helping us in this instance (unsurprising as this is a toy data set)
full_fit <- glm(lwage ~ ., data=wages_alt)
k_fold_mse <- cv.glm(wages_alt, full_fit, K=5)$delta[1]
sqrt(k_fold_mse)

# Some cleaner plotting with a little bit of work ---

# Map lambdas to beta matrix variables
lambda_map <- data.frame(lambda = colnames(lasso$beta),
                         lambda_values = lasso$lambda,
                         stringsAsFactors = F)

# Extract coefficients relative to lamba values
lasso_by_lambda <-
  lasso$beta %>%
  matrix(dim(lasso$beta)[1]) %>%
  data.frame(stringsAsFactors = F) %>%
  mutate(predictor = row.names(lasso$beta)) %>%
  tidyr::gather(key = lambda, value = coefficient, -predictor) %>%
  mutate(lambda = paste0("s", as.numeric(gsub("X", "", lambda))-1)) %>%
  left_join(lambda_map, by="lambda")

lasso_by_lambda[lasso_by_lambda$lambda == 's1',] # Offers some sense of the order in which a model would pick the best predictors

# Visualize
ggplot(lasso_by_lambda, aes(x=lambda_values, y=coefficient, col=predictor, size=predictor)) +
  geom_line() +
  scale_color_viridis_d() +
  coord_trans(x = "log") +
  scale_size_manual(values = rep(0.5, n_distinct(lasso_by_lambda$predictor))) +
  labs(x = "lambda", y="Coefficients", title = "Coefficient size by lambda") +
  #guides(col = FALSE, size = FALSE) +
  theme_minimal() +
  theme(legend.title = element_blank())

# If we wanted to scale...
wages_scaled <- mutate_all(wages_alt, list(scale))
wages_scaled # BUT! `glmnet` is actually doing this for us, automatically, with the `standardize=T` default parameter

# A note on using lasso/ridge for classification
x <- model.matrix(default ~ . -1, data=Default)
y <- Default$default

glmnet(x, y, alpha=1)

y <- as.numeric(Default$default) - 1

glmnet(x, y, alpha=1, family='binomial')

# Trees -----------------------------------------------------------------------

# Datasets
?Boston
?Carseats
salaries <- read.csv('Salaries.csv', stringsAsFactors = F)

# Decision trees using the `tree` package ---
head(Carseats)
Carseats$high <- factor(ifelse(Carseats$Sales <= 8, "No", "Yes")) # Setting up a categorical target - we need this to be a factor!

# This says "model high as function of everything except Sales" - we do this because high is constructed from Sales
tree_model <- tree(high ~ . - Sales, data = Carseats)
summary(tree_model)

plot(tree_model)
text(tree_model, pretty = 0)

tree_model

# Classic approach to splitting data by randomly sampling rows and assigning to train
set.seed(1)
train <- sample(1:nrow(Carseats), 250)
tree_model_train = tree(high ~ . - Sales, Carseats, subset = train)
plot(tree_model_train) 
text(tree_model_train, pretty = 0) # Note how this tree is already quite different than our last one!

preds <- predict(tree_model_train, Carseats[-train, ], type = "class") # Setting type to class actually predicts the class labels - this is `tree` specific
table(preds, Carseats$high[-train]) # Quick and dirty classification report

# Accuracy here is not nearly as good as in sample - trees are *very* prone to overfitting the data
mean(preds == Carseats$high[-train])

full_preds <- predict(tree_model, type='class')
mean(full_preds == Carseats$high)

# Using cross-validation to 'prune' our trees
carseats_cv <- cv.tree(tree_model_train, FUN = prune.misclass)
carseats_cv

plot(carseats_cv) # Plot misclassification rate (how badly are we doing) against the complexity of the tree!

pruned_tree <- prune.misclass(tree_model_train, best = 15)
plot(pruned_tree)
text(pruned_tree, pretty = 0)

preds2 = predict(pruned_tree, Carseats[-train, ], type = "class")
table(preds2, Carseats$high[-train])
mean(preds2 == Carseats$high[-train]) # Not much, but something of an improvement

# `rpart` - prettier trees ---

rpart_tree <- rpart(high ~ . - Sales, Carseats)
summary(rpart_tree)

rpart.plot(rpart_tree, type=4, clip.facs = T, extra=104)

# In the background, already has a sense of the appropriate complexity for the best tree
?plotcp
plotcp(rpart_tree) # We can eyeball this for the best complexity parameter for our tree ('elbow method') - dotted line denotes one standard errora above the minimum, as well, to avoid overfitting
printcp(rpart_tree)

# Grab the best complexity parameter for tuning
pruned_rpart <- prune.rpart(rpart_tree, cp=0.032)

# Plot the pruned tree
rpart.plot(pruned_rpart, type=4, clip.facs = T, extra=104)

# Quality of predictions using a validation set
rpart_train <- rpart(high ~ . - Sales, Carseats[train,])

preds <- predict(rpart_train, Carseats[-train, ], type='class')
table(preds, Carseats$high[-train])
mean(preds == Carseats$high[-train])

preds2 <- predict(pruned_rpart, Carseats[-train, ], type='class')
table(preds2, Carseats$high[-train])
mean(preds2 == Carseats$high[-train]) # Has actually done a better job than tree

# For regression, using our trusty salaries_data
glimpse(salaries_data)
salaries_data$X <- NULL
salaries_data <- mutate_if(salaries_data, is.character, list(factor))

rpart_salaries <- rpart(salary ~ ., data=salaries_data)
rpart.plot(rpart_salaries, type=4, clip.facs = T, extra=1)
plotcp(rpart_salaries)

pruned_salaries <- prune.rpart(rpart_salaries, cp=0.11)
rpart.plot(pruned_salaries, type=4, clip.facs = T, extra=1) # The "best" way to split this data, is to only split on the 'rank' variable!

# Random forests --------------------------------------------------------------

set.seed(101)
dim(Boston)

train <- sample(1:nrow(Boston), 300)

rf_model <- randomForest(medv ~ ., data = Boston, subset = train)
rf_model

# Vary our forest across the `mtry` parameter (the number of splits allowed at each point in each tree)
metrics <- numeric(length=12L)
for (mtry in seq_along(metrics)) {
  message(glue::glue('Fitting randomForest models with {mtry} variable(s) per split'))
  fit <- randomForest(medv ~ ., data = Boston, subset = train, mtry = mtry, ntree = 400)
  pred <- predict(fit, Boston[-train, ])
  metrics[mtry] <- rmse(predicted=pred, observed=Boston$medv[-train])
}

plot(metrics)
min(metrics)

# Compare against a linear model...
boston_lm <- glm(medv ~ ., data = Boston, subset = train)
preds <- predict(boston_lm, newdata = Boston[-train,])
rmse(preds, Boston$medv[-train]) # Our randomForest model is doing a *much* better job here
