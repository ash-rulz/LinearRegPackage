install.packages("caret")
library(caret)
library(mlbench)
library(LinearRegPackage)
data("BostonHousing")

training_index <- createDataPartition(BostonHousing$medv, p=0.77, list = F)
data_train <- BostonHousing[training_index,]
data_test <- BostonHousing[-training_index,]
View(data_test)
nrow(data_train) #407
nrow(data_test) #99
dim(BostonHousing)

#Before tuning the model
lmFit <- train(medv ~ .,
               data = data_train,
               method = 'leapForward',
               preProcess = c("scale", "center"))

bh_pred <- predict(lmFit, data_test)
untuned_perf <- postResample(pred = bh_pred, obs = data_test$medv)


#After tuning the model
lmFit <- train(medv ~ .,
               data = data_train,
               method = 'leapForward',
               preProcess = c("scale", "center"),
               tuneGrid = expand.grid(nvmax = seq(1, 13, 2))
               )
bh_pred <- predict(lmFit, data_test)
tuned_perf <- postResample(pred = bh_pred, obs = data_test$medv)
tuned_perf
lm_untuned_perf
lm_tuned_perf


#Fitting ridge regression model to the training data set for different
# values of lambda (look at the plot too)

lambda = seq(0, 25, by = 1)
y_rmse <- c()
c_rsq <- c()
c_mae <- c()
for(i in lambda){

  x <- ridgereg(formula = medv~., data = data_train, lambda = i)
  x_pred <- x$predict(data_test)
  tuned_perf_ridge <- postResample(pred = x_pred, obs = data_test$medv)
  y_rmse <- c(y_rmse, tuned_perf_ridge[1])
  c_rsq <- c(c_rsq, tuned_perf_ridge[2])
  c_mae <- c(c_mae, tuned_perf_ridge[3])

}
plot(lambda, y_rmse)
plot(lambda, c_mae)
plot(lambda, c_rsq)


#Finding best hyperparameter lambda using 10 fold cross validation
ctrl <- trainControl(method = 'repeatedcv', repeats = 10)
ridgefit <- train(medv~.,
                  data = data_train,
                  method = 'ridge',
                  preProcess = c('scale', 'center'),
                  tuneLength = 10,
                  trControl = ctrl
                  )
ridgefit
plot(ridgefit)

#performance evaluation of Linear Regression model fit for predicting test data

tuned_perf <- postResample(pred = bh_pred, obs = data_test$medv)
tuned_perf
#performance evaluation of Ridge Regression model fit for predicting test data

y_rmse
c_rsq
c_mae

plot(lambda, y_rmse)
plot(lambda, c_mae)
plot(lambda, c_rsq)

#performance evaluation of Ridge Regression by 10-fold-cross validation

ridgefit
plot(ridgefit)
