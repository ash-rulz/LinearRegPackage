install.packages("caret")
library(caret)
library(mlbench)

training_index <- createDataPartition(BostonHousing$medv, p=0.77, list = F)
data_train <- BostonHousing[training_index,]
data_test <- BostonHousing[-training_index,]
nrow(data_train) #407
nrow(data_test) #99
dim(BostonHousing)

#Before tuning the model
lmFit <- train(medv ~ ., 
               data = data_train,
               method = 'leapForward',
               preProcess = c("scale", "center"))
)
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

lm_untuned_perf
lm_tuned_perf


#Calling ridgereg
r1 <- ridgereg$new()
