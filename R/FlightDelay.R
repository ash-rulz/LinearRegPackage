install.packages("nycflights13")
install.packages("caret")
install.packages("dplyr")
library(nycflights13)
library(caret)
library(dplyr)
library(tidyr)
# View(weather)
# View(flights)


#Create the new delay field summing up the arrival and departure delays. Used 
#only 90k records as the createDataPartition was failing for higher records.
flight_data <- dplyr::rowwise(nycflights13::flights) %>%
  dplyr::mutate(.,delay = sum(dep_delay, arr_delay, na.rm = T)) %>%
  dplyr::filter(., delay > 0) %>%
  dplyr::inner_join(., nycflights13::weather, 
                     by = c("origin", "year", "month", "day", "hour")) %>%
  dplyr::select(.,
                carrier,
                tailnum,
                origin,
                dest,
                month,
                day,
                flight,
                dep_time,
                arr_time,
                air_time,
                distance,
                delay,
                temp,
                humid,
                wind_dir,
                wind_speed,
                precip,
                visib)

# View(flight_data)
#Converting categorical data into numerical
# str(flight_data) #carrier, origin, dest are categorical data
flight_data$carrier <- as.factor(flight_data$carrier)
flight_data$origin <- as.factor(flight_data$origin)
flight_data$dest <- as.factor(flight_data$dest)
flight_data$tailnum <- as.factor(flight_data$tailnum)
flight_data[,c('carrier', 'origin', 'dest', 'tailnum')] <- 
  sapply(flight_data[,c('carrier', 'origin', 'dest' , 'tailnum')], unclass)

#Clean the data
# flight_data <- flight_data[,-1] #Remove the year
#Check if the data has NA or missing values
names(which(colSums(is.na(flight_data))>1))
flight_data <- na.omit(flight_data)
# flight_data <- tidyr::replace_na(flight_data,
#                                   list(
#                                     dep_time = median(flight_data$dep_time, na.rm = T),
#                                     arr_time = median(flight_data$arr_time, na.rm = T),
#                                     tailnum = median(flight_data$tailnum, na.rm = T),
#                                     air_time = median(flight_data$air_time, na.rm = T),
#                                     temp = median(flight_data$temp, na.rm = T),
#                                     humid = median(flight_data$humid, na.rm = T),
#                                     wind_dir = median(flight_data$wind_dir, na.rm = T),
#                                     wind_speed = median(flight_data$wind_speed, na.rm = T)
#                                   ))
names(which(colSums(is.na(flight_data))>1))
# View(flight_data)

#Split the data into test/train/validation in 5/80/15 % split
# dim(flight_data) #90000    20
training_index <- caret::createDataPartition(flight_data$delay, p=0.95, list = F)
data_test <- flight_data[-training_index,]
# dim(data_test)#4500   20
validation_index <- createDataPartition(flight_data$delay[training_index], 
                                      p=0.16, list = F)
data_validation <- flight_data[validation_index,]
data_train <- flight_data[-validation_index,]

dim(data_validation) #13680    20
dim(data_train) # 75983    18

#Pre-process the data
preProcValues <- preProcess(flight_data[,c('dep_time',
                                           'arr_time',
                                           'air_time',
                                           'distance',
                                           'delay',
                                           'temp',
                                           'humid',
                                           'wind_dir',
                                           'wind_speed',
                                           'precip',
                                           'visib')], method = c("center", "scale"))
data_train <- predict(preProcValues, data_train)
data_validation <- predict(preProcValues, data_validation)
data_test <- predict(preProcValues, data_test)

#Train the model

# X <- model.matrix(delay~., cleaned_data)
# dim(X) #75983   134
# X[,-1] <- scale(X[,-1])
# y <- as.matrix(cleaned_data[,all.vars(delay~.)[1]])
# dim(X)
# dim(t(X))
# dim(y)
# beta_ridge_f <- solve((t(X)%*%X)+(diag(ncol(X)))*0.5)%*%(t(X)%*% y)
# dim(beta_ridge_f)
# y_pred_f <- X %*% beta_ridge_f
# beta_ridge_f <- as.vector(beta_ridge_f)

ridgeobj <- ridgereg$new(formula = delay~., 
                         data = data_train, 
                         lambda = 0)

postResample(pred = ridgeobj$y_pred, obs = data_train$delay)
# RMSE        Rsquared  MAE 
# 75.9576582  0.1347723 43.6669446 
#The RMSE is quite high.

#Ignore the na rows and try
#After ignoring the na rows, the RMSE is same
# RMSE   Rsquared        MAE 
# 76.3138287  0.1544411 43.7435116 
#Revert back to having median values in missing values

#Ignore the rows where there are no delays
#After ignoring the entries without any delays, we get a better solution
# RMSE   Rsquared        MAE 
# 11.3249612  0.1266964  9.0650340

#Scaling and centering the data
# RMSE  Rsquared       MAE 
# 0.9354605 0.1256278 0.5913300 
final_df <- data.frame(matrix(nrow=0,ncol=4))
colnames(final_df) <- c('lambda',
                        'RMSE',
                        'RSQ',
                        'MAE')
coef_df <- NULL
for (i in 1:50) {
  robj <- ridgereg$new(formula = delay~., 
                           data = data_train, 
                           lambda = i)  
  x_pred <- robj$predict(data_validation)
  perf_ridge <- postResample(pred = x_pred, obs = data_validation$delay)
  final_df <- rbind(final_df, data.frame(lambda=i,
                               RMSE=perf_ridge[1],
                               RSQ=perf_ridge[2],
                               MAE=perf_ridge[3]))
  coef <- c(lambda = i,robj$beta_ridge)
  coef_df <- rbind(coef_df, t(as.data.frame(coef)))
}
rownames(final_df) <- NULL
final_df
coef_df <- as.data.frame(coef_df)
ggplot(coef_df) + geom_line(aes(x=lambda, y= humid))

ggplot(final_df) + geom_line(aes(x=lambda, y= RMSE))
geom_line(aes(x=lambda, y= RMSE))
+ geom_line(aes(x=lambda, y= MAE))
+ geom_line(aes(x=lambda, y= RSquare))

lambda = seq(0, 10, by = .01)
length(lambda)
y_rmse <- c()
c_rsq <- c()
c_mae <- c()
coef_df <- NULL
for(i in lambda){
  x <- ridgereg(formula = delay~., data = data_validation, lambda = i)
  x_pred <- x$predict(data_validation)
  tuned_perf_ridge <- postResample(pred = x_pred, obs = data_validation$delay)
  y_rmse <- c(y_rmse, tuned_perf_ridge[1])
  c_rsq <- c(c_rsq, tuned_perf_ridge[2])
  c_mae <- c(c_mae, tuned_perf_ridge[3])
  coef <- c(lambda = i,x$beta_ridge)
  coef_df <- rbind(coef_df, t(as.data.frame(coef)))
}

plot_df <- data.frame(lambda = lambda, RMSE = y_rmse,
                      MAE = c_mae, RSquare = c_rsq)
rownames(coef_df) <- NULL
coef_df <- as.data.frame(coef_df)
coef_melt <- melt(coef_df ,  id.vars = 'lambda', variable.name = 'series')
ggplot(coef_melt, aes(lambda, value)) +
  geom_line(aes(colour = series))

ggplot(plot_df) + geom_line(aes(x=lambda, y= RSquare))
  geom_line(aes(x=lambda, y= RMSE))
  + geom_line(aes(x=lambda, y= MAE))
  + geom_line(aes(x=lambda, y= RSquare))
  

ridgeobj$predict(data_validation)
tuned_perf <- postResample(pred = ridgeobj$predict(data_validation), 
                           obs = data_validation$delay)

