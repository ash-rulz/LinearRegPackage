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
flight_data <- dplyr::sample_n(nycflights13::flights, 90000, replace = FALSE) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(.,delay = sum(dep_delay, arr_delay, na.rm = T)) %>%
  dplyr::inner_join(., nycflights13::weather, 
                     by = c("origin", "year", "month", "day", "hour")) %>%
  dplyr::select(.,
                # year,
                # month,
                # day,
                dep_time,
                arr_time,
                # carrier,
                # flight,
                # origin,
                # dest,
                air_time,
                distance,
                delay,
                # temp,
                humid,
                # wind_dir,
                wind_speed,
                # precip,
                visib)

# View(flight_data)
#Converting categorical data into numerical
str(flight_data) #carrier, origin, dest are categorical data
flight_data$carrier <- as.factor(flight_data$carrier)
flight_data$origin <- as.factor(flight_data$origin)
flight_data$dest <- as.factor(flight_data$dest)
flight_data[,c('carrier', 'origin', 'dest')] <- 
  sapply(flight_data[,c('carrier', 'origin', 'dest')], unclass)

#Clean the data
# flight_data <- flight_data[,-1] #Remove the year
#Check if the data has NA or missing values
names(which(colSums(is.na(flight_data))>1))
flight_data <- tidyr::replace_na(flight_data,
                                  list(dep_time = median(flight_data$dep_time, na.rm = T),
                                       arr_time = median(flight_data$arr_time, na.rm = T),
                                       air_time = median(flight_data$air_time, na.rm = T),
                                       # temp = median(flight_data$temp, na.rm = T),
                                       humid = median(flight_data$humid, na.rm = T),
                                       # wind_dir = median(flight_data$wind_dir, na.rm = T),
                                       wind_speed = median(flight_data$wind_speed, na.rm = T)
                                  ))
flight_data[is.na(flight_data$dep_time),]
# View(flight_data)

#Split the data into test/train/validation in 5/80/15 % split
dim(flight_data) #90000    20
training_index <- caret::createDataPartition(flight_data$delay, p=0.95, list = F)
data_test <- flight_data[-training_index,]
dim(data_test)#4500   20
validation_index <- createDataPartition(flight_data$delay[training_index], 
                                      p=0.16, list = F)
data_validation <- flight_data[validation_index,]
data_train <- flight_data[-validation_index,]

dim(data_validation) #13680    20
dim(data_train) # 75983    18


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
                         lambda = 0.5)

postResample(pred = ridgeobj$y_pred, obs = data_train$delay)


lambda = seq(0, 0.25, by = 0.0001)
length(lambda)
y_rmse <- c()
c_rsq <- c()
c_mae <- c()
for(i in lambda){
  
  x <- ridgereg(formula = delay~., data = data_train, lambda = i)
  x_pred <- x$predict(data_validation)
  tuned_perf_ridge <- postResample(pred = x_pred, obs = data_validation$delay)
  y_rmse <- c(y_rmse, tuned_perf_ridge[1])
  c_rsq <- c(c_rsq, tuned_perf_ridge[2])
  c_mae <- c(c_mae, tuned_perf_ridge[3])
  
}
plot(lambda, y_rmse)
plot(lambda, c_mae)
plot(lambda, c_rsq)

ridgeobj$predict(data_validation)
tuned_perf <- postResample(pred = ridgeobj$predict(data_validation), 
                           obs = data_validation$delay)

