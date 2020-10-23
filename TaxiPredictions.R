library(tidyverse)
clean_taxi <- read_csv("/Users/graceedriggs/Documents/Stat 495/Taxi-Duration/TaxiCleaned.csv")
plot_missing(clean_taxi)

#change POSIXct to numeric
clean_taxi$dropoff_datetime <- as.numeric(clean_taxi$dropoff_datetime)
clean_taxi$pickup_datetime <- as.numeric(clean_taxi$pickup_datetime)

#split
taxi.train <- clean_taxi %>% filter(!is.na(dropoff_datetime))
taxi.test <- clean_taxi %>% filter(is.na(dropoff_datetime))

### XGB boosting
control <- trainControl(method='repeatedcv', 
                        number=2, 
                        repeats=1)
grid <- expand.grid(
  nrounds = 200,
  max_depth = 25,
  eta = .01,
  gamma = 10,
  colsample_bytree = 1,
  min_child_weight = 65,
  subsample = 1)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)

# Testing with only 10000 values to get the right tuning parameters
xgb_default <- train(trip_duration ~ ., 
                    data=taxi.train %>% select(-id, -dropoff_datetime) %>%
                      slice(sample(nrow(taxi.train), 500000)), 
                    method='xgbTree', 
                    trControl=control,
                    tuneGrid = grid)

names(xgb_default)
plot(xgb_default)
xgb_default$bestTune

predictions <- data.frame(id=taxi.test$id, trip_duration = (predict(xgb_default, newdata=taxi.test)))

## write to a csv
write.csv(predictions,"/Users/graceedriggs/Documents/STAT 495/Taxi-Duration/GD_XGB_TaxiPredictions.csv", row.names = FALSE)


#taxi.test$dropoff_datetime <- (predict(xgb_default, newdata=taxi.test))
#taxi.train$dur <- as.numeric(taxi.train$dropoff_datetime) - as.numeric(taxi.train$pickup_datetime)
