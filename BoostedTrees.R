library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(GGally)
library(DataExplorer)
library(patchwork)
library(glmnet)
library(parsnip)
library(xgboost)


trainData <- vroom("train.csv")
testData <- vroom("test.csv")

trainData <- trainData |> 
  select(-casual, -registered) |> 
  mutate(count = log(count))

my_recipe_boost <- recipe(count~., data = trainData) |> 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |> 
  step_mutate(weather = factor(weather)) |> 
  step_time(datetime, features = c("hour")) |> 
  step_mutate(season = factor(season)) |> 
  step_mutate(holiday = factor(holiday)) |>
  step_mutate(workingday = factor(workingday)) |> 
  step_mutate(datetime_hour = factor(datetime_hour)) |> 
  step_rm(datetime) |> 
  step_poly(all_numeric_predictors(), degree = 2) |> 
  step_interact(~ all_predictors():all_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_normalize(all_numeric_predictors())


boost_spec <- boost_tree(
  trees = 250,           # Number of trees (adjust as needed)
  tree_depth = 6,        # Depth of each tree
  min_n = 5,            # Minimum number of observations in a node
  learn_rate = 0.1,      # Learning rate
  loss_reduction = 0,    # Minimum loss reduction for a split
  sample_size = 1,       # Proportion of samples to use in each tree
  stop_iter = 20         # Early stopping rounds
) %>%
  set_engine("xgboost", eval_metric = "rmse") %>%  # Use RMSE as evaluation metric
  set_mode("regression")  # Set mode to regression since you're predicting a continuous value (log(count))


workflow_boost <- workflow() %>%
  add_recipe(my_recipe_boost) %>%
  add_model(boost_spec) %>%
  fit(data = trainData)


boost_predictions <- predict(workflow_boost, new_data = testData)
boost_predictions <- exp(boost_predictions)


## Format the Predictions for Submission to Kaggle
kaggle_submission_feature_engineering <- boost_predictions %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) #needed for right

## Write out the file
vroom_write(x=kaggle_submission_feature_engineering, file="./BoostPreds3.csv", delim=",")
