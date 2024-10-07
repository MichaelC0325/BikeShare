library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(GGally)
library(DataExplorer)
library(patchwork)
library(glmnet)
library(parsnip)
library(dbarts)


trainData <- vroom("train.csv")
testData <- vroom("test.csv")


trainData <- trainData |> 
  select(-casual, -registered) |> 
  mutate(count = log(count))

my_recipe_bart <- recipe(count~., data = trainData) |> 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |> 
  step_mutate(weather = factor(weather)) |> 
  step_time(datetime, features = c("hour")) |> 
  step_date(datetime, features = c("month", "year")) |> 
  step_mutate(datetime_month = factor(datetime_month)) |> 
  step_mutate(datetime_year = factor(datetime_year)) |> 
  step_mutate(season = factor(season)) |> 
  step_mutate(holiday = factor(holiday)) |>
  step_mutate(workingday = factor(workingday)) |> 
  step_mutate(datetime_hour = factor(datetime_hour)) |> 
  step_rm(datetime) |> 
  step_interact(~ all_predictors():all_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_normalize(all_numeric_predictors())


preped <- prep(my_recipe_bart)
baked <- bake(preped, new_data = testData)
baked



bart_model <- parsnip::bart(trees = 100) |> 
  set_engine("dbarts") |> 
  set_mode("regression")


# Create a workflow to combine the recipe and model specification
bart_workflow <- workflow() %>%
  add_recipe(my_recipe_bart) %>%
  add_model(bart_model) |> 
  fit(data = trainData)

# Fit the BART model to the training data
bart_predictions <- predict(bart_workflow, new_data = testData)
bart_predictions <- exp(bart_predictions)

## Format the Predictions for Submission to Kaggle
kaggle_submission_bart <- bart_predictions %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) #needed for right

## Write out the file
vroom_write(x=kaggle_submission_bart, file="./BartPreds15.csv", delim=",")
