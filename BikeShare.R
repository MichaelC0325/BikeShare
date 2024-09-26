library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(GGally)
library(DataExplorer)
library(patchwork)
library(glmnet)

ggpairs(dataset)
plot_intro(dataset)

trainData <- vroom("train.csv")
testData <- vroom("test.csv")

#windspeed vs count scatterplot
windspeed_plot <- ggplot(data=dataset, mapping=aes(x=windspeed, y=count)) + 
  geom_point() +
  geom_smooth(se=FALSE)

#humidity vs count scatterplot
humidity_plot <- ggplot(data=dataset, mapping=aes(x=humidity, y=count)) + 
  geom_point() +
  geom_smooth(se=FALSE)

#temp vs count scatterplot
temp_plot <- ggplot(data=dataset, mapping=aes(x=temp, y=count)) + 
  geom_point() +
  geom_smooth(se=FALSE)

#weather barplot
weather_plot <- ggplot(data, aes(x = weather)) + 
  geom_bar()

(weather_plot + temp_plot) / (windspeed_plot + humidity_plot)


# feature engineering -----------------------------------------------------


trainData <- vroom("train.csv")
testData <- vroom("test.csv")

#feature engineering
trainData <- trainData |> 
  select(-casual, -registered) |> 
  mutate(count = log(count))

my_recipe <- recipe(count~., data = trainData) |> 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |> 
  step_mutate(weather = factor(weather), levels = 3) |> 
  step_time(datetime, features = c("hour", "minute")) |> 
  step_mutate(season=factor(season, levels= 4)) |> 
  step_zv(all_predictors()) |> 
  step_mutate(holiday = factor(holiday), levels = 2) |>
  step_mutate(workingday = factor(workingday), levels = 2) |> 
  step_poly(windspeed, degree=2) |> 
  step_mutate(datetime_hour = factor(datetime_hour), levels = 24)


bike_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet
bake(bike_recipe, new_data=trainData)


## Define a Model
lin_model <- linear_reg() %>%
set_engine("lm") %>%
set_mode("regression")

# Combine into a Workflow and fit
bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%  # Add the untrained recipe here
  add_model(lin_model) %>%
  fit(data = trainData)  # Fit with training data


## Run all the steps on test data
lin_preds <- predict(bike_workflow, new_data = testData)
lin_preds = exp(lin_preds)

## Format the Predictions for Submission to Kaggle
kaggle_submission_feature_engineering <- lin_preds %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) #needed for right

## Write out the file
vroom_write(x=kaggle_submission_feature_engineering, file="./FeatureEngineeringPreds7.csv", delim=",")



# penalized regression ----------------------------------------------------


trainData <- vroom("train.csv")
testData <- vroom("test.csv")

#Penalized regression
trainData <- trainData |> 
  select(-casual, -registered) |> 
  mutate(count = log(count))

my_recipe <- recipe(count~., data = trainData) |> 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |> 
  step_mutate(weather = factor(weather), levels = 3) |> 
  step_time(datetime, features = c("hour", "minute")) |> 
  step_mutate(season=factor(season, levels= 4)) |> 
  step_zv(all_predictors()) |> 
  step_mutate(holiday = factor(holiday), levels = 2) |>
  step_mutate(workingday = factor(workingday), levels = 2) |> 
  step_poly(windspeed, degree=2) |> 
  step_mutate(datetime_hour = factor(datetime_hour), levels = 24) |> 
  step_rm(datetime) |> 
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors()) # Make mean 0, sd=1


bike_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet
bake(bike_recipe, new_data=trainData)

## Penalized regression model
penalized_model <- linear_reg(penalty=.01, mixture=0) %>% #Set model and tuning
  set_engine("glmnet")# Function to fit in R


preg_wf <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(penalized_model) %>%
fit(data=trainData)

## Run all the steps on test data
penalized_preds <- predict(preg_wf, new_data = testData)
penalized_preds = exp(penalized_preds)

## Format the Predictions for Submission to Kaggle
kaggle_submission_feature_engineering <- penalized_preds %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) #needed for right

## Write out the file
vroom_write(x=kaggle_submission_feature_engineering, file="./PenlizedPreds10.csv", delim=",")




# -------------------------------------------------------------------------


library(tidymodels)

## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response
  fit(formula=log(count)~as.factor(season)+as.factor(holiday)+as.factor(weather)
      +temp+humidity+windspeed, data=dataset)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,
                            new_data=test_data) # Use fit to predict
bike_predictions ## Look at the output




## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>%
bind_cols(., test_data) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds2.csv", delim=",")

# -------------------------------------------------------------------------

library(poissonreg)

my_pois_model <- poisson_reg() %>% #Type of model
  set_engine("glm") %>% # GLM = generalized linear model
  set_mode("regression") %>%
fit(formula=count~as.factor(season)+as.factor(holiday)+as.factor(weather)
    +temp+humidity+windspeed, data=trainData)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_pois_model,
                            new_data=testData) # Use fit to predict
bike_predictions ## Look at the output



## Format the Predictions for Submission to Kaggle
pois_kaggle_submission <- bike_predictions %>%
bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) #needed for right

## Write out the file
vroom_write(x=pois_kaggle_submission, file="./PoissonPreds.csv", delim=",")


# Regression Trees --------------------------------------------------------

library(tidymodels)


trainData <- vroom("train.csv")
testData <- vroom("test.csv")

my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n = tune()) |> 
set_engine("rpart") |> 
set_mode("regression")

my_recipe <- recipe(count~., data = trainData) |> 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |> 
  step_mutate(weather = factor(weather), levels = 3) |> 
  step_time(datetime, features = c("hour", "minute")) |> 
  step_mutate(season=factor(season, levels= 4)) |> 
  step_mutate(holiday = factor(holiday), levels = 2) |>
  step_mutate(workingday = factor(workingday), levels = 2) |> 
  step_poly(windspeed, degree=2) |> 
  step_mutate(datetime_hour = factor(datetime_hour), levels = 24) |> 
  step_rm(datetime)

my_recipe <- recipe(count ~ ., data = trainData) |> 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |>  # Adjust weather
  step_mutate(weather = factor(weather)) |>                  # Convert weather to factor
  step_time(datetime, features = c("hour", "minute")) |>      # Extract time features
  step_mutate(season = factor(season, levels = 1:4)) |>       # Convert season to factor
  step_mutate(holiday = factor(holiday, levels = c(0, 1))) |> # Convert holiday to factor
  step_mutate(workingday = factor(workingday, levels = c(0, 1))) |> # Convert workingday to factor
  step_poly(windspeed, degree = 2) |>                         # Apply polynomial transformation
  step_rm(datetime)                                           # Remove datetime



tree_wf <- workflow() %>%
  add_recipe(my_recipe) |> 
  add_model(my_mod) %>%
  fit(data=trainData)

## Run all the steps on test data
tree_preds <- predict(tree_wf, new_data = testData)
tree_preds = exp(tree_preds)

## Format the Predictions for Submission to Kaggle
kaggle_submission_feature_engineering <- tree_preds %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) #needed for right

## Write out the file
vroom_write(x=kaggle_submission_feature_engineering, file="./TreePreds.csv", delim=",")


# Regression Trees --------------------------------------------------------

library(tidymodels)

trainData <- vroom("train.csv")
testData <- vroom("test.csv")

trainData <- trainData |> 
  select(-casual, -registered) |> 
  mutate(count = log(count))

my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

## Create a workflow with model & recipe

my_recipe <- recipe(count~., data = trainData) |> 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |> 
  step_mutate(weather = factor(weather), levels = 3) |> 
  step_time(datetime, features = c("hour", "minute")) |> 
  step_mutate(season=factor(season, levels= 4)) |> 
  step_zv(all_predictors()) |> 
  step_mutate(holiday = factor(holiday), levels = 2) |>
  step_mutate(workingday = factor(workingday), levels = 2) |> 
  step_poly(windspeed, degree=2) |> 
  step_mutate(datetime_hour = factor(datetime_hour), levels = 24) |> 
  step_rm(datetime)

forest_wf <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(my_mod)

## Set up grid of tuning values

grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 5) ## L^2 total tuning possibilities

# mtry(range = c(1,10), min_n()

grid_of_tuning_params <- grid_regular(
  mtry(range = c(2, 10)),
  min_n(range = c(1, 10)),
  levels = 5
)

## Set up K-fold CV

folds <- vfold_cv(trainData, v = 5, repeats=1)

## Find best tuning parameters

## Run the CV1
CV_results <- forest_wf %>%
tune_grid(resamples=folds,
          grid=grid_of_tuning_params,
          metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best(metric = "rmse")

## Finalize workflow and predict

# Finalize the Workflow & fit it
final_wf <-
forest_wf %>%
finalize_workflow(bestTune) %>%
fit(data=trainData)

## Predict
forest_preds <- final_wf %>%
predict(new_data = testData)

forest_preds <- exp(forest_preds)

## Format the Predictions for Submission to Kaggle
kaggle_submission_feature_engineering <- forest_preds %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) #needed for right

## Write out the file
vroom_write(x=kaggle_submission_feature_engineering, file="./ForestPreds.csv", delim=",")
