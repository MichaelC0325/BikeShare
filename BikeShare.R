library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(GGally)
library(DataExplorer)
library(patchwork)

ggpairs(dataset)
plot_intro(dataset)

dataset <- vroom("train.csv")

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


# -------------------------------------------------------------------------

summary(dataset)


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
                            new_data=dataset) # Use fit to predict
bike_predictions ## Look at the output




## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>%
bind_cols(., dataset) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")
