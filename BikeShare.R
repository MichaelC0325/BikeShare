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
