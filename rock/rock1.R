library(datasets)
library(dplyr)
library(ggplot2)
ggplot(data = rock, aes( x = area, y = shape, size = peri)) + 
  geom_point() 