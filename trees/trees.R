library(datasets)
library(dplyr)
library(ggplot2)
ggplot(data = trees, aes( x = Girth, y = Height)) + 
  geom_point() 