library(datasets)
library(dplyr)
library(ggplot2)
ggplot(data = ToothGrowth, aes( x = len, y = supp, color = dose)) + 
  geom_point() 
