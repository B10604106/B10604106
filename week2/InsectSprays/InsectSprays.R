library(datasets)
library(dplyr)
library(ggplot2)
ggplot(data = InsectSprays, aes( x = factor)) + 
  geom_bar(fill = "lightblue", colour = "black") 


str(InsectSprays)
