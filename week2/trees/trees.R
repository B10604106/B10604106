# "此圖為樹木的周長和高度作圖"
library(datasets)
library(dplyr)
library(ggplot2)
ggplot(data = trees, aes( x = Girth, y = Height)) + 
  geom_point() 
## "由圖可看出樹木的周長大多為10~14之間，而其高度大多在70以上"
