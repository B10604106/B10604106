library(datasets)
library(dplyr)
library(ggplot2)
ggplot(data = rock, aes( x = area, y = shape, size = peri)) + 
  geom_point() 


#這是一個石頭分布與其大小還有形狀有關的圖
##根據圖可以推斷出，位在7500~12500 area之間的石頭有較高的peri，而其shape並未較突出。
##所以假使我要找較大的石頭可以去7500~12500 area找尋。