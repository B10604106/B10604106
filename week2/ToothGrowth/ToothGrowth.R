library(datasets)
library(dplyr)
library(ggplot2)
ggplot(data = ToothGrowth, aes( x = len, y = supp, color = dose)) + 
  geom_point() 

#The Effect of Vitamin C on Tooth Growth in Guinea Pigs
##資料中的levels分兩種一種是"OJ"，一種是"VC"
##顏色表示劑量
