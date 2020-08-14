#Discovery for creating ML predictions using FPL data

#First iteration based on FPL data only

data <- read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2019-20/gws/merged_gw.csv')

str(data)

#choosing only variables want to build model on

head(data)

#Feature engineering

chosen <- c('fixture', 'was_home', 'total_points')


data_c <- subset(data, minutes>80)
data_c <- data_c[chosen]


library(ggplot2)

ggplot(data_c,aes(x=fixture,y=total_points)) + geom_point(aes(color=was_home)) + theme_bw()


library(caTools)

split <- sample.split(data_c$total_points,SplitRatio = 0.7)
train <- subset(data_c,split==T)
test <- subset(data_c,split==F)





