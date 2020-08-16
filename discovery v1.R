#Discovery for creating ML predictions using FPL data

library(stringr)
library(tidyverse)

#Second iteration - added sample of xG data

setwd("~/fpl/fpl_ML")

data <- read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2019-20/gws/merged_gw.csv')

#xg data is currently just a subset for FWDs only GWs 27-38
xgdata <- read.csv('xG_by_gameweek.csv')


#### Data cleaning fpl data ####

# Recasting post COVID Gameweeks

for(i in 1:length(data$GW)){
  if(data[i,"GW"] >=39){
    data[i,"GW"] <- data[i,"GW"]-9}
}

#Extracting player surname from 'FORENAME_SURNAME_NUMBER' format. Could be more elegant....

a <- data$name
data_firstname <- gsub("(_).*", "\\1", a)
data_surname <- str_remove(data$name,data_firstname)
a <- data_surname
data_surname <- gsub("(_).*", "\\1", a)
data$player <- str_remove(data_surname,'_')

#just want player name and fpl score by gameweek - the GW score is the target variable

GW_score <- select(data, player, GW, was_home, total_points)
?select

#### Feature engineering ####

#Generate some features from the xgdata

#have to order by player name and GW for the following row operations to work
xgdata <- arrange(xgdata,name,GW)

#grab previous weeks xg
for(i in 1:length(xgdata$GW)){
  if(i==1){xgdata[i,'prev_xg'] <- NA}
 else if(xgdata[i-1,'name']==xgdata[i,'name']){ xgdata[i,'prev_xg'] <- xgdata[i-1,'xg']
  } else{xgdata[i,'prev_xg'] <- NA}
}

#Let's try previous 5 GWeeks xG score...
for(i in 1:length(xgdata$GW)){
  #only rows where at least 5 GWs of data exist and not first 5 rows to avoid a 0 subscript
  if((xgdata[i,'GW']<32)|(i<6) ){xgdata[i,'xg_avg'] <- NA}
  #check previous 5 rows are for the player - if so, take mean value of previous 5 rows
  else if(xgdata[(i-5),'name']==xgdata[i,'name']){xgdata[i,'xg_avg']  <- mean(xgdata[(i-5):i,'xg'])
  } else{xgdata[i,'xg_avg'] <- NA}
}

#Now last 5 weeks xA score
for(i in 1:length(xgdata$GW)){
  #only rows where at least 5 GWs of data exist and not first 5 rows to avoid a 0 subscript
  if((xgdata[i,'GW']<32)|(i<6) ){xgdata[i,'xa_avg'] <- NA}
  #check previous 5 rows are for the player - if so, take mean value of previous 5 rows
  else if(xgdata[(i-5),'name']==xgdata[i,'name']){xgdata[i,'xa_avg']  <- mean(xgdata[(i-5):i,'xa'])
  } else{xgdata[i,'xa_avg'] <- NA}
}

#### Join fpl data and XG data ####






chosen <- c('fixture', 'was_home', 'total_points')


data_c <- subset(data, minutes>80)
data_c <- data_c[chosen]

substr()




library(ggplot2)

ggplot(xgdata,aes(x=g,y=xg)) + geom_point(aes(color=name)) + theme_bw()


library(caTools)

split <- sample.split(data_c$total_points,SplitRatio = 0.7)
train <- subset(data_c,split==T)
test <- subset(data_c,split==F)





