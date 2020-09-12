# Discovery for creating ML predictions using FPL data  ---------------------------------------------------------------
# Also includes feature engineering

library(stringr)
library(tidyverse)

#Second iteration - added sample of xG data

setwd("~/fpl/fpl_ML")

#data hosted on github. Credit : https://github.com/vaastav/Fantasy-Premier-League/

#get GW data
data <- read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2019-20/gws/merged_gw.csv')

#Get fixtures data
fixtures <- read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2019-20/fixtures.csv')

#Get teams data
teams <- read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2019-20/teams.csv')

#Get player id list
playerid <- read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2019-20/player_idlist.csv')

#xg data -> currently just a subset for FWDs only GWs 27-38
xgdata <- read.csv('xG_by_gameweek.csv')

#### Data cleaning FPL data ####

# # Recasting post COVID Gameweeks
 
 for(i in 1:length(data$GW)){
   if(data[i,"GW"] >=39){
     data[i,"GW"] <- data[i,"GW"]-9}
 }

for(i in 1:length(fixtures$event)){
  if(fixtures[i,"event"] >=39){
    fixtures[i,"event"] <- fixtures[i,"event"]-9}
}

#Extracting player id

data$playerid <- as.integer(gsub(".*?([0-9]+).*", "\\1", data$name))

#select variables we want from 'data' for the model - the GW score (total_points) is the target variable
data <- filter(data, minutes>45 )

GW_score <- select(data, playerid, GW, was_home, total_points, fixture, value, ict_index, selected, transfers_in, transfers_out)

#add fixture info
fixture_s <- select(fixtures, 'id','team_a','team_h','team_a_difficulty','team_h_difficulty')

#Merge GW data with fixture data
GW_score <- merge(GW_score,fixture_s, by.x='fixture', by.y='id',all.x = TRUE )

#Identify team
for(i in 1:length(GW_score$total_points)){
 if(GW_score[i,'was_home']=='True'){
   GW_score[i,'team'] <- GW_score[i,'team_h']
   GW_score[i,'opp_team'] <- GW_score[i,'team_a']
                              }
    else{GW_score[i,'team'] <- GW_score[i,'team_a']
         GW_score[i,'opp_team'] <- GW_score[i,'team_h']
         }
}

#Fixture difficulty
for(i in 1:length(GW_score$total_points)){
  if(GW_score[i,'was_home']=='True'){
    GW_score[i,'team_diff'] <- GW_score[i,'team_h_difficulty']
    GW_score[i,'opp_team_diff'] <- GW_score[i,'team_a_difficulty']
  }
  else{GW_score[i,'team_diff'] <- GW_score[i,'team_a_difficulty']
  GW_score[i,'opp_team_diff'] <- GW_score[i,'team_h_difficulty']
  }
}

#### Feature engineering - GW goal data ####

GW_goals <- select(data, 'playerid','GW','goals_scored') %>% 
            arrange(playerid) %>% 
            group_by(playerid) %>% 
            mutate(cum_goals = cumsum(goals_scored))

#shifting Gameweek by one - so it's the goals up until and not including that GW
GW_goals$GW = GW_goals$GW + 1
GW_goals <- rename(GW_goals, prev_goals = goals_scored )

                  

#### Feature engineering - points so far ####

cum_points <- select(GW_score, 'playerid','GW','total_points')
cum_points <- arrange(cum_points,playerid,GW)

#cumulative points
for(i in unique(cum_points$playerid)){
  tp <- cum_points$total_points[cum_points$playerid==i]
  cum_points$cum_points[cum_points$playerid==i] <- cumsum(tp)
}

#points per gameweek
cum_points$ppgw <- cum_points$cum_points/cum_points$GW

#shifting Gameweek by one - so it's the score up until and not including that GW
cum_points$GW = cum_points$GW + 1

#drop total_points variable for tidyness 
cum_points <- select(cum_points, -total_points)



#### Feature engineering - xG data ####

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

#add second name to GW data

joined <- merge(x=GW_score, y=playerid, by.x=("playerid"), by.y="id")

#partial xG data
#joined <- merge(x=joined, y=xgdata, by.x=c("second_name","GW"), by.y=c("name","GW"))

joined <- merge(x=joined, y=GW_goals, by.x=c("playerid","GW"), by.y=c("playerid","GW"))

joined <- merge(x=joined, y=cum_points, by.x=c("playerid","GW"), by.y=c("playerid","GW"))


str(joined)


substr()




library(ggplot2)

ggplot(xgdata,aes(x=g,y=xg)) + geom_point(aes(color=name)) + theme_bw()


library(caTools)

split <- sample.split(data_c$total_points,SplitRatio = 0.7)
train <- subset(data_c,split==T)
test <- subset(data_c,split==F)





