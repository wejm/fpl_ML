#try neural net model on joined fpl data

# joined is created in "discovery v1.R"

#install.packages('caTools')
library(caTools)
library(rpart)
library(caret)

###### Neural Net ########

#First need to transform data to numeric only

df <- joined

#create classification variable - did they return or not?
#Identify team
for(i in 1:length(df$total_points)){
  if(df[i,'total_points']>2){
    df[i,'return'] <- 1}
  else{df[i,'return'] <- 0}
}

#remove rows with missing values
df <- remove_missing(df)

#create test and train sample
split <- sample.split(df$return, SplitRatio = 0.7)

training.data <- subset(df,split == TRUE)
test.data <- subset(df,split == FALSE)

str(training.data)

training.data$was_home <- as.integer(training.data$was_home) - 1

library(neuralnet)

nn <- neuralnet((return ~ was_home + team_diff + opp_team_diff + cum_goals + prev_goals + cum_points + ppgw + selected + transfers_in + transfers_out), training.data, linear.output=FALSE, hidden = 10 )
plot(nn)
str(nn)

test.data <- select(test.data, was_home , team_diff , opp_team_diff , cum_goals , prev_goals , cum_points , ppgw , selected , transfers_in , transfers_out, return)
test.data$was_home <- as.integer(test.data$was_home) - 1

predicted.nn.values <- compute(nn, test.data[1:10])
str(predicted.nn.values)

#predictions at $net.result
head(predicted.nn.values$net.result)

#predictions are in probability format so we use round 
predictions <- round(predicted.nn.values$net.result)

#create confusion matrix
table(predictions,test.data$return)
