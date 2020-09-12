#try tree model on joined fpl data


# joined is created in "discovery v1.R"

#install.packages('caTools')
library(caTools)
library(rpart)
library(caret)
#for confusion matrix 

df <- joined

#create classification variable - did they return or not?
#Identify team
for(i in 1:length(df$total_points)){
  if(df[i,'total_points']>2){
    df[i,'return'] <- 1}
    else{df[i,'return'] <- 0}
}
df$return <- as.factor(df$return)

#remove rows with missing values
df <- remove_missing(df)

#create test and train sample
split <- sample.split(df$return, SplitRatio = 0.7)

training.data <- subset(df,split == TRUE)
test.data <- subset(df,split == FALSE)

#TREE Model
?rpart

dtree <- rpart(return ~ was_home + xa_avg + xg_avg + prev_xg + team_diff + opp_team_diff + cum_goals + prev_goals + cum_points + ppgw, method='class', data=training.data)

#model with no xG

dtree <- rpart(return ~ was_home + team_diff + opp_team_diff + cum_goals + prev_goals + cum_points + ppgw + selected + transfers_in + transfers_out, method='class', data=training.data)

tree.preds <- predict(dtree,test.data)
head(tree.preds)

#Create prediction column as model outputs probabilities

return_pred <- 1:(length(test.data$return))

for (i in 1:length(test.data$return)){
  
  if(tree.preds[i,1] > tree.preds[i,2]){
    return_pred[i] <- 0
  } else {return_pred[i] <- 1}
  
}


tree.preds <- cbind(tree.preds,return_pred)

#Adding the actual value to the same table - not completely necessary
tree.preds <- cbind(tree.preds,test.data['return'])

head(tree.preds)

#Creating flag for whether prediction was correct or not

Correct <- 1:(length(test.data$return))


for (i in 1:length(test.data$return)){
  
  if(tree.preds[i,'return_pred'] == tree.preds[i,'return']){
    Correct[i] <- 1
  } else {Correct[i] <- 0}
  
}

tree.preds <- cbind(tree.preds,Correct)
tree_accuracy <- mean(tree.preds$Correct)
print(tree_accuracy)

#Create confusion matrix
table(tree.preds$return_pred, test.data$return)
?table
library(rpart.plot)
prp(dtree)

tree.preds$return_pred <- as.factor(tree.preds$return_pred)



confusionMatrix(tree.preds$return_pred, test.data$return, positive = '1')
tree.preds$return
summary(tree.preds$return)

print(dtree)

########### RANDOM FOREST ##############

library(randomForest)

rf.model <- randomForest(return ~ was_home + team_diff + opp_team_diff + cum_goals + prev_goals + cum_points + ppgw + selected + transfers_in + transfers_out, data = training.data, importance=TRUE)
print(rf.model)

rf.model$confusion

#Predictions
rf.preds <- predict(rf.model,test.data)

print(rf.preds)
table(rf.preds,test.data$return)

#Calculating success rate
(267+991)/(267+991+200+471)

names <- as.data.frame(names(training.data))
write_csv(names,"~/fpl/fpl_ML/names")
?write_csv

confusionMatrix(rf.preds,test.data$return, positive='1')

###### Neural Net ########


