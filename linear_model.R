## Try linear model on joined fpl data

# joined is created in "discovery v1.R"

#install.packages('caTools')
library(caTools)

#being lazy and renaming it
df <- joined

##### Data exploration ####

ggplot(joined, aes(opp_team_diff,total_points)) + geom_point() + theme_bw()

#### Build test and train set ####

#remove rows with missing values
df <- remove_missing(df)


#split up sample
sample <- sample.split(df$total_points,SplitRatio = 0.7)
# 70% of data -> training
train <- subset(df,sample==TRUE)
# 30% of data -> test
test <- subset(df,sample==FALSE)

## TRAIN AND BUILD A MODEL
str(train)

model <- lm(total_points ~ was_home + xa_avg + xg_avg + prev_xg + team_diff + opp_team_diff + cum_goals + prev_goals, data=train)

print(summary(model))

#Analysing residuals
res <- residuals(model)
class(res)
res <- as.data.frame(res)
head(res)

#plotting residuals
ggplot(res,aes(res)) + geom_histogram(fill='blue',alpha=0.5)

plot(model)

# Good explanation of these residuals graphs at - https://data.library.virginia.edu/diagnostic-plots/

# Part 3 - PREDICTIONS

#use model to predict on test set
total_points.predictions <- predict(model,test)

#get results and combine with actuals
results <- cbind(total_points.predictions,test$total_points)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
print(head(results))

results <- remove_missing(results)

# Take care of negative values
to_zero <- function(x){
  if (x<0){
    return(0)
  }else{
    return(x)
  }
}

#Apply zero function
results$predicted <- sapply(results$predicted,to_zero)

# Mean squared error
mse <- mean( (results$actual - results$predicted)^2)
print("MSE")
print(mse)


#RMSE
print("Squared root of MSE")
print(mse^0.5)

library(ggplot2)

ggplot(results, aes(actual,predicted)) + geom_point() + theme_bw() + geom_abline(slope=1, intercept = 0) + ylim(min=0,max=15)
