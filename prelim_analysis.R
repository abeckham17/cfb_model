library(faraway)
library(MASS)

get_prop <- function(num){
  numer <- exp(num)
  denom <- 1 + exp(num)
  numer/denom
}

get_max_loc <- function(vec){
  i <- 0
  max <- vec[0]
  for (elem in vec){
    if (elem > max){
      max <- elem
      i <- i
    }
    i <- i + 1
  }
  i
}
get_min_loc <- function(vec){
  i <- 0
  min <- vec[0]
  for (elem in vec){
    if (elem < min){
      max <- elem
      i <- i
    }
    i <- i + 1
  }
  i
}

cfb_improved <- read.csv("~/Documents/fall_2020_classes/stat_methods/final_project/cfb_improved.csv")
# this model was designed to incorporate stats that were context independents
rates_model <- glm(win_percent ~ Off.Yards.Play + Yards.Play.Allowed + Avg.Turnover.Margin.per.Game + 
                     avg_top + X4th.Percent + Opponent.4th.Percent + Avg.Yards.per.Kickoff.Return.Allowed + 
                     Avg.Yard.per.Kickoff.Return + Pass.Yards.Attempt + Yards.Attempt.Allowed + 
                     Penalty.Yards.Per.Game + Avg.Yards.Per.Punt.Return + 
                     Avg.Yards.Allowed.per.Punt.Return + Yds.Rush.Allowed + Yards.Rush + 
                     Average.Sacks.per.Game + X3rd.Percent +  fumbles_rec_game + 
                     opponents_int_game + fumbles_lost_game + int_thrown_game + 
                     Off.Yards.Play * Pass.Yards.Attempt + Off.Yards.Play * Yards.Rush + 
                     Yards.Rush * Pass.Yards.Attempt + Yards.Play.Allowed * Yards.Attempt.Allowed + 
                     Yards.Play.Allowed * Yds.Rush.Allowed + Yards.Attempt.Allowed * Yds.Rush.Allowed, 
                   family = "binomial", weights = Games, data = cfb_improved)
summary(rates_model)

AICmodel <- stepAIC(rates_model)
summary(AICmodel)

par(mfrow=c(1,2))
plot(AICmodel$fitted.values, AICmodel$residuals, main = "Residual Plot", 
     xlab = "Fitted Values", ylab = "Residuals", sub = "Figure 1")
hist(AICmodel$residuals, main = "Histogram of Residuals", xlab = "Residual Values", breaks = 9, sub= "Figure 2")
qqnorm(get_prop(AICmodel$residuals))
abline()

par(mfrow=c(1,2))
plot(cfb_improved$Off.Yards.Play, cfb_improved$win_percent, xlab = "Offensive Yards Per Play", ylab = "Win Percentage", sub = "Figure 3")
plot(cfb_improved$Yards.Play.Allowed, cfb_improved$win_percent, xlab = "Yards Per Play Allowed", ylab = "Win Percentage", sub = "Figure 4")

plot(cfb_improved$Avg.Turnover.Margin.per.Game, cfb_improved$win_percent, xlab = "Average Turnover Margin Per Game", ylab = "Win Percentage", sub = "Figure 5")
plot(cfb_improved$Opponent.4th.Percent, cfb_improved$win_percent, xlab = "Opponent 4th Down Percentage", ylab = "Win Percentage", sub = "Figure 6")

plot(cfb_improved$Pass.Yards.Attempt, cfb_improved$win_percent, xlab = "Pass Yards Per Attempt", ylab = "Win Percentage", sub = "Figure 7")
plot(cfb_improved$Average.Sacks.per.Game , cfb_improved$win_percent, xlab = "Average Sacks Per Game", ylab = "Win Percentage", sub = "Figure 8")

par(mfrow=c(1,2))
plot(cfb_improved$X3rd.Percent, cfb_improved$win_percent, xlab = "3rd Down Conversion Rate", ylab = "Win Percentage", sub = "Figure 9")
plot(cfb_improved$fumbles_rec_game, cfb_improved$win_percent, xlab = "Avg Fumbles Recovered Per Game", ylab = "Win Percentage", sub = "Figure 10")

plot(cfb_improved$opponents_int_game, cfb_improved$win_percent, xlab = "Avg Defensive Interceptions Per Game", ylab = "Win Percentage", sub = "Figure 11")
plot(cfb_improved$fumbles_lost_game, cfb_improved$win_percent, xlab = "Avg Fumbles Lost Per Game", ylab = "Win Percentage", sub = "Figure 12")

plot(cfb_improved$int_thrown_game, cfb_improved$win_percent, xlab = "Avg Interceptions Thrown Per Game", ylab = "Win Percentage", sub = "Figure 13")
plot(cfb_improved$Yds.Rush, cfb_improved$win_percent, xlab = "Yards Per Rush Allowed", ylab = "Win Percentage", sub = "Figure 14")

plot(get_prop(predict(AICmodel)), cfb_improved$win_percent, main = "Model Accuracy",
     xlab = "Predicted Win Percentage", ylab = "Actual Win Percentage", sub = "Figure 15")
abline(a=0, b=1)









