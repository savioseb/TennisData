## Libraries used in the Project
library(ggplot2)
library(reshape2)
library(scales)
library(lubridate)
library(e1071)
## to be able to plot in grids
library(grid)
## to be able to plot in grids
library(gridExtra)


tennisData <- read.csv( "Data/Reformatted.csv")

## building a logistic model to predict the result
## of a tennis match
## creating the Logistic model using all the independent variables
## available in the dataset
WinLogModelAll <- glm( 
  formula =  Result ~ . , 
  family = "binomial" , 
  data = tennisData )


summary(WinLogModelAll)


## Getting the predictions of the Logistic regression
## it is just too perfect!
predl <- predict( WinLogModelAll , type = "response" )

tennisData$predl <- predl


## Using Step backwards to find a more suitable model
backwards = step(WinLogModelAll) 

## Summary
summary(backwards)

## Model is still too perfect.
model <- glm( Result ~ firstServePointsWon. + secondServePointsWon. + breakPointsWon. + returnPointsWon. , family = "binomial" , 
              data = tennisData )

## Model is still too perfect.
model <- glm( Result ~ firstServePointsWon. + secondServePointsWon. + returnPointsWon. , family = "binomial" , 
              data = tennisData )

predl <- predict( model , type = "response" )

tennisData$predl <- predl

acesModel <- lm( aces ~ . -predl - Result  , data=tennisData)

## Using stepwise back to build model
acesModel2 <- step(acesModel)

## The model that was built:
acesModel3 <- lm( aces ~ Tournament + Match + firstServeIn. + doubleFaults + 
                    firstServePointsWon. + returnPointsWon., data = tennisData)




