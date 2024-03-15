#Shannon McWaters -  september 2022. Empirical Chapter Data analysis#

library(dplyr)
library(data.table)
library(lme4)
library(ggplot2)

#Import#
RawData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Risk-sensitivity/main/Risk%20sensitivity%20raw%20data%20.csv")
RawData = na.omit(RawData)
#plots of proportion of visits to variable flowers against age and size#
plot(RawData$PropRisky,RawData$DaysOld)
plot(RawData$PropRisky,RawData$Size)

#Linear Regression with proportion of choices for each trip for each bee#
summary(glm(Size ~ReproductiveStage, data=RawData))
#AgeSize=residuals(glm(Size~DaysOld,data=RawData))
ProportionModel = glm(PropRisky~ ReproductiveStage + Size, data = RawData)
summary(ProportionModel)


## next step is to make the separated data ##

Choices = strsplit(as.character(RawData$FSequence), split=",")
ColonyID = rep(RawData$Colony, sapply(Choice, length))
BeeID = rep(RawData$Individual, sapply(Choice, length))
ColonyStage = rep(RawData$ReproductiveStage, sapply(Choice, length))
Trip = rep(RawData$Trip, sapply(Choice, length))
NectarValue = as.numeric(unlist(strsplit(as.character(RawData$NectarValue), split=",")))
NumChoices = rep(RawData$NumChoices, sapply(Choice, length))
NumRisky = rep(RawData$NumRisky, sapply(Choice, length))
Age = rep(RawData$DaysOld, sapply(Choice, length))
Size =  rep(RawData$Size, sapply(Choice, length))
ChoiceNumber= as.numeric(unlist(strsplit(as.character(RawData$ChoiceNumber), split=",")))

#now unlist choice#
Choice = unlist(Choice)
Choice = ifelse(Choice[]=="S",1,0)

#Create Separated data frame#

RawDataNew = cbind.data.frame(ColonyID,BeeID,ColonyStage,Trip,ChoiceNumber,NectarValue,NumChoices,NumRisky,Age,Choice)


#Now to calculate the current perceived value of the variable/risky flower for each choice#
RiskyNum= vector()
RiskyVal= vector()
RiskValuePerception = vector()
RelativeRiskyValPerception = vector()

for(i in 1:length(BeeID)){
  if(ChoiceNumber[i]== 1 & Choice[i] == "S"){
    RiskyNum[i] = 0
    RiskyVal[i] = 0
  }
  else if(ChoiceNumber[i]== 1 & Choice[i] == "R"){
    RiskyVal[i]= NectarValue[i]
    RiskyNum[i]= 1
    }
  else if(Choice[i] == "S"){
   RiskyVal[i] = RiskyVal[i-1] 
   RiskyNum[i] = RiskyNum[i-1]
  }
  else{
    RiskyVal[i] = RiskyVal[i-1] + NectarValue[i]
    RiskyNum[i] = RiskyNum[i-1] + 1
  }
 RiskValuePerception[i] = RiskyVal[i] / RiskyNum[i]
 RelativeRiskyValPerception[i] = 1.25 - RiskValuePerception[i] 
}

#Logistic Regression for single choices#
summary(glm(Size ~ Age))
AgeSize2 = residuals(glm(Age ~ Size))
SingleChoiceModel = glm(Choice ~ AgeSize2 + RelativeRiskyValPerception + BeeID , data = RawDataNew, family= "binomial") #logistic regression
summary(SingleChoiceModel)

#plot size and stage
plot = ggplot(RawData, aes(x=Size, y=PropRisky, color = ReproductiveStage)) +
  geom_point() +
  labs(x="Number of individuals in colony", y="Proportion risk-prone choices")+
  scale_color_discrete(labels = c("Male & Gyne production", "Worker production"),
                    name = "")
###########################################################################################################################################################
