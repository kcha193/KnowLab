

rm(list = ls())
.rs.restartR()

#devtools::install_github("kcha193/simarioV2")


detach("simframe")

library(tidyverse)
library(simarioV2)
# library(profvis)

setwd("C:\\Users\\kcha193\\workspace\\KnowLab")

NUM_ITERATIONS <- 21


source("initKnowLab.R")
initialSim <- initSim(NUM_ITERATIONS)

saveRDS(initialSim, "base/initialSim.rds")
saveRDS(initialSim, "../KnowLabShiny/base/initialSim.rds")

Simenv <- createSimenv("Base", initialSim$simframe, 
                       initialSim$dict, "years1_21")

#source("SimmoduleMELC1_21.R")
source("simulateKnowLab.R")

env.base <- simulateSimario(Simenv, 10, simulateKnowLab)

# p <-
#   profvis({
#    env.base <- simulateSimario(Simenv, 2, simulateKnowLab, parallel = FALSE)
#  })

saveRDS(env.base, "base/FullBaseRun.rds")
saveRDS(env.base, "../KnowLabShiny/base/FullBaseRun.rds")

.rs.restartR()

#####################################################################################

table(env.base$simframe$BREAST)


tableBuilderNew(env.base, statistic = "freq", "z1BullyLvl1", 
                logisetexpr = "BREAST == 1")

tableBuilderNew(env.base, statistic = "freq", "BREAST", 
                logisetexpr = "BREAST == 1")

tableBuilderNew(env.base, statistic = "freq", "BREAST", 
                logisetexpr = "BREAST == 2")

tableBuilderNew(env.base, statistic = "freq", "BREAST", 
                logisetexpr = "BREAST == 1")


#####################################################################################
tableBuilderNew(env.base, statistic = "freq", "z1BullyLvl1")
tableBuilderNew(env.base, statistic = "freq", "z1BullyLvl1", grpbyName = "z1genderLvl1")
tableBuilderNew(env.base, statistic = "freq", "z1BullyLvl1", grpbyName = "r1stchildethn")

tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1")
tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1", grpbyName = "z1genderLvl1")
tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1", grpbyName = "r1stchildethn")

tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1")
tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1", grpbyName = "z1genderLvl1")
tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1", grpbyName = "r1stchildethn")

##########################################################################################

tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1")%>% 
  filter(Var == "Overweight") 

tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1") %>% 
  filter(Var == "Obese")


tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", grpbyName = "r1stchildethn") %>% 
  filter(Var == "Overweight") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)


tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1", grpbyName = "r1stchildethn") %>% 
  filter(Var == "Obese") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)




tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", grpbyName = "z1genderLvl1") %>% 
  filter(Var == "Overweight") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)


tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1", grpbyName = "z1genderLvl1") %>% 
  filter(Var == "Obese") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)



tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", grpbyName = "r1stchildethn",
                logisetexpr = "z1genderLvl1 == 1") %>% 
  filter(Var == "Overweight") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "mean", "BMI")

tableBuilderNew(env.base, statistic = "mean", "BMI", grpbyName = "z1genderLvl1") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "mean", "BMI", grpbyName = "r1stchildethn") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)


##########################################################################################

tableBuilderNew(env.base, statistic = "mean", "IQ")

tableBuilderNew(env.base, statistic = "freq", "IQ")


tableBuilderNew(env.base, statistic = "mean", "IQ", grpbyName = "SESBTH")


tableBuilderNew(env.base, statistic = "freq", "SESBTH", grpbyName = "r1Region") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

##########################################################################################

tableBuilderNew(env.base, statistic = "freq", "r1Score")

tableBuilderNew(env.base, statistic = "freq", "z1ScoreLvl1") %>% 
  filter(Var == "Passed" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1DropLvl1") %>% 
  filter(Var == "Dropout" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1FailLvl1") %>% 
  filter(Var == "Failed" & Year == 17)


tableBuilderNew(env.base, statistic = "freq", "z1ScoreLvl1", grpbyName = "z1genderLvl1") %>% 
  filter(Var == "Passed" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1DropLvl1", grpbyName = "z1genderLvl1")%>% 
  filter(Var == "Dropout" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1FailLvl1", grpbyName = "z1genderLvl1")%>% 
  filter(Var == "Failed" & Year == 17)



tableBuilderNew(env.base, statistic = "freq", "z1ScoreLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 1")%>% 
  filter(Var == "Passed" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1DropLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 1")%>% 
  filter(Var == "Dropout" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1FailLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 1")%>% 
  filter(Var == "Failed" & Year == 17)


tableBuilderNew(env.base, statistic = "freq", "z1ScoreLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 2")%>% 
  filter(Var == "Passed" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1DropLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 2")%>% 
  filter(Var == "Dropout" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1FailLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 2")%>% 
  filter(Var == "Failed" & Year == 17)


tableBuilderNew(env.base, statistic = "freq", "z1ScoreLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 4")%>% 
  filter(Var == "Passed" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1DropLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 4")%>% 
  filter(Var == "Dropout" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1FailLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 4")%>% 
  filter(Var == "Failed" & Year == 17)


tableBuilderNew(env.base, statistic = "freq", "z1ScoreLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 3")%>% 
  filter(Var == "Passed" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1DropLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 3")%>% 
  filter(Var == "Dropout" & Year == 17)
tableBuilderNew(env.base, statistic = "freq", "z1FailLvl1",
                grpbyName = "z1genderLvl1",
                logisetexpr = "r1stchildethn == 3")%>% 
  filter(Var == "Failed" & Year == 17)







library(ggplot2)
library(plotly)

tableBuilderNew(env.base, statistic = "freq", "z1NEETLvl1") %>% 
  filter(Var == "NEET") %>% 
  ggplot(aes(x = Year, y = Mean)) + geom_bar( stat = "identity", pos = "dodge")


tableBuilderNew(env.base, statistic = "freq", "z1NEETLvl1", grpbyName = "z1genderLvl1") %>% 
  filter(Var == "NEET") %>% 
  ggplot(aes(x = Year, y = Mean, fill = groupByData)) + geom_bar( stat = "identity", pos = "dodge")

tableBuilderNew(env.base, statistic = "freq", "z1NEETLvl1", grpbyName = "z1genderLvl1")%>% 
  filter(Var == "NEET") %>% 
  ggplot(aes(x = Year, y = Mean, col = groupByData)) + geom_path() + geom_point()

tableBuilderNew(env.base, statistic = "freq", "z1NEETLvl1", grpbyName = "r1stchildethn")%>% 
  filter(Var == "NEET") %>% 
  ggplot(aes(x = Year, y = Mean, fill = groupByData)) + geom_bar( stat = "identity", pos = "dodge")

tableBuilderNew(env.base, statistic = "freq", "z1NEETLvl1", grpbyName = "r1stchildethn")%>% 
  filter(Var == "NEET") %>% 
  ggplot(aes(x = Year, y = Mean, col = groupByData)) + geom_path() + geom_point()

p <- 
  tableBuilderNew(env.base, statistic = "freq", "z1NEETLvl1", grpbyName = "r1Region")%>% 
  filter(Var == "NEET") %>% 
  ggplot(aes(x = Year, y = Mean, fill = groupByData)) + geom_bar( stat = "identity", pos = "dodge")

ggplotly(p)

tableBuilderNew(env.base, statistic = "freq", "r1Region")



tableBuilderNew(env.base, statistic = "freq", "z1ScoreLvl1", grpbyName = "r1stchildethn")



tableBuilderNew(env.base, statistic = "freq", "z1ScoreLvl1", grpbyName = "z1genderLvl1")

##########################################################################################

tableBuilderNew(env.base, statistic = "mean", "IQ")

tableBuilderNew(env.base, statistic = "mean", "IQ", grpbyName = "SESBTH")


tableBuilderNew(env.base, statistic = "freq", "SESBTH", grpbyName = "r1Region") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

##########################################################################################

tableBuilderNew(env.base, statistic = "freq", "r1Region")

tableBuilderNew(env.base, statistic = "freq", "r1Region", grpbyName = "SESBTH") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)


tableBuilderNew(env.base, statistic = "freq", "SESBTH", grpbyName = "r1Region") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

##########################################################################################


tableBuilderNew(env.base, statistic = "freq", "MAGE")


tableBuilderNew(env.base, statistic = "qu", "MAGE")


tableBuilderNew(env.base, statistic = "mean", "BMI")

tableBuilderNew(env.base, statistic = "mean", "BMI", grpbyName = "z1genderLvl1")







tableBuilderNew(env.base, statistic = "freq", "z1ParentAlcLvl1")
tableBuilderNew(env.base, statistic = "freq", "z1ParentDepressLvl1")





tableBuilderNew(env.base,statistic = "freq", "z1WatchTVLvl1")%>% 
  select(-Lower, -Upper)  %>%  spread(Var, Mean)


tableBuilderNew(env.base,statistic = "freq", "chres")%>% 
  select(-Lower, -Upper)  %>%  spread(Var, Mean)

tableBuilderNew(env.base,statistic = "freq", "mhrswrk", CI = FALSE)%>% 
  select(-Lower, -Upper)  %>%  spread(Var, Mean)

tableBuilderNew(env.base,statistic = "freq", "fhrswrk", CI = FALSE)%>% 
  select(-Lower, -Upper)  %>%  spread(Var, Mean)

tableBuilderNew(env.base,statistic = "mean", "fhrswrk")

tableBuilderNew(env.base,statistic = "freq", "z1ParentAlcLvl1")
tableBuilderNew(env.base,statistic = "freq", "r1SchoolFundingLvl1")

tableBuilderNew(env.base,statistic = "freq", "z1accomLvl1")

tableBuilderNew(env.base, statistic = "freq", "kids") 

tableBuilderNew(env.base, statistic = "mean", "BMI")%>% select(-Lower, -Upper)  %>%  
  left_join(tableBuilderNew(env.base, statistic = "mean", "BMI", grpbyName = "z1genderLvl1") %>% 
              select(-Lower, -Upper) %>%  spread(groupByData, Mean))


tableBuilderNew(env.base, statistic = "mean", "BMI", grpbyName = "r1stchildethn")  %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)


tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1") %>% filter(Var == "Overweight")

tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", grpbyName = "z1genderLvl1") %>% 
  filter(Var == "Overweight") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "freq", "z1OverweightBMILvl1", grpbyName = "z1genderLvl1") %>% 
  filter(Var == "Overweight") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)


tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1", grpbyName = "z1genderLvl1") %>% 
  filter(Var == "Obese") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)





tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1", grpbyName = "r1stchildethn") %>% 
  filter(Var == "Obese") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)



tableBuilderNew(env.base, statistic = "freq", "z1OverweightBMILvl1") %>% filter(Var == "Overweight")

tableBuilderNew(env.base, statistic = "freq", "z1OverweightBMILvl1", grpbyName = "r1stchildethn") %>% 
  filter(Var == "Overweight") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)


tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1") %>% filter(Var == "Obese")

tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1", grpbyName = "z1genderLvl1")
tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1", grpbyName = "r1stchildethn")


  filter(Var == "Overweight")

tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", 
                grpbyName = "r1stchildethn") %>% 
  filter(Var == "Overweight")


tableBuilderNew(env.base, statistic = "mean", "BMI")


tableBuilderNew(env.base, statistic = "mean", "BMI", grpbyName = "z1genderLvl1") 

tableBuilderNew(env.base, statistic = "mean", "BMI", grpbyName = "z1genderLvl1") 



tableBuilderNew(env.base, statistic = "freq", "z1OverweightBMILvl1") %>% 
  filter(Var == "Overweight")


tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1") %>% 
  filter(Var == "Obese")


tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1")  %>% 
  filter(Var == "Overweight")


tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", grpbyName = "z1genderLvl1") %>% 
  filter(Var == "Overweight")


tableBuilderNew(env.base, statistic = "freq", "z1OverweightBMILvl1", grpbyName = "z1genderLvl1") %>% 
  filter(Var == "Overweight")


tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1") %>% 
  filter(Var == "Obese")


tableBuilderNew(env.base, statistic = "mean", "BMI")

tableBuilderNew(env.base, statistic = "mean", "burt")


temp <- tableBuilderNew(env.base, statistic = "freq", "r1School")
temp%>% filter(Year == 1)


tableBuilderNew(env.base, statistic = "freq", "r1Sleep")



#####################################################################################
#BWKG
Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")
tableBuilderNew(env.base, "freq", "bwkg")
Simenv.scenario$cat.adjustments$bwkg[1,] <- c(0, 0, 0, 0, 1)	

Simenv.scenario <- simulateSimario(Simenv.scenario, 2, simulateKnowLab)

tableBuilderNew(Simenv.scenario, "freq", "bwkg")

tableBuilderNew(env.base, "mean", "bwkg")
tableBuilderNew(Simenv.scenario, "mean", "bwkg")



#################################################################################################
#SES

Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

tableBuilderNew(env.base, "freq", "SESBTH")

Simenv.scenario$cat.adjustments$SESBTH[1,] <- c(1, 0, 0)	

Simenv.scenario <- simulateSimario(Simenv.scenario, 10, simulateKnowLab)



tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1AlcAbuseLvl1")


tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1DepressLvl1")



tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1", 
                logisetexpr = "(r1stchildethn == 1 | r1stchildethn == 3)")


#################################################################################################
#ADHD
Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

tableBuilderNew(env.base, "freq", "z1ADHDLvl1")

Simenv.scenario$cat.adjustments$z1ADHD[1,] <- c(1, 0)	

Simenv.scenario <- simulateSimario(Simenv.scenario, 10, simulateKnowLab)

tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1AlcAbuseLvl1")

tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1", logisetexpr = "z1ADHDLvl1 == 1" )
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1AlcAbuseLvl1", logisetexpr = "z1ADHDLvl1 == 1" )

tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1",
                logisetexpr = "z1ADHDLvl1 == 1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1AlcAbuseLvl1",
                logisetexpr = "z1ADHDLvl1 == 1", envBase = env.base, basePop = TRUE )


#################################################################################################

tableBuilderNew(env.base, statistic = "freq", "z1INTERACTLvl1")
tableBuilderNew(env.base, statistic = "freq", "z1PUNISHLvl1")


#################################################################################################
#PUNISH 
Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")
Simenv.scenario$cat.adjustments$z1PUNISH[5,1] <- 0.7
Simenv.scenario$cat.adjustments$z1PUNISH[5,2] <- 0.3

Simenv.scenario <- simulateSimario(Simenv.scenario, 2, simulateKnowLab)

tableBuilderNew(Simenv.scenario, statistic = "freq", "z1PUNISHLvl1")

tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1AlcAbuseLvl1")

tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1DepressLvl1")

#################################################################################################
#INTERACT 
Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

Simenv.scenario$cat.adjustments$z1INTERACT[5,1] <- 0
Simenv.scenario$cat.adjustments$z1INTERACT[5,2] <- 1

Simenv.scenario <- simulateSimario(Simenv.scenario, 2, simulateKnowLab)

tableBuilderNew(Simenv.scenario, statistic = "freq", "z1INTERACTLvl1")

tableBuilderNew(env.base, statistic = "freq", "z1INTERACTLvl1")

tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1AlcAbuseLvl1")

tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1DepressLvl1")


tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1", logisetexpr = "z1INTERACTLvl1 == 0" )
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1DepressLvl1", logisetexpr = "z1INTERACTLvl1 == 0" )

tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1", 
                logisetexpr = "z1INTERACTLvl1 == 0", envBase = env.base, basePop = TRUE )
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1DepressLvl1", 
                logisetexpr = "z1INTERACTLvl1 == 0", envBase = env.base, basePop = TRUE )


#################################################################################################
#Bulley 

Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

tableBuilderNew(env.base, statistic = "freq", "z1BullyLvl1")

Simenv.scenario$cat.adjustments$z1Bully[16,1]<-1
Simenv.scenario$cat.adjustments$z1Bully[16,2]<-0
	
Simenv.scenario <- simulateSimario(Simenv.scenario, 2, simulateKnowLab)


tableBuilderNew(env.base, statistic = "freq", "z1BullyLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1BullyLvl1")


tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1AlcAbuseLvl1")

tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1DepressLvl1")

tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1", 
                logisetexpr = "z1BullyLvl1 == 1")
tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1", 
                logisetexpr = "z1BullyLvl1 == 0")


tableBuilderNew(Simenv.scenario, statistic = "freq", "z1DepressLvl1", 
                logisetexpr = "z1BullyLvl1 == 1")

tableBuilderNew(env.base, statistic = "freq", 
                "z1DepressLvl1", envBase = env.base, basePop = TRUE, 
                logisetexpr = "z1BullyLvl1 == 1")


tableBuilderNew(Simenv.scenario, statistic = "freq", 
                "z1DepressLvl1", envBase = env.base, basePop = TRUE, 
                logisetexpr = "z1BullyLvl1 == 1")
tableBuilderNew(Simenv.scenario, statistic = "freq", 
                "z1DepressLvl1", envBase = env.base, basePop = TRUE, 
                logisetexpr = "z1BullyLvl1 == 0")

####################################################################################
#Parental alcohol abuse to 14%

source("SimmoduleMELC1_21.R")

Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

tableBuilderNew(env.base, statistic = "freq", "z1ParentAlcLvl1")%>% filter(Var == "Yes")

Simenv.scenario$cat.adjustments$z1ParentAlc[15,] <- c(0.9, 0.1)


Simenv.scenario <- simulateSimario(Simenv.scenario, 10, simulateKnowLab)

tableBuilderNew(Simenv.scenario, statistic = "freq", "z1ParentAlcLvl1") %>% filter(Var == "Yes")


tableBuilderNew(env.base, statistic = "freq", "z1AlcAbuseLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1AlcAbuseLvl1")


####################################################################################
#Parental depression to 14%
Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

tableBuilderNew(env.base, statistic = "freq", "z1ParentDepressLvl1")

Simenv.scenario$cat.adjustments$z1ParentDepress[15,] <- c(0.9,  0.1)

Simenv.scenario <- simulateSimario(Simenv.scenario, 10, simulateKnowLab) %>% filter(Var == "Yes")

tableBuilderNew(env.base, statistic = "freq", "z1DepressLvl1")
tableBuilderNew(Simenv.scenario, statistic = "freq", "z1DepressLvl1")





#test <- tableBuilder(env.base, "means", "IQ")

tableBuilderNew(env.base, statistic = "frequencies", "bwkg")
tableBuilderNew(env.base, statistic = "means", "bwkg")

tableBuilderNew(env.base, "means", "IQ")

tableBuilderNew(env.base, "frequencies", "z1OverweightLvl1")

tableBuilderNew(env.base, "frequencies", "z1accomLvl1")

test <- tableBuilder(env.base, "frequencies", "r1School", grpbyName = "r1stchildethnLvl1", CI = FALSE)

test <- tableBuilder(env.base, "frequencies", "z1genderLvl1", grpbyName = "r1School" , CI = FALSE)
genderSchool = data.frame(t(sapply(strsplit(colnames(test), " "), function(x) x)), Freq = test[1,])
names(genderSchool)[1:2] = c("School", "Gender")
genderSchool %>% spread(Gender,Freq)%>% write.csv("genderSchool.csv")



test <- tableBuilder(env.base, "frequencies", "r1stchildethn", grpbyName = "r1School" , CI = FALSE)
colnames(test) <- gsub("NZ ", "", colnames(test))
ethnSchool = data.frame(t(sapply(strsplit(colnames(test), " "), function(x) x)), Freq = test[1,])
names(ethnSchool)[1:2] = c("School", "Eth")
ethnSchool %>% spread(Eth,Freq) %>% write.csv("ethnSchool.csv")


test <- tableBuilder(env.base, "frequencies", "SESBTH", grpbyName = "r1School" , CI = FALSE)
SESBSchool = data.frame(t(sapply(strsplit(colnames(test), " "), function(x) x)), Freq = test[1,])
names(SESBSchool)[1:2] = c("School", "SESB")
SESBSchool %>% spread(SESB,Freq) %>% write.csv("SESBSchool.csv")



test <- tableBuilder(env.base, "means", "SESBTH", grpbyName = "r1School" , CI = FALSE)

ggplot(data.frame(Dec = x, ses = test[1,]), aes(Dec, ses, label = colnames(test))) + 
  geom_text(check_overlap = TRUE) + geom_smooth()

SESBSchool = data.frame(t(sapply(strsplit(colnames(test), " "), function(x) x)), Freq = test[1,])
names(SESBSchool)[1:2] = c("School", "SESB")
SESBSchool %>% spread(SESB,Freq) 



temp <- sapply(1:5000, function(i) sample(1:100, 1, prob = transition_probabilities$r1School[i,]))

range(table(temp))

mean(c(12,16,11,15,7,12,11,13,15))
mean(c(135,125,121,124,115,120,128,117,122))


sample(1:10,1, prob = c(0.5, rep(0.5/9,9)))


apply(transition_probabilities$r1School, 1, sum)


range(table(env.base$modules[[1]]$run_results$run1$outcomes$r1School[,1]))




env.base$modules[[1]]$run_results_collated$means$IQ	
env.base$modules[[1]]$run_results_collated$freqs$z1ScoreLvl1

env.base$modules[[1]]$run_results_collated$means$IQ	
env.base$modules[[1]]$run_results_collated$freqs$z1ScoreLvl1



env.base$modules[[1]]$run_results_collated$means$fhrswrk	


test <- tableBuilder(env = env.base, statistic="means", variableName="fhrswrk")

test <- tableBuilder(env.base, "frequencies", "SESBTH", CI = FALSE)
test <- tableBuilder(env.base, statistic="frequencies", variableName="welfareLvl1", 
                     grpbyName="",logisetexpr="z1singleLvl1==0", CI = FALSE)

test <- tableBuilder(env = env.base, statistic="means", variableName="fhrswrk", grpbyName="z1gender")

test <- tableBuilder(env = env.base, statistic="frequencies", 
                     variableName="z1singleLvl1", grpbyName="SESBTHLvl1")
test <- tableBuilder(env = env.base, statistic="frequencies", 
                     variableName="z1singleLvl1", grpbyName="SESBTHLvl2")
test <- tableBuilder(env = env.base, statistic="frequencies", 
                     variableName="z1singleLvl1", grpbyName="z1genderLvl0")

test <- tableBuilder(env = env.base, statistic="frequencies", 
                     variableName="z1singleLvl1", grpbyName="z1genderLvl0")

test <- tableBuilder(env = env.base, statistic="frequencies", variableName="z1accomLvl1", grpbyName="z1genderLvl0")

head(env.base$simframe$z1genderLvl1)
head(env.base$simframe$z1genderLvl0)














