

rm(list = ls())
.rs.restartR()

#devtools::install_github("kcha193/simarioV2")


detach("simframe")

library(tidyverse)
library(simarioV2)
# library(profvis)

#setwd("C:\\Users\\kcha193\\workspace\\KnowLab")

NUM_ITERATIONS <- 21

source("initKnowLab.R")
initialSim <- initSim(NUM_ITERATIONS)

saveRDS(initialSim, "base/initialSim.rds")
saveRDS(initialSim, "../KnowLabShiny/base/initialSim.rds")

Simenv <- createSimenv("Base", initialSim$simframe, 
                       initialSim$dict, "years1_21")


source("simulateKnowLab.R")


env.base <- simulateSimario(Simenv, 10, simulateKnowLab)

# p <-
#   profvis({
#    env.base <- simulateSimario(Simenv, 2, simulateKnowLab, parallel = FALSE)
#  })

saveRDS(env.base, "base/FullBaseRun.rds")
saveRDS(env.base, "../KnowLabShiny/base/FullBaseRun.rds")


env.base <- simulateSimario(Simenv, 50, simulateKnowLab)

saveRDS(env.base, "base/FullBaseRun.rds")


.rs.restartR()

#####################################################################################


prop.table(table(env.base$modules$run_results$run1$z1OverweightLvl1))[1]


env.base10 <- simulateSimario(Simenv, 10, simulateKnowLab)
saveRDS(env.base, "base/FullBaseRun.rds")
env.base20 <- simulateSimario(Simenv, 20, simulateKnowLab)
env.base30 <- simulateSimario(Simenv, 30, simulateKnowLab)
env.base40 <- simulateSimario(Simenv, 40, simulateKnowLab)
env.base50 <- simulateSimario(Simenv, 50, simulateKnowLab)
env.base60 <- simulateSimario(Simenv, 60, simulateKnowLab)
env.base70 <- simulateSimario(Simenv, 70, simulateKnowLab)
env.base80 <- simulateSimario(Simenv, 80, simulateKnowLab)
env.base90 <- simulateSimario(Simenv, 90, simulateKnowLab)
env.base100 <- simulateSimario(Simenv, 100, simulateKnowLab)
env.base110 <- simulateSimario(Simenv, 110, simulateKnowLab)
env.base120 <- simulateSimario(Simenv, 120, simulateKnowLab)
env.base130 <- simulateSimario(Simenv, 130, simulateKnowLab)
env.base140 <- simulateSimario(Simenv, 140, simulateKnowLab)
env.base150 <- simulateSimario(Simenv, 150, simulateKnowLab)
env.base160 <- simulateSimario(Simenv, 160, simulateKnowLab)
env.base170 <- simulateSimario(Simenv, 170, simulateKnowLab)
env.base180 <- simulateSimario(Simenv, 180, simulateKnowLab)
env.base190 <- simulateSimario(Simenv, 190, simulateKnowLab)
env.base200 <- simulateSimario(Simenv, 200, simulateKnowLab)



pop <- NULL

propStore <- NULL

for(i in 1:100){
  print(i)
  pop <- c(pop, 
           env.base100$modules$run_results[[i]]$z1OverweightLvl1)
  
  propStore <- c(propStore, prop.table(table(pop))[2])
}


data.frame(Run = 1:100, 
           Percentage = propStore*100) %>% 
  ggplot(aes(x = Run, y = Percentage)) + 
  geom_path() + geom_point() + 
  theme_bw() + geom_vline(xintercept = 10) + 
  ylim(35, 36) + ylab("Proportion")


ggsave("compare10runsto100runs.png")



tableBuilderNew(env.base, "freq", "z1WatchTVLvl1")

WatchTV.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

WatchTV.scenario$cat.adjustments$z1WatchTVLvl1[1,]=c(0.8, 0.2)

WatchTV.scenario100 <- 
  simulateSimario(WatchTV.scenario, 100, simulateKnowLab)


pop <- NULL

propStoreTV <- NULL

for(i in 1:100){
  print(i)
  pop <- c(pop, 
           WatchTV.scenario100$modules$run_results[[i]]$z1OverweightLvl1)
  
  propStoreTV <- c(propStoreTV, prop.table(table(pop))[2])
}




data.frame(Run = 1:100, 
           Percentage = c(propStore*100, propStoreTV*100), 
           Scenario = rep(c("Base", "WatchTV"), each = 100)) %>% 
  ggplot(aes(x = Run, y = Percentage, col = Scenario)) + 
  geom_path() + geom_point() + 
  theme_bw() + geom_vline(xintercept = 10) + 
  ylab("Proportion")+ 
  ylim(33, 36)

ggsave("compare10runsto100runsWatchTV.png")


91/60

808.69/60


temp <- 
  sapply(env.base100$modules$run_results, 
         function(x) prop.table(table(x$z1OverweightLvl1))[1])



sd(temp[1:2])

seq(1,seq(2,100))




temp <- 
  sapply(env.base100$modules$run_results, 
         function(x) prop.table(table(x$z1OverweightLvl1))[1])




write.csv(cbind(temp,
c(0, sapply(lapply(2:100, function(x) 1:x), function(x) sd(temp[x])))), file = "compareOverweightSD.csv")

library(ggplot2)

tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1") %>% 
  filter(Var == "Overweight") %>% 
  ggplot(aes(x = Year, y = Mean)) + 
  geom_path() + 
  geom_point() + 
  theme_bw()


env.base100 <- simulateSimario(Simenv, 100, simulateKnowLab)


old <- tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1") %>% 
  filter(Var == "Overweight")

new100 <- tableBuilderNew(env.base100, statistic = "freq", "z1OverweightLvl1") %>% 
  filter(Var == "Overweight") 

old$Run = 10
new100$Run = 100

dodge <- position_dodge(width=0.9)



compare <- rbind(old, new100)

compare$Run <- factor(compare$Run)

ggplot(compare, aes(x = Year, y = Mean, col = Run)) + 
         geom_path(position = dodge) + 
         geom_point(position = dodge) + 
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = dodge, width = 0.25) +
  ylab("Percentage") + 
  ggtitle("Base simulation comparing bewteen using 10 runs and 100 runs") + 
         theme_bw() 

ggsave("compare10runsto100runs.png")



tableBuilderNew(env.base, statistic = "freq", "z1RuralLvl1")

tableBuilderNew(env.base, statistic = "freq", "z1RuralLvl1", 
                grpbyName = "r1Region") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "freq", "z1RuralLvl1", 
                grpbyName = "r1stchildethn")%>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "freq", "z1RuralLvl1",
                grpbyName = "r1Region",
                logisetexpr = "r1stchildethn == 1") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "freq", "z1RuralLvl1",
                grpbyName = "r1Region",
                logisetexpr = "r1stchildethn == 2") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "freq", "z1RuralLvl1",
                grpbyName = "r1Region",
                logisetexpr = "r1stchildethn == 3") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "freq", "z1RuralLvl1",
                grpbyName = "r1Region",
                logisetexpr = "r1stchildethn == 4") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)


data.frame(Eth = children$r1stchildethn,
         Region =   env.base$simframe$r1Region,      
      Rural = env.base$modules$run_results$run1$z1RuralLvl1[,1]) %>% 
  filter(Eth == 4, Region == 4)


data.frame(Eth = children$r1stchildethn,
           Region =   env.base$simframe$r1Region,      
           Rural = env.base$modules$run_results$run9$z1RuralLvl1[,1]) %>% 
  filter(Eth == 4, Region == 4)



tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", 
                grpbyName = "r1stchildethn") %>% 
  filter(Var == "Overweight")  %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1", 
                grpbyName = "r1stchildethn") %>% 
  filter(Var == "Obese")  %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)



########################################################################

tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1") %>% 
  filter(Var == "Overweight") 

tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", 
                grpbyName = "z1RuralLvl1",
                logisetexpr = "r1stchildethn == 2") %>% 
  filter(Var == "Overweight") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)

tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", 
                grpbyName = "z1RuralLvl1") %>% 
  filter(Var == "Overweight") %>% 
  select(-Lower, -Upper) %>%  spread(groupByData, Mean)



tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1")%>% 
  filter(Var == "Obese") 

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














