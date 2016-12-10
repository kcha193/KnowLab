

rm(list = ls())

#devtools::install_github("kcha193/simarioV2")

library(simarioV2)


setwd("C:\\Users\\kcha193\\workspace\\KnowLab")

initialSim <<- readRDS("base/initialSim.rds")
env.base  <<- readRDS("base/FullBaseRun.rds")


NUM_ITERATIONS <<- 21
dict <<- initialSim$dict
limits <<- initialSim$limits
binbreaks <<- initialSim$binbreaks
catToContModels <<- initialSim$catToContModels
models <<- initialSim$models
PropensityModels <<- initialSim$PropensityModels
children <<- initialSim$children
transition_probabilities <<- initialSim$transition_probabilities

tableBuilderNew(env.base, "freq", "z1ScoreLvl1")
tableBuilderNew(env.base, "freq", "z1OverweightLvl1")

tableBuilderNew(env.base, "freq", "z1ScoreLvl1", logisetexpr = "z1ECELvl1 == 0")
tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", grpbyName = "r1stchildethn")


source("SimmoduleMELC1_21.R")

Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")
#Simenv.scenario$cat.adjustments$z1ECELvl1[1,] <- c(0,1)	

tableBuilderNew(env.base, "mean", "IQ", logisetexpr = "pregalc == 0")

temp <- data.frame(env.base$modules$run_results$run1$IQ, env.base$simframe$pregalc)

mean(env.base$modules$run_results$run1$IQ[env.base$simframe$pregalc == 0,1])



Simenv.scenario$cat.adjustments$r1mBMI[1,] <- c(0.77, 0.02, 0.135, 0.075)	

tableBuilderNew(env.base, "freq", "z1BreakfastLvl1")

Simenv.scenario$cat.adjustments$z1Breakfast[1,] <- c(0, 1)


# subgroupExpression <- "mhrswrk<21"	
# Simenv.scenario <- setGlobalSubgroupFilterExpression(Simenv.scenario, subgroupExpression)

# Initiate cluster

Simenv.scenario <- simulateSimario(Simenv.scenario, 10, simulateKnowLab)

#Simenv.scenario <- simulateNP(Simenv.scenario, 4)

##########################################################################################
Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

tableBuilderNew(env.base, statistic = "freq", "z1WatchTVLvl1") 

Simenv.scenario$cat.adjustments$z1WatchTV[3,] <- c(0.8, 0.2)	

Simenv.scenario <- simulateSimario(Simenv.scenario, 10, simulateKnowLab)

tableBuilderNew(env.base, statistic = "mean", "BMI")

tableBuilderNew(Simenv.scenario, statistic = "mean", "BMI")

##########################################################################################
Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

tableBuilderNew(env.base, statistic = "freq", "SESBTH") 

Simenv.scenario$cat.adjustments$SESBTH[1,] <- c(0.90, 0.05,0.05)	

Simenv.scenario <- simulateSimario(Simenv.scenario, 10, simulateKnowLab)


tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1")%>% 
  filter(Var == "Overweight") 

tableBuilderNew(Simenv.scenario, statistic = "freq", "z1OverweightLvl1")%>% 
  filter(Var == "Overweight") 



tableBuilderNew(env.base, statistic = "freq", "z1ObeseLvl1") %>% 
  filter(Var == "Obese")

tableBuilderNew(Simenv.scenario, statistic = "freq", "z1ObeseLvl1")%>% 
  filter(Var == "Obese") 


##########################################################################################


Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

tableBuilderNew(env.base, statistic = "freq", "z1ScoreLvl1") %>% 
  filter(Year == 17)

Simenv.scenario$cat.adjustments$z1Score[17,] <- c(0 ,1)

Simenv.scenario <- simulateSimario(Simenv.scenario, 10, simulateKnowLab)

tableBuilderNew(Simenv.scenario, statistic = "freq", "z1ScoreLvl1") %>% 
  filter(Year == 17)

tableBuilderNew(env.base, statistic = "freq", "z1NEETLvl1") %>% 
  filter(Var == "NEET")

tableBuilderNew(Simenv.scenario, statistic = "freq", "z1NEETLvl1") %>% 
  filter(Var == "NEET")

#################################################################################################

tableBuilderNew(env.base, "freq", "pregalc")
tableBuilderNew(env.base, "freq", "z1DrinkLvl1")

Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

Simenv.scenario$cat.adjustments$pregalc[1,] <- c(0.9, 0.1,0,0,0,0,0,0,0)	

Simenv.scenario <- simulateSimario(Simenv.scenario, 2, simulateKnowLab)

tableBuilderNew(Simenv.scenario, "freq", "pregalc")
tableBuilderNew(Simenv.scenario, "freq", "z1DrinkLvl1")


###################################################################################################

length(env.base$modules$run_results$run1)

index <- 
mapply(all.equal, env.base$modules$run_results$run1, 
       Simenv.scenario$modules$run_results$run1) != "TRUE"

Simenv.scenario$modules$run_results <- 
  lapply(Simenv.scenario$modules$run_results, function(x) x[index])
  

index <-
!names(env.base$modules$run_results$run1) %in%
  names(Simenv.scenario$modules$run_results$run1)  

combineSimario <-
  function(base, scenario, index){
    for(i in 1:length(base))
      scenario[[i]]<-  c(base[[i]][index], scenario[[i]])
    
    scenario
  }
  
    
Simenv.scenario$modules$run_results <- 
  combineSimario(env.base$modules$run_results, 
         Simenv.scenario$modules$run_results, index)


tableBuilderNew(env.base, "freq", "z1OverweightLvl1") 

tableBuilderNew(Simenv.scenario, "freq", "z1OverweightLvl1", envBase = env.base) 

tableBuilderNew(Simenv.scenario, "freq", "z1ScoreLvl1", envBase = env.base) 

##################################################################################################
library(xlsx)

tableBuilderNew(env = env.base, "freq", "z1OverweightLvl1") %>% filter(Var == "Overweight") %>% 
  write.xlsx("z1BreakfastScenario.xlsx", sheetName = "Base", row.names = FALSE)

tableBuilderNew(Simenv.scenario, "freq", "z1OverweightLvl1") %>% filter(Var == "Overweight")%>% 
  write.xlsx("z1BreakfastScenario.xlsx", sheetName = "Scenario", row.names = FALSE, append = TRUE)


tableBuilderNew(env.base, "freq", "z1OverweightLvl1", logisetexpr = "r1stchildethn == 2") %>% 
  filter(Var == "Overweight") %>% 
  write.xlsx("z1BreakfastScenario.xlsx", sheetName = "BaseMaori", row.names = FALSE, append = TRUE)

tableBuilderNew(Simenv.scenario, "freq", "z1OverweightLvl1", logisetexpr = "r1stchildethn == 2") %>% 
  filter(Var == "Overweight") %>% 
  write.xlsx("z1BreakfastScenario.xlsx", sheetName = "ScenarioMaori", row.names = FALSE, append = TRUE)


tableBuilderNew(env.base, "freq", "z1OverweightLvl1", logisetexpr = "r1stchildethn == 3") %>% 
  filter(Var == "Overweight")%>% 
  write.xlsx("z1BreakfastScenario.xlsx", sheetName = "BasePacific", row.names = FALSE, append = TRUE)


tableBuilderNew(Simenv.scenario, "freq", "z1OverweightLvl1", logisetexpr = "r1stchildethn == 3") %>% 
  filter(Var == "Overweight")%>% 
  write.xlsx("z1BreakfastScenario.xlsx", sheetName = "ScenarioPacific", row.names = FALSE, append = TRUE)


tableBuilderNew(env.base, "freq", "z1OverweightLvl1", logisetexpr = "SESBTH == 3") %>% 
  filter(Var == "Overweight")%>% 
  write.xlsx("z1BreakfastScenario.xlsx", sheetName = "BaseLowSES", row.names = FALSE, append = TRUE)


tableBuilderNew(Simenv.scenario, "freq", "z1OverweightLvl1", logisetexpr = "SESBTH == 3") %>% 
  filter(Var == "Overweight")%>% 
  write.xlsx("z1BreakfastScenario.xlsx", sheetName = "ScenarioLowSES", row.names = FALSE, append = TRUE)





compare = 
  function(env.base, env.scenario){
    
    Base <-	
      sapply(env.base$modules[[1]]$run_results, function(x) apply(x$outcome$z1OverweightLvl1[-1],  2, table))
                                                                  
    
    Scenario <-
      sapply(env.scenario$modules[[1]]$run_results, function(x) apply(x$freqs$z1OverweightLvl1[-1], function(y) y[2]/5000))	
    
    results <- numeric(3)
    
    for(i in 1:20){
      lm.fit <- lm(c(Base[i,], Scenario[i,]) ~ factor(rep(c("B", "S"), each = 10)))
      
      results <- 
        rbind(results,c(summary(lm.fit )$coef[2,1],confint(	lm.fit )[2,]))
    }
    
    results <- cbind((1:21)/100, results)
    
    colnames(results) <- c("Age", "Mean Diff", "Lower CI", "Upper CI")
    results <- results[-1,]
    
    round(results*100, 4)	
  }

compare(env.base, Simenv.scenario)







tableBuilderNew(Simenv.scenario, "freq", "z1ScoreLvl1", logisetexpr = "z1ECELvl1 == 0")

temp <- Simenv.scenario
temp$simframe <- env.base$simframe


tableBuilderNew(env.base, "freq", "z1ScoreLvl1", logisetexpr = "z1ECELvl1 == 0")  %>% 
  filter(Year == 17 & Var == "Passed")

tableBuilderNew(Simenv.scenario, "freq", "z1ScoreLvl1", logisetexpr = "z1ECELvl1 == 0", 
                simframe = env.base$simframe)  %>%
  filter(Year == 17 & Var == "Passed")

tableBuilderNew(env.base, "freq", "z1ScoreLvl1", logisetexpr = "z1ECELvl1 == 0", 
                grpbyName = "z1genderLvl1")  %>% 
  filter(Year == 17 & Var == "Passed")

tableBuilderNew(temp, "freq", "z1ScoreLvl1", logisetexpr = "z1ECELvl1 == 0", 
                grpbyName = "z1genderLvl1")  %>%
  filter(Year == 17 & Var == "Passed")


tableBuilderNew(env.base, "freq", "z1ScoreLvl1", logisetexpr = "z1ECELvl1 == 0", 
                grpbyName = "r1stchildethn") %>% filter(Year == 17 & Var == "Passed")

tableBuilderNew(temp, "freq", "z1ScoreLvl1", logisetexpr = "z1ECELvl1 == 0", 
                grpbyName = "r1stchildethn")  %>% filter(Year == 17 & Var == "Passed")


tableBuilderNew(env.base, "freq", "z1FailLvl1", logisetexpr = "z1ECELvl1 == 0")

tableBuilderNew(temp, "freq", "z1FailLvl1", logisetexpr = "z1ECELvl1 == 0")


tableBuilderNew(env.base, "freq", "z1DropLvl1", logisetexpr = "z1ECELvl1 == 0")

tableBuilderNew(temp, "freq", "z1DropLvl1", logisetexpr = "z1ECELvl1 == 0")


#########################################################################################

library(parallel)
library(simarioV2)


setwd("C:\\Users\\kcha193\\workspace\\KnowLab")

initialSim <<- readRDS("base/initialSim.rds")
env.base  <<- readRDS("base/FullBaseRun.rds")


NUM_ITERATIONS <<- 21
dict <<- initialSim$dict
limits <<- initialSim$limits
binbreaks <<- initialSim$binbreaks
catToContModels <<- initialSim$catToContModels
models <<- initialSim$models
PropensityModels <<- initialSim$PropensityModels
children <<- initialSim$children
transition_probabilities <<- initialSim$transition_probabilities

source("SimmoduleMELC1_21.R")

Simenv.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")
Simenv.scenario$cat.adjustments$BREAST[1,]=c(0.18,
                                          table(env.base$simframe$BREAST)[2:12]/5000, 0.257)

cl <- makeCluster(detectCores())

clusterExport(cl, c("binbreaks", "transition_probabilities", "models", 
                    "PropensityModels", "children"))

clusterEvalQ(cl, {library(simarioV2)})
clusterSetRNGStream(cl, 1)

Simenv.scenario <- simulatePShiny(cl, Simenv.scenario, 4)

#Simenv.scenario <- simulateNP(Simenv.scenario, 4)

stopCluster(cl)
tableBuilderNew(env = Simenv.scenario, statistic="freq", variableName="BREAST")
tableBuilderNew(env = env.base, statistic="freq", variableName="BREAST")


###################################################
Simenv.scenario$cat.adjustments$SESBTH[1,] <- c(0.90, 0.05,0.05)	
Simenv.scenario$cat.adjustments$BREAST[1,] <- c(1, rep(0,12))

Simenv.scenario$cat.adjustments$z1CaesareanLvl1[1,] <- c(0.90, 0.05)	




tableBuilderNew(env = Simenv.scenario, statistic="freq", variableName="r1mBMI")
tableBuilderNew(env = Simenv.scenario, statistic="freq", variableName="SESBTH")
tableBuilderNew(env = Simenv.scenario, statistic="freq", variableName="SESBTHLvl1")
tableBuilderNew(env = Simenv.scenario, statistic="freq", variableName="z1CaesareanLvl1")


tableBuilderNew(env = Simenv.scenario, statistic="freq", variableName="BREAST")


tableBuilderNew(env = env.base, statistic="freq", variableName="z1accomLvl1", grpbyName = "bthorder")

tableBuilderNew(env = env.base, statistic="freq", variableName="z1accomLvl1", 
                grpbyName = "bthorder", logisetexpr = "bwkg < 2")

tableBuilderNew(env = env.base, statistic="freq", variableName="z1accomLvl1", grpbyName = "bwkg")

tableBuilderNew(env = env.base, statistic="means", variableName="bwkg", grpbyName = "bthorder")

tableBuilderNew(env = scenario, statistic="means", variableName="bwkg", grpbyName = "bthorder")


env.base$modules[[1]]$run_results_collated$means$IQ	
env.base$modules[[1]]$run_results_collated$freqs$z1ScoreLvl1

env.scenario$modules[[1]]$run_results_collated$means$IQ	
env.scenario$modules[[1]]$run_results_collated$freqs$z1ScoreLvl1

table(env.base$simframe$z1ECELvl1)


apply(
  rbind(
    table(env.base$modules[[1]]$run_results$run1$outcomes$z1ScoreLvl1[which(env.base$simframe$z1ECELvl1 == 0),17])/sum(env.base$simframe$z1ECELvl1 == 0),
    table(env.base$modules[[1]]$run_results$run2$outcomes$z1ScoreLvl1[which(env.base$simframe$z1ECELvl1 == 0),17])/sum(env.base$simframe$z1ECELvl1 == 0),
    table(env.base$modules[[1]]$run_results$run3$outcomes$z1ScoreLvl1[which(env.base$simframe$z1ECELvl1 == 0),17])/sum(env.base$simframe$z1ECELvl1 == 0),
    table(env.base$modules[[1]]$run_results$run4$outcomes$z1ScoreLvl1[which(env.base$simframe$z1ECELvl1 == 0),17])/sum(env.base$simframe$z1ECELvl1 == 0)),
  2, mean)


apply(
  rbind(
    table(env.scenario$modules[[1]]$run_results$run1$outcomes$z1ScoreLvl1[which(env.base$simframe$z1ECELvl1 == 0),17])/sum(env.base$simframe$z1ECELvl1 == 0),
    table(env.scenario$modules[[1]]$run_results$run2$outcomes$z1ScoreLvl1[which(env.base$simframe$z1ECELvl1 == 0),17])/sum(env.base$simframe$z1ECELvl1 == 0),
    table(env.scenario$modules[[1]]$run_results$run3$outcomes$z1ScoreLvl1[which(env.base$simframe$z1ECELvl1 == 0),17])/sum(env.base$simframe$z1ECELvl1 == 0),
    table(env.scenario$modules[[1]]$run_results$run4$outcomes$z1ScoreLvl1[which(env.base$simframe$z1ECELvl1 == 0),17])/sum(env.base$simframe$z1ECELvl1 == 0)),
  2, mean)



test <- tableBuilder(env = env.base, statistic="frequencies", variableName="z1ScoreLvl1")

test <- tableBuilder(env = env.scenario, statistic="frequencies", variableName="z1ScoreLvl1")

test <- tableBuilder(env = env.scenario, statistic="means", variableName="IQ")








label_flattened_mx_grping.and.CIs

test <- tableBuilder(env = env.scenario, statistic="means", variableName="fhrswrk")

test <- tableBuilder(env = env.scenario, statistic="means", variableName="fhrswrk", grpbyName = "r1stchildethn")

test <- tableBuilder(env = env.scenario, statistic="frequencies", variableName="z1HearingLvl1")

tableBuilder(env = env.scenario, statistic="frequencies", variableName="SESBTHLvl1")
tableBuilder(env = env.scenario, statistic="frequencies", variableName="z1singleLvl1")

catvars <- getOutcomeVars(env.scenario$simframe, "categorical")
contvars <- c(getOutcomeVars(env.scenario$simframe, "continuous"), "age")
presimvars <- names(env.scenario$presim.stats)

tableBuilder(env = env.scenario, statistic="frequencies", variableName="SESBTH")


test <- tableBuilder(env = env.scenario, statistic="means", variableName="burt")
test <- tableBuilder(env = env.scenario, statistic="means", variableName="burt")



