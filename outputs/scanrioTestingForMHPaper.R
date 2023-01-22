

rm(list = ls())
.rs.restartR()


library(tidyverse)
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

source("simulateKnowLab.R")



tableBuilderNew(env.base, "freq", "z1ParentDepressLvl1")

tableBuilderNew(env.base, "freq", "z1ParentDepressLvl1", 
                logisetexpr = " z1INTERACTLvl1 == 0  & z1PUNISHLvl1 == 1")



compareFreq = 
  function(env.base, env.scenario, varName){
    
    Base <-	
      sapply(env.base$modules$run_results, function(x) 
        apply(x[[varName]],  2, table)[-1,])/5000
    
    Scenario <-
      sapply(env.scenario$modules$run_results, function(x) 
        apply(x[[varName]],  2, table)[-1,])/5000
     
    lm.fit <- lm(c(Base, Scenario) ~ factor(rep(c("B", "S"), each = 70)))
      
    
    result <- c(summary(lm.fit )$coef[2,1]*100,confint(	lm.fit )[2,]*100, summary(lm.fit )$coef[2,4])
  
    names(result) <- c("Mean Diff", "Lower CI", "Upper CI", "P-val")  
  
    
    result <- rbind(result, c(result[1:3]/mean(Base), result[4]))
    
    round(result, 4)
  }

compareFreq(env.base, env.base, "z1DepressLvl1")


compareFreqHighRisk = 
  function(env.base, env.scenario, varName, highrisk, yes){
  
    Base <- numeric(0)
    

    
    for(i in 1:10){
      hrGrp <- env.base$modules$run_results[[i]][[highrisk]][,1]
      
      Base <- c(Base,
                    apply(env.base$modules$run_results[[i]][[varName]], 2, 
                          function(x) table(x[hrGrp == yes])/sum(hrGrp==yes))[-1,])
    }
    
    
    Scenario <- numeric(0)
      
      
    for(i in 1:10){
      hrGrp <- env.base$modules$run_results[[i]][[highrisk]][,1]
      
      Scenario <- c(Scenario,
      apply(env.scenario$modules$run_results[[i]][[varName]], 2, 
            function(x) table(x[hrGrp == yes])/sum(hrGrp==yes))[-1,])
    }
   
   
    
    lm.fit <- lm(c(Base, Scenario) ~ factor(rep(c("B", "S"), each = 70)))
    
    
    result <- c(summary(lm.fit )$coef[2,1]*100,confint(	lm.fit )[2,]*100, summary(lm.fit )$coef[2,4])
    
    names(result) <- c("Mean Diff", "Lower CI", "Upper CI", "P-val")  
    
    result <- rbind(result, c(result[1:3]/mean(Base), result[4]))
    
    round(result, 4)
  }


compareFreqHighRisk2 = 
  function(env.base, env.scenario, varName, highrisk, yes){
    
    Base <- numeric(0)
    
    
    
    for(i in 1:10){
      hrGrp1 <- env.base$modules$run_results[[i]][[highrisk[1]]][,1]
      
      hrGrp2 <- env.base$modules$run_results[[i]][[highrisk[2]]][,1]
      
      Base <- c(Base,
                apply(env.base$modules$run_results[[i]][[varName]], 2, 
                      function(x) table(x[hrGrp1 == yes[1] & hrGrp2 == yes[2]])/
                        sum(hrGrp1 == yes[1] & hrGrp2 == yes[2]))[-1,])
    }
    
    
    Scenario <- numeric(0)
    
    
    for(i in 1:10){
      hrGrp1 <- env.base$modules$run_results[[i]][[highrisk[1]]][,1]
      
      hrGrp2 <- env.base$modules$run_results[[i]][[highrisk[2]]][,1]
      
      Scenario <- c(Scenario,
                    apply(env.scenario$modules$run_results[[i]][[varName]], 2, 
                          function(x) table(x[hrGrp1 == yes[1] & hrGrp2 == yes[2]])/
                            sum(hrGrp1 == yes[1] & hrGrp2 == yes[2]))[-1,])
    }
    
    
    
    lm.fit <- lm(c(Base, Scenario) ~ factor(rep(c("B", "S"), each = 70)))
    
    
    result <- c(summary(lm.fit )$coef[2,1]*100,confint(	lm.fit )[2,]*100, summary(lm.fit )$coef[2,4])
    
    names(result) <- c("Mean Diff", "Lower CI", "Upper CI", "P-val")  
    
    result <- rbind(result, c(result[1:3]/mean(Base), result[4]))
    
    round(result, 4)
  }


compareFreqGrp = 
  function(env.base, env.scenario, varName, grp, yes){
    
    Base <- numeric(0)
    
    hrGrp <- env.base$simframe[[grp]]
    
    for(i in 1:10){
      
      Base <- c(Base,
                apply(env.base$modules$run_results[[i]][[varName]], 2, 
                      function(x) table(x[hrGrp == yes])/sum(hrGrp==yes))[-1,])
    }
    
    
    Scenario <- numeric(0)
    

    for(i in 1:10){
      Scenario <- c(Scenario,
                    apply(env.scenario$modules$run_results[[i]][[varName]], 2, 
                          function(x) table(x[hrGrp == yes])/sum(hrGrp==yes))[-1,])
    }
    
    
    
    lm.fit <- lm(c(Base, Scenario) ~ factor(rep(c("B", "S"), each = 70)))
    
    
    result <- c(summary(lm.fit )$coef[2,1]*100,confint(	lm.fit )[2,]*100, summary(lm.fit )$coef[2,4])
    
    names(result) <- c("Mean Diff", "Lower CI", "Upper CI", "P-val")  
    
    result <- rbind(result, c(result[1:3]/mean(Base), result[4]))
    
    round(result, 4)
  }



source("simulateKnowLab.R")

############################################################################
# Parental education ? change from High 34% 58% 8%? ?to???high 50% 45% 5%

tableBuilderNew(env.base, "freq", "z1ParentDepressLvl1")

ParentDepress.scenario <- createSimenv("scenario", 
                                    initialSim$simframe, initialSim$dict, "years1_21")

ParentDepress.scenario$cat.adjustments$z1ParentDepress[16,] = c(1, 0)

ParentDepress.scenario <- simulateSimario(ParentDepress.scenario, 2, 
                                          simulateKnowLab)


tableBuilderNew(ParentDepress.scenario, "freq", "z1ParentDepressLvl1")

cor(ParentDepress.scenario$modules$run_results$run1$z1ParentDepressLvl1)



############################################################################
# Parental education ? change from High 34% 58% 8%? ?to???high 50% 45% 5%


# INTERACT100.scenario ####

INTERACT100.scenario <- createSimenv("scenario", 
       initialSim$simframe, initialSim$dict, "years1_21")

INTERACT100.scenario$cat.adjustments$z1INTERACT[5,] <- c(0,1)

INTERACT100.scenario <- simulateSimario(INTERACT100.scenario, 10, 
                                          simulateKnowLab)

tableBuilderNew(env.base, "freq", "z1DepressLvl1")%>%
  filter(Var == "Depression")
tableBuilderNew(INTERACT100.scenario, "freq", "z1DepressLvl1")%>%
  filter(Var == "Depression")
compareFreq(env.base, INTERACT100.scenario, "z1DepressLvl1")

tableBuilderNew(env.base, "freq", "z1DepressLvl1", 
                logisetexpr = "z1INTERACTLvl1 == 0")
tableBuilderNew(INTERACT100.scenario, "freq","z1DepressLvl1", 
                logisetexpr = "z1INTERACTLvl1 == 0",
                envBase = env.base, basePop = TRUE) 

compareFreqHighRisk(env.base, INTERACT100.scenario, 
                    "z1DepressLvl1", "z1INTERACTLvl1", yes = 0)

tableBuilderNew(env.base, "freq", "z1DepressLvl1", grpbyName = "SESBTH")%>%
  filter(Var == "Depression")

compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)
compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)


tableBuilderNew(env.base, "freq", "z1DepressLvl1", grpbyName = "r1ParentEduc")%>%
  filter(Var == "Depression")

compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)
compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)


# INTERACT50.scenario ####

INTERACT50.scenario <- createSimenv("scenario", 
                                    initialSim$simframe, 
                                    initialSim$dict, 
                                    "years1_21")

INTERACT50.scenario$cat.adjustments$z1INTERACT[5,] <- c(0.079,0.921)

INTERACT50.scenario <- simulateSimario(INTERACT50.scenario, 10, 
                                     simulateKnowLab)

tableBuilderNew(env.base, "freq", "z1DepressLvl1") %>% 
  filter(Var == "Depression")

tableBuilderNew(INTERACT50.scenario, "freq", "z1DepressLvl1") %>%
  filter(Var == "Depression")

compareFreq(env.base, INTERACT50.scenario, "z1DepressLvl1")

tableBuilderNew(env.base, "freq", "z1DepressLvl1", 
                logisetexpr = "z1INTERACTLvl1 == 0") %>%
  filter(Var == "Depression")

tableBuilderNew(INTERACT50.scenario, "freq","z1DepressLvl1", 
                logisetexpr = "z1INTERACTLvl1 == 0",
                envBase = env.base, basePop = TRUE) %>% 
  filter(Var == "Depression")

compareFreqHighRisk(env.base, INTERACT50.scenario, 
                    "z1DepressLvl1", "z1INTERACTLvl1", yes = 0)

# INTERACT25.scenario ####

INTERACT25.scenario <- createSimenv("scenario", 
                                    initialSim$simframe, initialSim$dict, "years1_21")

INTERACT25.scenario$cat.adjustments$z1INTERACT[5,] <- c(0.118,0.882)

INTERACT25.scenario <- simulateSimario(INTERACT25.scenario, 10, 
                                       simulateKnowLab)

tableBuilderNew(env.base, "freq", "z1DepressLvl1") %>% 
  filter(Var == "Depression")
tableBuilderNew(INTERACT25.scenario, "freq", "z1DepressLvl1") %>%
  filter(Var == "Depression")
compareFreq(env.base, INTERACT25.scenario, "z1DepressLvl1")

tableBuilderNew(env.base, "freq", "z1DepressLvl1", 
                logisetexpr = "z1INTERACTLvl1 == 0") %>%
  filter(Var == "Depression")
tableBuilderNew(INTERACT25.scenario, "freq","z1DepressLvl1", 
                logisetexpr = "z1INTERACTLvl1 == 0",
                envBase = env.base, basePop = TRUE) %>% 
  filter(Var == "Depression")

compareFreqHighRisk(env.base, INTERACT25.scenario,
                    "z1DepressLvl1", "z1INTERACTLvl1", yes = 0)

compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)
compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)
compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)


compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)
compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)

# PUNISH100.scenario ####

PUNISH100.scenario <- createSimenv("scenario", 
                                     initialSim$simframe,
                                     initialSim$dict, "years1_21")

PUNISH100.scenario$cat.adjustments$z1PUNISH[5,] <- c(1,0)

PUNISH100.scenario <- simulateSimario(PUNISH100.scenario, 10, 
                                        simulateKnowLab)

tableBuilderNew(env.base, "freq", "z1DepressLvl1")%>%
  filter(Var == "Depression")
tableBuilderNew(PUNISH100.scenario, "freq", "z1DepressLvl1")%>%
  filter(Var == "Depression")

compareFreq(env.base, PUNISH100.scenario, "z1DepressLvl1")

tableBuilderNew(env.base, "freq", "z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1")%>%
  filter(Var == "Depression")
tableBuilderNew(PUNISH100.scenario, "freq","z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1",
                envBase = env.base, basePop = TRUE) %>%
  filter(Var == "Depression")

compareFreqHighRisk(env.base, PUNISH100.scenario, 
                    "z1DepressLvl1", "z1PUNISHLvl1", yes = 1)


# PUNISH50.scenario ####


PUNISH50.scenario <- createSimenv("scenario", 
                                    initialSim$simframe, 
                                    initialSim$dict, 
                                    "years1_21")

PUNISH50.scenario$cat.adjustments$z1PUNISH[5,] <- c(0.889, 0.111)

PUNISH50.scenario <- simulateSimario(PUNISH50.scenario, 10, 
                                       simulateKnowLab)

tableBuilderNew(env.base, "freq", "z1DepressLvl1") %>% 
  filter(Var == "Depression")
tableBuilderNew(PUNISH50.scenario, "freq", "z1DepressLvl1") %>%
  filter(Var == "Depression")
compareFreq(env.base, PUNISH50.scenario, "z1DepressLvl1")

tableBuilderNew(env.base, "freq", "z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1") %>%
  filter(Var == "Depression")
tableBuilderNew(PUNISH50.scenario, "freq","z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1",
                envBase = env.base, basePop = TRUE) %>% 
  filter(Var == "Depression")

compareFreqHighRisk(env.base, PUNISH50.scenario, 
                    "z1DepressLvl1", "z1PUNISHLvl1", yes = 1)

# PUNISH25.scenario ####

PUNISH25.scenario <- createSimenv("scenario", 
                                    initialSim$simframe, initialSim$dict, 
                                  "years1_21")

PUNISH25.scenario$cat.adjustments$z1PUNISH[5,] <- c(0.833,0.167)

PUNISH25.scenario <- simulateSimario(PUNISH25.scenario, 10, 
                                       simulateKnowLab)

tableBuilderNew(env.base, "freq", "z1DepressLvl1") %>% 
  filter(Var == "Depression")
tableBuilderNew(PUNISH25.scenario, "freq", "z1DepressLvl1") %>%
  filter(Var == "Depression")
compareFreq(env.base, PUNISH25.scenario, "z1DepressLvl1")

tableBuilderNew(env.base, "freq", "z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1") %>%
  filter(Var == "Depression")
tableBuilderNew(PUNISH25.scenario, "freq","z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1",
                envBase = env.base, basePop = TRUE) %>% 
  filter(Var == "Depression")

compareFreqHighRisk(env.base, PUNISH25.scenario,
                    "z1DepressLvl1", "z1PUNISHLvl1", yes = 1)

########################################################################################################

# INTERACTPUNISH100.scenario ####
INTERACTPUNISH100.scenario <- createSimenv("scenario", 
                                   initialSim$simframe,
                                   initialSim$dict, "years1_21")

INTERACTPUNISH100.scenario$cat.adjustments$z1PUNISH[5,] <- c(1,0)
INTERACTPUNISH100.scenario$cat.adjustments$z1INTERACT[5,] <- c(0,1)

INTERACTPUNISH100.scenario <- simulateSimario(INTERACTPUNISH100.scenario, 10, 
                                      simulateKnowLab)

tableBuilderNew(env.base, "freq", "z1DepressLvl1")%>%
  filter(Var == "Depression")
tableBuilderNew(INTERACTPUNISH100.scenario, "freq", "z1DepressLvl1")%>%
  filter(Var == "Depression")
compareFreq(env.base, INTERACTPUNISH100.scenario, "z1DepressLvl1")

tableBuilderNew(env.base, "freq", "z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1 & z1INTERACTLvl1 == 0")%>%
  filter(Var == "Depression")
tableBuilderNew(INTERACTPUNISH100.scenario, "freq","z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1 & z1INTERACTLvl1 == 0",
                envBase = env.base, basePop = TRUE) %>%
  filter(Var == "Depression")

compareFreqHighRisk2(env.base, INTERACTPUNISH100.scenario, 
                    "z1DepressLvl1", c("z1PUNISHLvl1", "z1INTERACTLvl1") , yes = c(1,0))


# INTERACTPUNISH50.scenario ####


INTERACTPUNISH50.scenario <- createSimenv("scenario", 
                                  initialSim$simframe, 
                                  initialSim$dict, 
                                  "years1_21")

INTERACTPUNISH50.scenario$cat.adjustments$z1PUNISH[5,] <- c(0.889, 0.111)
INTERACTPUNISH50.scenario$cat.adjustments$z1INTERACT[5,] <- c(0.079,0.921)

INTERACTPUNISH50.scenario <- simulateSimario(INTERACTPUNISH50.scenario, 10, 
                                     simulateKnowLab)

tableBuilderNew(env.base, "freq", "z1DepressLvl1") %>% 
  filter(Var == "Depression")
tableBuilderNew(INTERACTPUNISH50.scenario, "freq", "z1DepressLvl1") %>%
  filter(Var == "Depression")
compareFreq(env.base, INTERACTPUNISH50.scenario, "z1DepressLvl1")

tableBuilderNew(env.base, "freq", "z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1 & z1INTERACTLvl1 == 0") %>%
  filter(Var == "Depression")
tableBuilderNew(INTERACTPUNISH50.scenario, "freq","z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1 & z1INTERACTLvl1 == 0",
                envBase = env.base, basePop = TRUE) %>% 
  filter(Var == "Depression")

compareFreqHighRisk2(env.base, INTERACTPUNISH50.scenario, 
                     "z1DepressLvl1", 
                     c("z1PUNISHLvl1", "z1INTERACTLvl1") , yes = c(1,0))


# INTERACTPUNISH25.scenario ####

INTERACTPUNISH25.scenario <- createSimenv("scenario", 
                                  initialSim$simframe, initialSim$dict, "years1_21")

INTERACTPUNISH25.scenario$cat.adjustments$z1PUNISH[5,] <- c(0.833,0.167)
INTERACTPUNISH25.scenario$cat.adjustments$z1INTERACT[5,] <- c(0.118,0.882)

INTERACTPUNISH25.scenario <- simulateSimario(INTERACTPUNISH25.scenario, 10, 
                                     simulateKnowLab)

tableBuilderNew(env.base, "freq", "z1DepressLvl1") %>% 
  filter(Var == "Depression")
tableBuilderNew(INTERACTPUNISH25.scenario, "freq", "z1DepressLvl1") %>%
  filter(Var == "Depression")
compareFreq(env.base, INTERACTPUNISH25.scenario, "z1DepressLvl1")

tableBuilderNew(env.base, "freq", "z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1 & z1INTERACTLvl1 == 0") %>%
  filter(Var == "Depression")
tableBuilderNew(INTERACTPUNISH25.scenario, "freq","z1DepressLvl1", 
                logisetexpr = "z1PUNISHLvl1 == 1 & z1INTERACTLvl1 == 0",
                envBase = env.base, basePop = TRUE) %>% 
  filter(Var == "Depression")

compareFreqHighRisk2(env.base, INTERACTPUNISH25.scenario, 
                     "z1DepressLvl1", 
                     c("z1PUNISHLvl1", "z1INTERACTLvl1") , yes = c(1,0))


#############################################################################################

compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)
compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)

compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)
compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, INTERACT25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)

compareFreqGrp(env.base, INTERACT50.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, INTERACT50.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)
compareFreqGrp(env.base, INTERACT50.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)

compareFreqGrp(env.base, INTERACT50.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)
compareFreqGrp(env.base, INTERACT50.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, INTERACT50.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)


compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)
compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)

compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)
compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, INTERACT100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)

############################################################################
compareFreqGrp(env.base, PUNISH25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, PUNISH25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)
compareFreqGrp(env.base, PUNISH25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)

compareFreqGrp(env.base, PUNISH25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)
compareFreqGrp(env.base, PUNISH25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, PUNISH25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)

compareFreqGrp(env.base, PUNISH50.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, PUNISH50.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)
compareFreqGrp(env.base, PUNISH50.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)

compareFreqGrp(env.base, PUNISH50.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)
compareFreqGrp(env.base, PUNISH50.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, PUNISH50.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)


compareFreqGrp(env.base, PUNISH100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, PUNISH100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)
compareFreqGrp(env.base, PUNISH100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)

compareFreqGrp(env.base, PUNISH100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)
compareFreqGrp(env.base, PUNISH100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, PUNISH100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)

############################################################################

compareFreqGrp(env.base, INTERACTPUNISH25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, INTERACTPUNISH25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)
compareFreqGrp(env.base, INTERACTPUNISH25.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)

compareFreqGrp(env.base, INTERACTPUNISH25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)
compareFreqGrp(env.base, INTERACTPUNISH25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, INTERACTPUNISH25.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)

compareFreqGrp(env.base, INTERACTPUNISH50.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, INTERACTPUNISH50.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)
compareFreqGrp(env.base, INTERACTPUNISH50.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)

compareFreqGrp(env.base, INTERACTPUNISH50.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)
compareFreqGrp(env.base, INTERACTPUNISH50.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, INTERACTPUNISH50.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)


compareFreqGrp(env.base, INTERACTPUNISH100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 3)
compareFreqGrp(env.base, INTERACTPUNISH100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 2)
compareFreqGrp(env.base, INTERACTPUNISH100.scenario, 
               "z1DepressLvl1", "SESBTH", yes = 1)

compareFreqGrp(env.base, INTERACTPUNISH100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 3)
compareFreqGrp(env.base, INTERACTPUNISH100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 2)
compareFreqGrp(env.base, INTERACTPUNISH100.scenario, 
               "z1DepressLvl1", "r1ParentEduc", yes = 1)


############################################################################

prop.test(c(74, 72)*5, c(5000, 5000))$p.value 
prop.test(c(0.131, 0.091)*785, c(785, 785))$p.value 


prop.test(c(74, 70)*5, c(5000, 5000))$p.value 
prop.test(c(.131, .084)*785, c(785, 785))$p.value 


prop.test(c(74, 65)*5, c(5000, 5000))$p.value 
prop.test(c(.131, .071)*785, c(785, 785))$p.value 


prop.test(c(74, 72)*5, c(5000, 5000))$p.value 
prop.test(c(.108, .083)*1100, c(1100, 1100))$p.value 


prop.test(c(74, 71)*5, c(5000, 5000))$p.value 
prop.test(c(.108, .079)*1100, c(1100, 1100))$p.value 


prop.test(c(74, 67)*5, c(5000, 5000))$p.value 
prop.test(c(.108, .073)*1100, c(1100, 1100))$p.value 


prop.test(c(74, 69)*5, c(5000, 5000))$p.value 
prop.test(c(.18, .095)*208, c(208, 208))$p.value 


prop.test(c(74, 65)*5, c(5000, 5000))$p.value 
prop.test(c(.18, .084)*208, c(208, 208))$p.value 


prop.test(c(74, 58)*5, c(5000, 5000))$p.value 
prop.test(c(.18, .063)*208, c(208, 208))$p.value 



























