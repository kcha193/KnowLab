

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


compareFreq = 
  function(env.base, env.scenario, varName){
    
    Base <-	
      sapply(env.base$modules$run_results, function(x) 
        apply(x[[varName]],  2, table)[-1,])/5000
    
    Scenario <-
      sapply(env.scenario$modules$run_results, function(x) 
        apply(x[[varName]],  2, table)[-1,])/5000
    
    results <- numeric(3)
    
    for(i in 1:20){
      lm.fit <- lm(c(Base[i,], Scenario[i,]) ~ factor(rep(c("B", "S"), each = 10)))
      
      results <- 
        rbind(results,c(summary(lm.fit )$coef[2,1],confint(	lm.fit )[2,]))
    }
    
    #results <- round(apply(results, 2, mean, na.rm = TRUE)*100, 4)	
    results <- cbind(1:21/100, results)
    
    colnames(results) <- c("Age", "Mean Diff", "Lower CI", "Upper CI")
    
    results[-1,]*100
  }

compareFreq(env.base, env.base, "z1OverweightLvl1")




