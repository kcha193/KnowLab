

rm(list = ls())

#devtools::install_github("kcha193/simarioV2")

library(simarioV2)
library(snowfall)
library(stringr)
library(stringi)

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

env.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

env.scenario$cat.adjustments$z1ECE[1,] <- c(0,1)	

sfInit(parallel=TRUE, cpus = 4, slaveOutfile = "test.txt" )

sfExportAll()

sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(simarioV2)
sfLibrary(stringr)

env.scenario <- simulatePShiny(env.scenario, 4)
sfStop()




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



