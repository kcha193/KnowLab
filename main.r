

rm(list = ls())

#devtools::install_github("kcha193/simarioV2")

library(simarioV2)
library(snowfall)
library(stringr)
library(stringi)

setwd("C:\\Users\\kcha193\\workspace\\KnowLab")

source("initKnowLab.R")
source("SimmoduleMELC1_21.R")

NUM_ITERATIONS <- 21

initialSim <- initSim(NUM_ITERATIONS)

saveRDS(initialSim, "./base/initialSim.rds")

sfInit(parallel=TRUE, cpus = 4, slaveOutfile = "log.txt" )

sfExportAll()

sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(simarioV2)
sfLibrary(stringr)

Simenv <- createSimenv("Base", initialSim$simframe, initialSim$dict, "years1_21")

env.base <- simulateP(Simenv, 10)

sfStop()

saveRDS(env.base, "base/FullBaseRun.rds")



##########################################################################################


env.base$modules[[1]]$run_results_collated$means$IQ	
env.base$modules[[1]]$run_results_collated$freqs$z1ScoreLvl1

env.scenario$modules[[1]]$run_results_collated$means$IQ	
env.scenario$modules[[1]]$run_results_collated$freqs$z1ScoreLvl1




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


