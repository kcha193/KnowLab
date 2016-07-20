

rm(list = ls())

#devtools::install_github("kcha193/simarioV2")

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

Simenv.scenario$cat.adjustments$r1mBMI[1,] <- c(0.90, 0.05,0.05, 0)	

# subgroupExpression <- "mhrswrk<21"	
# Simenv.scenario <- setGlobalSubgroupFilterExpression(Simenv.scenario, subgroupExpression)

# Initiate cluster
cl <- makeCluster(detectCores())

clusterExport(cl, c("binbreaks", "transition_probabilities", "models", 
                    "PropensityModels", "children"))

clusterEvalQ(cl, {library(simarioV2)})
clusterSetRNGStream(cl, 1)

Simenv.scenario <- simulatePShiny(cl, Simenv.scenario, 4)

#Simenv.scenario <- simulateNP(Simenv.scenario, 4)

stopCluster(cl)
##################################################################################################


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



