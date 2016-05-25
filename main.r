

rm(list = ls())

.rs.restartR()

#devtools::install_github("kcha193/simarioV2")

library(simarioV2)
library(snowfall)
library(stringr)
library(stringi)
library(tidyr)
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
 
.rs.restartR()

##########################################################################################

library(knitr)
library(ggplot2)

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



