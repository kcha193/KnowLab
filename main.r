

rm(list = ls())

.rs.restartR()

#devtools::install_github("kcha193/simarioV2")

library(parallel)
library(simarioV2)

setwd("C:\\Users\\kcha193\\workspace\\KnowLab")

source("initKnowLab.R")
source("SimmoduleMELC1_21.R")

NUM_ITERATIONS <- 21

initialSim <- initSim(NUM_ITERATIONS)

saveRDS(initialSim, "base/initialSim.rds")
saveRDS(initialSim, "../KnowLabShiny/base/initialSim.rds")

Simenv <- createSimenv("Base", initialSim$simframe, initialSim$dict, "years1_21")


# Initiate cluster
cl <- makeCluster(detectCores())

clusterExport(cl, c("binbreaks", "transition_probabilities", "models", 
                    "PropensityModels", "children"))

clusterEvalQ(cl, {library(simarioV2)})
clusterSetRNGStream(cl, 1)

env.base <- simulatePShiny(cl, Simenv, 10)

#env.base <- simulateNP(Simenv, 2)
stopCluster(cl)

saveRDS(env.base, "base/FullBaseRun.rds")
saveRDS(env.base, "../KnowLabShiny/base/FullBaseRun.rds")

.rs.restartR()

##########################################################################################

tableBuilderNew(env.base, statistic = "mean", "Score")

tableBuilderNew(env.base, statistic = "mean", "Score", grpbyName = "z1genderLvl1")
tableBuilderNew(env.base, statistic = "mean", "Score", grpbyName = "r1stchildethn")

tableBuilderNew(env.base, statistic = "mean", "Score", grpbyName = "r1stchildethn",
                logisetexpr = "z1genderLvl1 == 0")
tableBuilderNew(env.base, statistic = "mean", "Score", grpbyName = "r1stchildethn",
                logisetexpr = "z1genderLvl1 == 1")




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



tableBuilderNew(env.base, statistic = "means", "z1ScoreLvl1", grpbyName = "z1genderLvl1")




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



