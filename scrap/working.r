
setwd("C:/Users/kcha193/workspace/MELC/data")

shiny::runApp("shinyMELC")


setwd("C:/Users/kcha193/workspace/MELC/data")

source("MELC.R")

source("MELCBase.R")


env.base  <- readRDS("base/FullBaseRun.rds")

#load("base/FullBaseRun.rData") # loads env.base object 




env.base <<- SimenvMELC$new("Base", simframe=simframe.master)
	
env.base.list = as.list(env.base)

simframe <- env.base.list$simframe

tableBuilder("Base", "frequencies", "z1OverweightLvl1", dict = env.base$dict)


env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1


EU <- tableBuilder("Base", "frequencies", "z1OverweightLvl1", 
	dict = env.base$dict, grpbyName = "r1stchildethnLvl1")[,10:12]
	
MA <- tableBuilder("Base", "frequencies", "z1OverweightLvl1", 
	dict = env.base$dict, grpbyName = "r1stchildethnLvl2")[,10:12]
	
PA <- tableBuilder("Base", "frequencies", "z1OverweightLvl1", 
	dict = env.base$dict, grpbyName = "r1stchildethnLvl3")[,10:12]

AS <- tableBuilder("Base", "frequencies", "z1OverweightLvl1", 
	dict = env.base$dict, grpbyName = "r1stchildethnLvl4")[,10:12]



overweight.scenario1.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,], 
	EU[-1,], MA[-1,], PA[-1,], AS[-1,])

rownames(overweight.scenario1.counts) <- NULL	
	

overweight.scenario1.counts <- 
	data.frame(
		Year = rep(2:21,5), 
		EthnicGroup = factor(rep(c("Base", "European", "Maori", "Pacific", "Asian"), each = 20)),
		overweight.scenario1.counts)
								
names(overweight.scenario1.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario1.counts, aes(x = Year, y=Overweight, colour  = EthnicGroup)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

p1

ggplot(overweight.scenario1.counts, aes(x = Year, y=Overweight, colour  = EthnicGroup)) + geom_point()+
		 geom_errorbar(limit, width=0.25, position=dodge)		
	
#################################################################################

env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1

env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1 

  env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario1$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1


pOverweight <- c(NA, 29.02058, 29.27779, 29.60634, 30.00623, 30.47746, 31.02002,
			31.63393, 32.31917, 33.07576, 33.90369, 34.80295, 35.77356, 36.81550,
			37.92878, 39.11341, 40.36937, 41.69667, 43.09531, 44.56530, 46.10662)



overweight.scenario1.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,], 
	cbind(pOverweight, pOverweight, pOverweight)[-1,])

rownames(overweight.scenario1.counts) <- NULL	
	
overweight.scenario1.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Name = factor(rep(c("Base", "NZHS"), each = 20)),
		overweight.scenario1.counts)
								
names(overweight.scenario1.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario1.counts, aes(x = Year, y=Overweight, colour  = Name)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

p1








			
pObese <- c(NA, 8.340484,  8.764238,  9.208993,  9.674749, 10.161506, 10.669264,  11.198024, 11.747785, 12.318547, 12.910310, 13.523074, 14.156840, 14.811607, 15.487375, 16.184144, 16.901914, 17.640686, 18.400458, 19.181232, 19.983007)			   
	


pObese.scenario1.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,], 	
	cbind(pObese, pObese, pObese)[-1,])

rownames(pObese.scenario1.counts) <- NULL	
	
pObese.scenario1.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Name = factor(rep(c("Base", "NZHS"), each = 20)),
		pObese.scenario1.counts)
								
names(pObese.scenario1.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario1.counts, aes(x = Year, y=pObese, colour  = Name)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, ncol=2)
	  		

#######################################################
#Testing scenario
env.scenario <<- SimenvMELC$new()
	
#subgroupExpression <- "z1genderLvl1==0"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
#env.scenario1$cat.adjustments$BREAST[1,]=c(0.01,rep( 0.0825,12))
 env.scenario$cat.adjustments$SESBTH[1,] = 
	c(1, 0, 0)

sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario$simulateP(10)

sfStop()
	

print(env.base$presim.stats$SESBTH)	
print(env.scenario$presim.stats$SESBTH)

env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1

save(env.scenario, file="scenario.rData") # save env.base object
	
#######################################################
#Testing scenario
env.scenario1 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
env.scenario1$cat.adjustments$r1Sleep[2,]=
env.scenario1$cat.adjustments$r1Sleep[3,]=
env.scenario1$cat.adjustments$r1Sleep[4,]=
env.scenario1$cat.adjustments$r1Sleep[5,]=
env.scenario1$cat.adjustments$r1Sleep[6,]=
env.scenario1$cat.adjustments$r1Sleep[7,]=
env.scenario1$cat.adjustments$r1Sleep[8,]=
env.scenario1$cat.adjustments$r1Sleep[9,]=
env.scenario1$cat.adjustments$r1Sleep[10,]=
env.scenario1$cat.adjustments$r1Sleep[11,]=
env.scenario1$cat.adjustments$r1Sleep[12,]=
env.scenario1$cat.adjustments$r1Sleep[13,]=
env.scenario1$cat.adjustments$r1Sleep[14,]=
env.scenario1$cat.adjustments$r1Sleep[15,]=
env.scenario1$cat.adjustments$r1Sleep[16,]=
env.scenario1$cat.adjustments$r1Sleep[17,]=
env.scenario1$cat.adjustments$r1Sleep[18,]=c(0.98,0.01,0.01)


sfInit(parallel=TRUE, cpus = 3, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario1$simulateP(10)

sfStop()
	
env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario1$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1

env.base$modules$years1_21$run_results_collated$freqs$r1Sleep
env.scenario1$modules$years1_21$run_results_collated$freqs$r1Sleep



env.base$modules$years1_21$run_results_collated$freqs$r1Sleep
env.scenario1$modules$years1_21$run_results_collated$freqs$r1Sleep

#######################################################
#Testing scenario
env.scenario1 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
env.scenario1$cat.adjustments$BREAST[1,]=c(0.01,rep( 0.0825,12))


sfInit(parallel=TRUE, cpus = 3, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario1$simulateP(10)

sfStop()


env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1

env.scenario1$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario1$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1



overweight.scenario1.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-c(1),],
	env.scenario1$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-c(1),])

rownames(overweight.scenario1.counts) <- NULL	
	
overweight.scenario1.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario1 = factor(rep(c("Breakfast 81%", "Breakfast 100%"), each = 20)),
		overweight.scenario1.counts)
								
names(overweight.scenario1.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario1.counts, aes(x = Year, y=Overweight, colour  = scenario1)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


p1
			   


pObese.scenario1.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-c(1),],
	env.scenario1$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-c(1),])

rownames(pObese.scenario1.counts) <- NULL	
	
pObese.scenario1.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario1 = factor(rep(c("Breakfast 81%", "Breakfast 100%"), each = 17)),
		pObese.scenario1.counts)
								
names(pObese.scenario1.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario1.counts, aes(x = Year, y=pObese, colour  = scenario1)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, ncol=2)


#######################################################
#Testing scenario
env.scenario2 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
#env.scenario1$cat.adjustments$BREAST[1,]=c(0.01,rep( 0.0825,12))
 env.scenario2$cat.adjustments$z1Breakfast[1,] = 
	c(0, 1)

sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario2$simulateP(10)

sfStop()

save(env.scenario2, file="scenario2.rData") # save env.base object

load("scenario2.rData") 

########################################################


env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario2$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1



overweight.scenario2.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-c(1),],
	env.scenario2$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-c(1),])

rownames(overweight.scenario2.counts) <- NULL	
	
overweight.scenario2.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario2 = factor(rep(c("Breakfast 81%", "Breakfast 100%"), each = 20)),
		overweight.scenario2.counts)
								
names(overweight.scenario2.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario2.counts, aes(x = Year, y=Overweight, colour  = scenario2)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


ggplot(overweight.scenario2.counts, aes(x = Year, y=Overweight, colour  = scenario2)) + geom_point()+ geom_errorbar(limit, width=0.25, position=dodge) 
			   


pObese.scenario2.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-c(1:4),],
	env.scenario2$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-c(1:4),])

rownames(pObese.scenario2.counts) <- NULL	
	
pObese.scenario2.counts <- 
	data.frame(
		Year = rep(5:21,2), 
		scenario2 = factor(rep(c("Breakfast 81%", "Breakfast 100%"), each = 17)),
		pObese.scenario2.counts)
								
names(pObese.scenario2.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario2.counts, aes(x = Year, y=pObese, colour  = scenario2)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, ncol=2)

#######################################################
#Testing scenario
env.scenario3 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
#env.scenario1$cat.adjustments$BREAST[1,]=c(0.01,rep( 0.0825,12))
 env.scenario3$cat.adjustments$r1mBMI[1,] = 
	c(0.77, 0.02, 0.135, 0.075)

sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario3$simulateP(10)

sfStop()

save(env.scenario3, file="scenario3.rData") # save env.base object
load("scenario3.rData") # save env.base object

env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario3$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1

overweight.scenario3.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-c(1),],
	env.scenario3$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-c(1),])

rownames(overweight.scenario3.counts) <- NULL	
	
overweight.scenario3.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario3 = factor(rep(c("Maternal Overweight+Obese 43%", "Maternal Overweight+Obese 21%"), each = 20)),
		overweight.scenario3.counts)
								
names(overweight.scenario3.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario3.counts, aes(x = Year, y=Overweight, colour  = scenario3)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)
		
	
 
p1 + geom_smooth(span = 0.3)

ggplot(overweight.scenario3.counts, aes(x = Year, y=Overweight, colour  = scenario3)) + geom_point() + stat_smooth(span = 1)

x11()
ggplot(overweight.scenario3.counts, aes(x = Year, y=Overweight, colour  = scenario3)) + geom_point() + geom_errorbar(limit, width=0.25, position=dodge)






pObese.scenario3.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-c(1, 13:21),],
	env.scenario3$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-c(1, 13:21),])

rownames(pObese.scenario3.counts) <- NULL	
	
pObese.scenario3.counts <- 
	data.frame(
		Year = rep(2:12,2), 
		scenario3 = factor(rep(c("Maternal Overweight+Obese 43%", "Maternal Overweight+Ob ese 21%"), each = 11)),
		pObese.scenario3.counts)
								
names(pObese.scenario3.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario3.counts, aes(x = Year, y=pObese, colour  = scenario3)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

grid.arrange(p1, p2, ncol=2)

#######################################################
#Testing scenario
env.scenario4 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
#env.scenario1$cat.adjustments$BREAST[1,]=c(0.01,rep( 0.0825,12))
env.scenario4$cat.adjustments$pregsmk[1,] = c(0.90,0.02,0.02,0.02,0.02,0.02)


sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario4$simulateP(10)

sfStop()




env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario4$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1



overweight.scenario4.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario4$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

rownames(overweight.scenario4.counts) <- NULL	
	
overweight.scenario4.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario4 = factor(rep(c("SmokePreg 22%", "SmokePreg 10%"), each = 20)),
		overweight.scenario4.counts)
								
names(overweight.scenario4.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario4.counts, aes(x = Year, y=Overweight, colour  = scenario4)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


p1
			   


pObese.scenario4.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario4$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,])

rownames(pObese.scenario4.counts) <- NULL	
	
pObese.scenario4.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario4 = factor(rep(c("SmokePreg 22%", "SmokePreg 10%"), each = 20)),
		pObese.scenario4.counts)
								
names(pObese.scenario4.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario4.counts, aes(x = Year, y=pObese, colour  = scenario4)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, ncol=2)

#######################################################
#Testing scenario
env.scenario5 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
env.scenario5$cat.adjustments$BREAST[1,]=c(0.15,rep( 0.07083333,12))


sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario5$simulateP(10)

sfStop()




env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario5$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1



overweight.scenario5.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario5$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

rownames(overweight.scenario5.counts) <- NULL	
	
overweight.scenario5.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario5 = factor(rep(c("BreastFed 64%", "BreastFed 85%"), each = 20)),
		overweight.scenario5.counts)
								
names(overweight.scenario5.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario5.counts, aes(x = Year, y=Overweight, colour  = scenario5)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


p1
			   


pObese.scenario5.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario5$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,])

rownames(pObese.scenario5.counts) <- NULL	
	
pObese.scenario5.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario5 = factor(rep(c("BreastFed 22%", "BreastFed 10%"), each = 20)),
		pObese.scenario5.counts)
								
names(pObese.scenario5.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario5.counts, aes(x = Year, y=pObese, colour  = scenario5)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, ncol=2)

#######################################################
#Testing scenario
env.scenario6 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
env.scenario6$cat.adjustments$z1Caesarean[1,]=c(0.95, 0.05)


sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario6$simulateP(10)

sfStop()

save(env.scenario6, file="scenario6.rData") # save env.base object

load("scenario6.rData") 



env.base$modules$years1_21$run_results_collated$freqs$z1CaesareanLvl1

env.scenario6$modules$years1_21$run_results_collated$freqs$z1CaesareanLvl1


env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario6$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1



overweight.scenario6.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario6$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

	
rownames(overweight.scenario6.counts) <- NULL	
	
overweight.scenario6.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario6 = factor(rep(c("Caesarean delivery 13%", "Caesarean delivery 5%"), each = 20)),
		overweight.scenario6.counts)
								
names(overweight.scenario6.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario6.counts, aes(x = Year, y=Overweight, colour  = scenario6)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

p1

pObese.scenario6.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario6$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,])

rownames(pObese.scenario6.counts) <- NULL	
	
pObese.scenario6.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario6 = factor(rep(c("Caesarean delivery 20%", "Caesarean delivery 10%"), each = 20)),
		pObese.scenario6.counts)
								
names(pObese.scenario6.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario6.counts, aes(x = Year, y=pObese, colour  = scenario6)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, ncol=2)



#######################################################
#Testing scenario
env.scenario7 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
env.scenario7$cat.adjustments$r1ParentEduc[1,]=c(0.6, 0.324, 0.076)


sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario7$simulate(10)

sfStop()




env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario7$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1



overweight.scenario7.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario7$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

	
rownames(overweight.scenario7.counts) <- NULL	
	
overweight.scenario7.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario7 = factor(rep(c("Tertiary 34%", "Tertiary 60%"), each = 20)),
		overweight.scenario7.counts)
								
names(overweight.scenario7.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario7.counts, aes(x = Year, y=Overweight, colour  = scenario7)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)



pObese.scenario7.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario7$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,])

rownames(pObese.scenario7.counts) <- NULL	
	
pObese.scenario7.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario7 = factor(rep(c("Tertiary 34%", "Tertiary 60%"), each = 20)),
		pObese.scenario7.counts)
								
names(pObese.scenario7.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario7.counts, aes(x = Year, y=pObese, colour  = scenario7)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, ncol=2)




#######################################################
#Testing scenario
env.scenario7 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
env.scenario7$cat.adjustments$z1Breakfast[1,] = 
	c(0, 1)
 env.scenario7$cat.adjustments$r1mBMI[1,] = 
	c(0.77, 0.02, 0.135, 0.075)

	sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario7$simulateP(10)

sfStop()


save(env.scenario7, file="scenario7.rData") # save env.base object


env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario7$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1



overweight.scenario7.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario7$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

	
rownames(overweight.scenario7.counts) <- NULL	
	
overweight.scenario7.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario7 = factor(rep(c("Breakfast 81%, Maternal Overweight+Obese 43%", "Breakfast 100%, Maternal Overweight+Obese 21%"), each = 20)),
		overweight.scenario7.counts)
								
names(overweight.scenario7.counts)[3:5] = c("Overweight", "Lower", "Upper")

overweight.scenario7.counts$CI <- overweight.scenario7.counts$Overweight - overweight.scenario7.counts$Lower





limit <- aes(ymax = Overweight + CI, ymin=Overweight - CI)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario7.counts, aes(x = Year, y=Overweight, colour  = scenario7)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

p1

ggplot(overweight.scenario7.counts[-c(21,22, 25:26),], aes(x = Year, y=Overweight, colour  = scenario7)) + geom_point() + stat_smooth(span = 1, se = FALSE)+ geom_errorbar(limit, width=0.25, position=dodge)



load("scenario7.rData")
load("scenario2.rData")
load("scenario3.rData")




overweight.scenario.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario7$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario2$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario3$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

rownames(overweight.scenario.counts) <- NULL
		
overweight.scenario.counts <- 
	data.frame(
		Year = rep(2:21,4), 
		Scenario = factor(rep(c("Breakfast 81%, Maternal Overweight+Obese 43%", "Breakfast 100% + Maternal Overweight+Obese 21%", 
		"Breakfast 100%", "Maternal Overweight+Obese 21%"), each = 20), 
		levels =c("Breakfast 81%, Maternal Overweight+Obese 43%", 
		"Breakfast 100%", "Maternal Overweight+Obese 21%", "Breakfast 100% + Maternal Overweight+Obese 21%")),
		overweight.scenario.counts)

names(overweight.scenario.counts)[3:5] = c("Overweight", "Lower", "Upper")

overweight.scenario.counts$CI <- overweight.scenario.counts$Overweight - overweight.scenario.counts$Lower

p1 <- ggplot(overweight.scenario.counts, aes(x = Year, y=Overweight, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


p1 <- ggplot(overweight.scenario.counts, aes(x = Year, y=Overweight, colour  = Scenario)) + 
		geom_point(size = 2)+ geom_errorbar(limit, width=0.25)

x11()
ggplot(overweight.scenario.counts[-c(21,22, 25,26, 61,62, 65, 66),], aes(x = Year, y=Overweight, colour  = Scenario)) + geom_point() + stat_smooth(span = 1, se = FALSE)+ geom_errorbar(limit, width=0.25, position=dodge)


ggplot(overweight.scenario.counts, aes(x = Year, y=Overweight, colour  = Scenario)) + geom_point() + stat_smooth(data = overweight.scenario.counts[-c(22,24, 25,26, 62,64,  65, 66, 67),], aes(x = Year, y=Overweight, colour  = Scenario), span = 1, se = FALSE,  inherit.aes = FALSE)+ geom_errorbar(limit, width=0.25, position=dodge)





		
overweight.scenario7.counts$scenario7 <- as.character(overweight.scenario7.counts$scenario7)
		
	

model <- lm(Overweight ~ I(Year) + I(Year^2), data=overweight.scenario7.counts[32:40,])	
		
grid <- with(overweight.scenario7.counts, expand.grid(
   Year = 5:21
))
grid$Overweight<-stats::predict(model, newdata=grid)	
grid$scenario7<-"Breakfast 100%"
grid$Overweight[9:17] <- 	overweight.scenario7.counts$Overweight[32:40]
grid$CI <- overweight.scenario7.counts$CI[24:40]
grid$Lower <- grid$Overweight - grid$CI
grid$Upper <- grid$Overweight + grid$CI

overweight.scenario7.counts <- rbind(overweight.scenario7.counts, grid)

overweight.scenario7.counts  <- overweight.scenario7.counts [-c(21:23, 32:40),]

p1 <- ggplot(overweight.scenario7.counts, aes(x = Year, y=Overweight, colour  = scenario7)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

p1


gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
	
	gg_color_hue(4)
	
	
	
p1	+ geom_path(data = grid, size = 1, col = gg_color_hue(4)[1])+ geom_errorbar(limit, width=0.25, position=dodge)	
	
p1	+ geom_path(data = overweight.scenario7.counts[32:40,], size = 1)


pObese.scenario7.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario7$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,])

rownames(pObese.scenario7.counts) <- NULL	
	
pObese.scenario7.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		scenario7 = factor(rep(c("Breakfast 81%, Maternal Overweight+Obese 43%", "Breakfast 100%, Maternal Overweight+Obese 21%"), each = 20)),
		pObese.scenario7.counts)
								
names(pObese.scenario7.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario7.counts, aes(x = Year, y=pObese, colour  = scenario7)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, ncol=2)

ggplot(pObese.scenario7.counts, aes(x = Year, y=pObese, colour  = scenario7)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)+ stat_smooth(data = pObese.scenario7.counts[24:31,], fullrange = TRUE)








#######################################################
#Testing scenario
env.scenario1 <<- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
#env.scenario1$cat.adjustments$BREAST[1,]=c(0.01,rep( 0.0825,12))
env.scenario1$cat.adjustments$pregsmk[1,] = 
	c(0.99, 0.002, 0.002, 0.002, 0.002, 0.002)

sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario1$simulateP(10)

sfStop()

#############################################################################

env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario1$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1

pOverweight <- c(NA, 27.39165, 28.09076, 28.82803, 29.60347, 
					   30.41708, 31.26884, 32.15878, 33.08687, 
					   34.05314, 35.05756, 36.10015, 37.18091, 
					   38.29983, 39.45691, 40.65216, 41.88558, 
					   43.15716, 44.46690, 45.81481, 47.20088)		


overweight.scenario1.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario1$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,], 
	cbind(pOverweight, pOverweight, pOverweight)[-1,])

rownames(overweight.scenario1.counts) <- NULL	
	
overweight.scenario1.counts <- 
	data.frame(
		Year = rep(2:21,3), 
		scenario1 = factor(rep(c("Base", "Scenario", "NZHS"), each = 20)),
		overweight.scenario1.counts)
								
names(overweight.scenario1.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p1 <- ggplot(overweight.scenario1.counts, aes(x = Year, y=Overweight, colour  = scenario1)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)



		
 pObese <- c(NA, 8.032883,  8.241457,  8.485419,  8.764769,  9.079508,  9.429635,
				  9.815150, 10.236054, 10.692345,  11.184025, 11.711093, 12.273550,
				  12.871394, 13.504627, 14.173248, 14.877257, 15.616655, 16.391440,
				  17.201614, 18.047177)		   


pObese.scenario1.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario1$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,], 	
	cbind(pObese, pObese, pObese)[-1,])

rownames(pObese.scenario1.counts) <- NULL	
	
pObese.scenario1.counts <- 
	data.frame(
		Year = rep(2:21,3), 
		scenario1 = factor(rep(c("Base", "Scenario", "NZHS"), each = 20)),
		pObese.scenario1.counts)
								
names(pObese.scenario1.counts)[3:5] = c("pObese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 <- ggplot(pObese.scenario1.counts, aes(x = Year, y=pObese, colour  = scenario1)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


ml <-grid.arrange(p1, p2, ncol=2)
	  	
		
#######################################################
#Testing subgroup scenario
env.scenario <<- SimenvMELC$new()

subgroupExpression <- "r1stchildethnLvl2==1"
setGlobalSubgroupFilterExpression(subgroupExpression)	
	
env.scenario$cat.adjustments$pregsmk[1,] = 
	c(0.9, 0.02, 0.02, 0.02, 0.02, 0.02)

sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario$simulateP(10)

sfStop()

env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
env.scenario$modules$years1_21$run_results_collated$freqs_by_subgroup$z1OverweightLvl1

env.scenario$modules$years1_21$run_results_collated$freqs_by_subgroup_base_data$z1OverweightLvl1

env.scenario$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1

env.scenario$modules$years1_21$run_results_collated$freqs_base_data$z1OverweightLvl1

test <- tableBuilderMELC("Base", "frequencies", variableName= "z1OverweightLvl1", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl2==1", dict=env.base$dict)

test <- tableBuilderMELC("Base", "frequencies", variableName= "z1OverweightLvl1", grpbyName="", CI=TRUE, dict=env.base$dict)


test <- tableBuilder("", "frequencies", variableName= "z1OverweightLvl1", grpbyName="", CI=TRUE, logisetexpr="z1singleLvl1==1", dict=env.base$dict)



pOverweight <- c(NA, 27.39165, 28.09076, 28.82803, 29.60347, 
					   30.41708, 31.26884, 32.15878, 33.08687, 
					   34.05314, 35.05756, 36.10015, 37.18091, 
					   38.29983, 39.45691, 40.65216, 41.88558, 
					   43.15716, 44.46690, 45.81481, 47.20088)		




overweight.scenario.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,], 
	cbind(pOverweight, pOverweight, pOverweight)[-1,])

rownames(overweight.scenario.counts) <- NULL	
	
overweight.scenario.counts <- 
	data.frame(
		Year = rep(2:21,3), 
		Scenario = factor(rep(c("Base", "Scenario", "NZHS"), each = 20)),
		overweight.scenario.counts)
								
names(overweight.scenario.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

ggplot(overweight.scenario.counts, aes(x = Year, y=Overweight, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

  
###############################################################
  
asthma.scenario.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,],
	env.scenario$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,])

rownames(asthma.scenario.counts) <- NULL	
	
asthma.scenario.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Scenario = factor(rep(c("Base", "Scenario"), each = 20)),
		asthma.scenario.counts)
								
names(asthma.scenario.counts)[3:5] = c("Asthma", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)


p1 = ggplot(asthma.scenario.counts, aes(x = Year, y=Asthma, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

	

overweight.scenario.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

rownames(overweight.scenario.counts) <- NULL	
	
overweight.scenario.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Scenario = factor(rep(c("Base", "Scenario"), each = 20)),
		overweight.scenario.counts)
								
names(overweight.scenario.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 = ggplot(overweight.scenario.counts, aes(x = Year, y=Overweight, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

  
obese.scenario.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,])

rownames(obese.scenario.counts) <- NULL	
	
obese.scenario.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Scenario = factor(rep(c("Base", "Scenario"), each = 20)),
		obese.scenario.counts)
								
names(obese.scenario.counts)[3:5] = c("Obese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p3 = ggplot(obese.scenario.counts, aes(x = Year, y=Obese, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p2, p3, ncol=2)
		
##############################################################################


asthma.scenario3.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,],
	env.scenario3$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,])

rownames(asthma.scenario3.counts) <- NULL	
	
asthma.scenario3.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Scenario = factor(rep(c("Base", "Scenario"), each = 20)),
		asthma.scenario3.counts)
								
names(asthma.scenario3.counts)[3:5] = c("Asthma", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)


p1 = ggplot(asthma.scenario3.counts, aes(x = Year, y=Asthma, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

overweight.scenario3.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario3$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

rownames(overweight.scenario3.counts) <- NULL	
	
overweight.scenario3.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Scenario = factor(rep(c("Base", "Scenario"), each = 20)),
		overweight.scenario3.counts)
								
names(overweight.scenario3.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 = ggplot(overweight.scenario3.counts, aes(x = Year, y = Overweight, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

	 
  
obese.scenario3.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario3$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,])

rownames(obese.scenario3.counts) <- NULL	
	
obese.scenario3.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Scenario = factor(rep(c("Base", "Scenario"), each = 20)),
		obese.scenario3.counts)
								
names(obese.scenario3.counts)[3:5] = c("Obese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p3 = ggplot(obese.scenario3.counts, aes(x = Year, y=Obese, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, p3, ncol=1)
		
###################################################################################				


asthma.scenario2.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,],
	env.scenario2$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,],
	env.scenario3$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,])

rownames(asthma.scenario2.counts) <- NULL	
	
asthma.scenario2.counts <- 
	data.frame(
		Year = rep(2:21,3), 
		Scenario = factor(rep(c("Base", "NoPS/YesBF", "YesPS/NoBF"), each = 20)),
		asthma.scenario2.counts)
								
names(asthma.scenario2.counts)[3:5] = c("Asthma", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)


p1 = ggplot(asthma.scenario2.counts, aes(x = Year, y=Asthma, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

	

overweight.scenario2.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario2$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario3$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

rownames(overweight.scenario2.counts) <- NULL	
	
overweight.scenario2.counts <- 
	data.frame(
		Year = rep(2:21,3), 
		Scenario = factor(rep(c("Base", "NoPS/YesBF", "YesPS/NoBF"), each = 20)),
		overweight.scenario2.counts)
								
names(overweight.scenario2.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 = ggplot(overweight.scenario2.counts, aes(x = Year, y=Overweight, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

	 
  
obese.scenario2.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario2$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario3$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,])

rownames(obese.scenario2.counts) <- NULL	
	
obese.scenario2.counts <- 
	data.frame(
		Year = rep(2:21,3), 
		Scenario = factor(rep(c("Base", "NoPS/YesBF", "YesPS/NoBF"), each = 20)),
		obese.scenario2.counts)
								
names(obese.scenario2.counts)[3:5] = c("Obese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p3 = ggplot(obese.scenario2.counts, aes(x = Year, y=Obese, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, p3, ncol=1)

x11()
p1			
		
		
###############################################################################		





asthma.scenario3.counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,1],
				env.scenario3$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,1])

x11()
barplot(asthma.scenario3.counts, main="Incident asthma scenario3", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(asthma.scenario3.counts), beside=TRUE)
  
  
  

obese.scenario3.counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,1],
				env.scenario3$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,1])

x11()				
barplot(obese.scenario3.counts, main="Obese scenario3", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(obese.scenario3.counts), beside=TRUE)   
    
 
overweight.scenario3.counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1],
				env.scenario3$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1])

x11()				
barplot(overweight.scenario3.counts, main="Overweight scenario3", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(overweight.scenario3.counts), beside=TRUE)   
  
  
######################################################################################
 
  
overweight.scenario2.counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1],
				env.scenario2$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1])

x11()				
barplot(overweight.scenario2.counts , main="Overweight scenario2", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(counts), beside=TRUE)

 
asthma.scenario2.counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,1],
				env.scenario2$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,1])

x11()				
barplot(asthma.scenario2.counts, main="Incident asthma scenario2", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(counts), beside=TRUE)


obese.scenario2.counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,1],
				env.scenario2$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,1])

x11()				
barplot(obese.scenario2.counts, main="Incident asthma scenario2", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(obese.scenario2.counts), beside=TRUE)

    
  
############################################################################################# 
#Checking the odds ratio   
  
  for(i in 2:21){
	
	temp <- table( env.scenario1$modules$years1_21$outcomes$z1OverweightLvl1[,i], 
		env.scenario1$modules$years1_21$outcomes$asthma[,i])
	
	print((temp[1,1] * temp[2,2])/(temp[2,1] * temp[1,2]))
  }
  

  
  for(i in 2:21){
	
	temp <- table( env.scenario1$modules$years1_21$outcomes$z1ObeseLvl1[,i], 
		env.scenario1$modules$years1_21$outcomes$asthma[,i])
	
	print((temp[1,1] * temp[2,2])/(temp[2,1] * temp[1,2]))
  }

  
  



  for(i in 2:21){
	
	tempNew = env.base$modules$years1_21$outcomes$z1OverweightLvl1[,i] - 
				env.base$modules$years1_21$outcomes$z1ObeseLvl1[,i]
	
	temp <- table( tempNew, 
		env.base$modules$years1_21$outcomes$asthma[,i])
	
	print((temp[1,1] * temp[2,2])/(temp[2,1] * temp[1,2]))
  }


  
##########################################################




  
env.base$presim.stats$pre gsmk

 > env.base$presim.stats$pregsmk
      Mean (%)     Lower     Upper
0        78.34 77.198216 79.481784
1-5       7.76  7.018428  8.501572
6-10      4.84  4.245142  5.434858
11-15     5.24  4.622352  5.857648
16-20     1.50  1.163080  1.836920
21+       2.32  1.902737  2.737263
attr(,"meta")
  varname 
"pregsmk" 


sfInit(parallel=TRUE, cpus=4)


binbreaks<-binbreaks
models <- models
PropensityModels <- PropensityModels
catToContModels <- catToContModels

sfExport('binbreaks', 'models', 'PropensityModels', 'catToContModels')
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
#sfExportAll()	

env.scenario1 <<- SimenvMELC$new()
env.scenario1$cat.adjustments$pregsmk[1,] = c(1,0,0,0,0,0)

env.scenario1$simulate(10)

sfStop()


env.scenario1$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
  
env.scenario1$modules$years1_21$run_results_collated$freqs$asthmaLvl1
  

counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,1],
				env.scenario1$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,1])

barplot(counts, main="Incident asthma", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(counts), beside=TRUE)


counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1],
				env.scenario1$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1])

barplot(counts, main="Overweight", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(counts), beside=TRUE)
		

sfInit(parallel=TRUE, cpus=4)

binbreaks<-binbreaks
models <- models
PropensityModels <- PropensityModels
catToContModels <- catToContModels

sfExport('binbreaks', 'models', 'PropensityModels', 'catToContModels')
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
#sfExportAll()	

env.scenario3 <<- SimenvMELC$new()
env.scenario3$cat.adjustments$pregsmk[1,] = c(0,0,0,0,0,1)
env.scenario3$cat.adjustments$BREAST[1,] =  as.numeric(c(1, rep(0,12)))

env.scenario3$simulate(10)

sfStop()




env.scenario3$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
  
env.scenario3$modules$years1_21$run_results_collated$freqs$asthmaLvl1
  

counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1],
				env.scenario3$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1])

x11()
barplot(counts, main="Incident asthma", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(counts), beside=TRUE)

	

counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,1],
				env.scenario3$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,1])

x11()
barplot(counts, main="Incident asthma", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(counts), beside=TRUE)

		
	
	
	

counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,1],
				env.scenario3$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,1])

x11()
barplot(counts, main="Incident asthma", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(counts), beside=TRUE)

		
	
> env.base$presim.stats$BREAST
    Mean (%)     Lower     Upper
0      35.72 34.391821 37.048179
1      11.24 10.364503 12.115497
2       8.86  8.072348  9.647652
3       6.16  5.493581  6.826419
4       4.30  3.737719  4.862281
5       4.44  3.869057  5.010943
6       6.16  5.493581  6.826419
7       4.22  3.662741  4.777259
8       3.60  3.083640  4.116360
9       2.56  2.122224  2.997776
10      2.80  2.342727  3.257273
11      1.96  1.575769  2.344231
12+     7.98  7.228886  8.731114
attr(,"meta")
 varname 
"BREAST" 



sfInit(parallel=TRUE, cpus=4)

binbreaks<-binbreaks
models <- models
PropensityModels <- PropensityModels
catToContModels <- catToContModels

sfExport('binbreaks', 'models', 'PropensityModels', 'catToContModels')
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
#sfExportAll()	

env.scenario2 <<- SimenvMELC$new()
env.scenario2$cat.adjustments$pregsmk[1,] = c(1,0,0,0,0,0)
env.scenario2$cat.adjustments$BREAST[1,] = c(rep(0,12), 1)

env.scenario2$simulate(10)

sfStop()

detach(simframe)



env.scenario2$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
  
env.scenario2$modules$years1_21$run_results_collated$freqs$asthmaLvl1
  



mean(env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[,1], na.rm = TRUE)
mean(env.scenario1$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[,1], na.rm = TRUE)
mean(env.scenario2$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[,1], na.rm = TRUE)

mean(env.base$modules$years1_21$run_results_collated$freqs$asthma[,1], na.rm = TRUE)
mean(env.scenario1$modules$years1_21$run_results_collated$freqs$asthma[,1], na.rm = TRUE)
mean(env.scenario2$modules$years1_21$run_results_collated$freqs$asthma[,1], na.rm = TRUE)


counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1],
				env.scenario2$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,1])


barplot(counts, main="Overweight", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(counts), beside=TRUE)

	


counts <- rbind(env.base$modules$years1_21$run_results_collated$freqs$asthma[-1,1],
				env.scenario2$modules$years1_21$run_results_collated$freqs$asthma[-1,1])


barplot(counts, main="asthma", ylab = "Percentage",
  xlab="Age", col=c("darkblue","red"),
 	legend = rownames(counts), beside=TRUE)

		
	
################################################################################

modelCoef <-function(px, py, OR, n = 5000){
		
		f<-function(x)
		(px - (1-py))*n*x + x^2 - OR*(x^2 +(1-px)*(1-py)*n^2 - (2-px-py)*n*x)

		a <- uniroot(f, c(0,n))$root
		c <-(1-py)*n-a
		b <-(py-px)*n+c
		d <-px*n-c

		#intercept <- log(b/a)
		#coefOR <- log(d/c) -  log(b/a)
		
		c(a=a, b = b, c=c , d=d)
	}

	
pOverweight <- c(27.32536, 27.79471, 28.32322, 28.91090, 29.55774, 30.26374 ,
				31.02891, 31.85325, 32.73674, 33.67941, 34.68123, 35.74222 ,
				36.86237, 38.04169, 39.28017, 40.57782, 41.93463, 43.35060 ,
				44.82574, 46.36004, 47.95350 )/100			
				
pp1 <- env.base$presim.stats$pregsmk[1,1]/100
pp2 <- env.base$presim.stats$BREAST[1,1]/100
		
	
test = t(sapply(1:21, function(x) 	 modelCoef(1 - pp2, p[x],
			exp(as.numeric(models$z1Overweight$coefficients[2][1])))))
			
		
			
t(apply(test, 1, function(x) c(x[1]*.1/.36, x[2]*.9/.64)))
			
coef1 <- modelCoef(1 - pp2, p[1:21], 
			exp(as.numeric(models$z1Overweight$coefficients[2][1])))


base = cbind(apply(test[,c(1,2)],1, function(x) x[2]/sum(x)),
	apply(test[,c(3,4)],1, function(x) x[2]/sum(x)))

t(apply(base, 1, function(x) c(x[1]*.1/.36, x[2]*.9/.64)))
		


test = t(sapply(1:21, function(x) 	 modelCoef(1 - pp1, p[x],
			exp(as.numeric(models$z1Overweight$coefficients[3][1])))))
			
base = cbind(apply(test[,c(1,2)],1, function(x) x[2]/sum(x)),
	apply(test[,c(3,4)],1, function(x) x[2]/sum(x)))
	

	
base1 = t(sapply(2:21, function(x) modelCoef(base[x, 1], pAsthma[x],
				  exp(as.numeric(models$Asthma$coefficients[2][1])))))
				  
base2 = t(sapply(2:21, function(x) modelCoef(base[x, 2], pAsthma[x],
				  exp(as.numeric(models$Asthma$coefficients[2][1])))))


base = cbind(apply(base1[,c(1,2)],1, function(x) x[2]/sum(x)),
	apply(base1[,c(3,4)],1, function(x) x[2]/sum(x)))
		


###########################################################		

test = t(sapply(2:21, function(x) 	 modelCoef(1 - env.base$presim.stats$pregsmk[1,1]/100, 
			pAsthma[x],
		1.47*1.19)))

base = cbind(apply(test[,c(1,2)],1, function(x) x[2]/sum(x)),
	apply(test[,c(3,4)],1, function(x) x[2]/sum(x)))

base

	
#########################################################


  pOverweight <- c(27.32536, 27.79471, 28.32322, 28.91090, 29.55774, 30.26374 ,
					   31.02891, 31.85325, 32.73674, 33.67941, 34.68123, 35.74222 ,
					   36.86237, 38.04169, 39.28017, 40.57782, 41.93463, 43.35060 ,
					   44.82574, 46.36004, 47.95350 )/100
	  
	  pAsthma <-  c(NA, 12.883273, 13.767930, 14.520986, 15.142442, 15.632299, 15.990555, 
					16.217212, 16.312268, 16.275725, 16.107582, 15.807839, 15.376496, 
					14.813553, 14.119011, 13.292868, 12.335125, 11.245783, 10.024840,
					8.672298,  7.188156) /100		

test = t(sapply(2:21, function(x) 
				modelCoef(pOverweight[x], pAsthma[x],
				  exp(as.numeric(models$Asthma$coefficients[2][1])))))
				  
			  
				  
#########################################################



tableBuilder("Base", "frequencies", "welfareLvl1", "", CI=T, logisetexpr=NULL)

tableBuilder("Base", "means", "welfareLvl1", "", CI=TRUE, logisetexpr=NULL)

catvars <- c(getOutcomeVars(env.base$simframe, "categorical"), "SESBTH", "r1stchildethn", 
               "r1stmeduc", "r1stfeduc", "z1single0", "fage", 
               "z1single0Lvl1", "bthorder", "NPRESCH", "z1genderLvl1")
  contvars <- c(getOutcomeVars(env.base$simframe, "continuous"), "bwkg", "pregalc", 
                "ga", "INTERACT", "PUNISH", "MAGE", "pregsmk", "BREAST")
  


	varName = env.base$dict$descriptions

varName[catvars]

varName[contvars]

tableBuilder("Base", "frequencies", "r1stchildethn", "", CI=FALSE, logisetexpr=NULL)
tableBuilder("Base", "frequencies", "r1stchildethn", "z1genderLvl1", CI=TRUE, logisetexpr=NULL)

tt = tableBuilder("Base", "frequencies", "kids", "NPRESCH", CI=FALSE, logisetexpr=NULL)

tableBuilder("Base", "frequencies", "householdsize", "", CI=TRUE, logisetexpr=NULL)

tableBuilder("Base", "frequencies", "z1householdsizeLvl1", "", CI=TRUE, logisetexpr=NULL)


tableBuilder("Base", "quantiles", "kids", "fage", CI=TRUE, logisetexpr=NULL)


env.scenario <<- SimenvMELC$new()
subgroupExpression <- "mhrswrk<21"	##1

setGlobalSubgroupFilterExpression(subgroupExpression)

env.scenario$cat.adjustments$pregsmk[1,] = c(0,1,0,0,0,0)
env.scenario$cat.adjustments$BREAST[1,]=c(1,rep(0,12))

env.scenario$simulate(2)


####################################################################

env.base <<- SimenvMELC$new("Base", simframe=simframe.master)
	
	
source("simulateRun.R")

simframe <- simenv$simframe
	
	for(i in names(simframe))
		assign(i, simframe[,i])
	
	#browser()
	
    #attach(simenv$simframe, name="simframe")

#########################################
 
 env.base <<- SimenvMELC$new("Base", simframe=simframe.master)

 
 sfInit( parallel=TRUE, cpus=4 )
  env.base.list = as.list(env.base)

 start_time <- proc.time()
 
outcomes <- simulateRun(10, env.base.list)

proc.time() - start_time
		
######################################	
 
setwd("C:/Users/kcha193/workspace/MELC/data")

source("MELC.R")

env.base <<- SimenvMELC$new("Base", simframe=simframe.master)
	
env.base.list = as.list(env.base)

start_time = proc.time()	
sfSetMaxCPUs( number=32 )


sfInit(parallel=TRUE, slaveOutfile="test.txt" )

	binbreaks<-binbreaks
	models <- models
	PropensityModels <- PropensityModels
	catToContModels <- catToContModels

	dict.MELC.list <- as.list(dict.MELC)
	
	sfExport('binbreaks', 'models', 'PropensityModels', 
			'catToContModels','predSimBinomsSelect_notChangeScores', 'predSimBinomsSelect','predSimNormsSelect3Models',
			'children', 'NUM_ITERATIONS', 'dict.MELC.list')
	sfExport(list=ls(simarioFun$env), namespace= "simario")
		
	sfLibrary(Hmisc)
	sfLibrary(snowfall)
	#sfExportAll()	

	source("simulateRun.R")
	
	outcomes <-sfLapply(1:10, simulateRun, simenv=env.base.list)
		
	all_run_results <-sfLapply(1:10, map_outcomes_to_run_results,  
		simframe = env.base.list$simframe, 
		outcomes = outcomes, 
		cat.adjustments = env.base.list$cat.adjustments,
		dict.MELC = dict.MELC.list)		
	
	run_results_collated <- collate_all_run_results(all_run_results, 
			cat.adjustments = env.base.list$cat.adjustments,
			simframe = env.base.list$simframe, outcomes = outcomes[[1]])
	
	rm(env.base.list)
	
	env.base$num_runs_simulated <- 10
	env.base$modules[[1]]$outcomes <- outcomes
	env.base$modules[[1]]$all_run_results <- all_run_results
	env.base$modules[[1]]$run_results_collated <- run_results_collated
	
	
sfStop()
proc.time() - start_time

###########################################################################

env.scenario <<- SimenvMELC$new()

subgroupExpression <- "r1stchildethnLvl3==1 & welfareLvl1==1 & mhrswrk < 21"
setGlobalSubgroupFilterExpression(subgroupExpression)	
	

env.scenario$cat.adjustments$z1homeown[1,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[2,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[3,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[4,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[5,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[6,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[7,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[8,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[9,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[10,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[11,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[12,] <- c(1, 0)
env.scenario$cat.adjustments$z1homeown[13,] <- c(.99, .01)

if (!exists("propensities")) propensities <- NULL
		
valid.subgroup <- env.scenario$check.subgroup.expr()

if (valid.subgroup==1) {
	 env.scenario$applyAllCatAdjustmentsToSimframe(1, propensities)
} else if (valid.subgroup==0) {
	cat("Pre-simulation scenario adjustments cannot be made because the subgroup expression is not defined \n")
} else {
	stop("Check creation of valid.subgroup \n")
}

#at this point after adjusting continuous variables some values may be higher than 
#the limits set throughout the simulation - can fix here (rather than changing
#more deep down simario functions)
if (exists("limits")) {
	for (j in 1:length(limits)) {
		v <- env.scenario$simframe[[names(limits)[j]]]
		env.scenario$simframe[[names(limits)[j]]][v>limits[[j]]] <- limits[[j]]
	}
}

env.scenario$presim.stats <- env.scenario$generatePreSimulationStats(env.scenario$simframe)


env.scenario.list = as.list(env.scenario)

start_time = proc.time()	
sfSetMaxCPUs( number=32 )
sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
	sfExport('binbreaks', 'models', 'PropensityModels', 
			'catToContModels','predSimBinomsSelect_notChangeScores', 'predSimBinomsSelect','predSimNormsSelect3Models',
			'children', 'NUM_ITERATIONS', 'dict.MELC', 
			'check.subgroup.expr', 'env.base', 'prepend.paths')
				
	sfExport(list=ls(simarioFun$env), namespace= "simario")
		
	sfLibrary(Hmisc)
	sfLibrary(snowfall)
	sfLibrary(stringi)
	sfLibrary(stringr)
	#sfExportAll()	

	source("simulateRun.R")

	outcomes <-sfLapply(1:2, simulateRun, simenv=env.scenario.list)
		
	all_run_results <-sfLapply(1:2, map_outcomes_to_run_results,  
		simframe = env.scenario.list$simframe, 
		outcomes = outcomes, 
		cat.adjustments = env.scenario.list$cat.adjustments)		
	
	run_results_collated <- collate_all_run_results(all_run_results, 
			cat.adjustments = env.scenario.list$cat.adjustments,
			simframe = env.scenario.list$simframe, outcomes = outcomes[[1]])
	
	rm(env.scenario.list)
	
	env.scenario$num_runs_simulated <- 10
	env.scenario$modules[[1]]$outcomes <- outcomes
	env.scenario$modules[[1]]$all_run_results <- all_run_results
	env.scenario$modules[[1]]$run_results_collated <- run_results_collated

sfStop()
proc.time() - start_time

print(stripMeta(env.scenario$modules$years1_21$run_results_collated$freqs$z1single))
print(env.scenario$presim.stats$SESBTH)

#########################################################################

  
asthma.scenario.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,],
	env.scenario$modules$years1_21$run_results_collated$freqs$asthmaLvl1[-1,])

rownames(asthma.scenario.counts) <- NULL	
	
asthma.scenario.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Scenario = factor(rep(c("Base", "Scenario"), each = 20)),
		asthma.scenario.counts)
								
names(asthma.scenario.counts)[3:5] = c("Asthma", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)


p1 = ggplot(asthma.scenario.counts, aes(x = Year, y=Asthma, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

overweight.scenario.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,],
	env.scenario$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1[-1,])

rownames(overweight.scenario.counts) <- NULL	
	
overweight.scenario.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Scenario = factor(rep(c("Base", "Scenario"), each = 20)),
		overweight.scenario.counts)
								
names(overweight.scenario.counts)[3:5] = c("Overweight", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p2 = ggplot(overweight.scenario.counts, aes(x = Year, y=Overweight, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)

  
obese.scenario.counts <- rbind(
	env.base$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,],
	env.scenario$modules$years1_21$run_results_collated$freqs$z1ObeseLvl1[-1,])

rownames(obese.scenario.counts) <- NULL	
	
obese.scenario.counts <- 
	data.frame(
		Year = rep(2:21,2), 
		Scenario = factor(rep(c("Base", "Scenario"), each = 20)),
		obese.scenario.counts)
								
names(obese.scenario.counts)[3:5] = c("Obese", "Lower", "Upper")

limit <- aes(ymax = Upper, ymin=Lower)
dodge <- position_dodge(width=0.1)

p3 = ggplot(obese.scenario.counts, aes(x = Year, y=Obese, colour  = Scenario)) + 
		geom_path(size = 1)+ geom_errorbar(limit, width=0.25, position=dodge)


grid.arrange(p1, p2, p3, ncol=1)
		






