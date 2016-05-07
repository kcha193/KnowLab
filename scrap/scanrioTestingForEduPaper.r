

setwd("C:/Users/kcha193/workspace/MELC/data")

#source("MELC.R")

source("MELCBase.R")


################################################################


baseIQ <- env.base$modules$years1_21$run_results_collated$means$IQ[2:16,]
baseIQ

ethIQ <- tableBuilderMELC("Base", "means", "IQ", dict = env.base$dict, 
grpbyName = "r1stchildethn")[2:16,4:9]

ethIQ

	
	
sesIQ <- tableBuilderMELC("Base", "means", "IQ", dict = env.base$dict, 
grpbyName = "SESBTH")[2:16,7:9]

IQresults(sesIQ)


 
genderIQ <- tableBuilderMELC("Base", "means", "IQ", dict = env.base$dict, 
grpbyName = "z1genderLvl1")[2:16,]

IQresults(genderIQ)


###############################################################################	


baseScore1 <- env.base$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,]
	
ethScore1 <- tableBuilder("Base", "frequencies", "z1ScoreLvl1", dict = env.base$dict, grpbyName = "r1stchildethnLvl1")[17,10:12]
		
ethScore2 <- tableBuilder("Base", "frequencies", "z1ScoreLvl1", dict = env.base$dict, grpbyName = "r1stchildethnLvl2")[17,10:12]
		
ethScore3 <- tableBuilder("Base", "frequencies", "z1ScoreLvl1", dict = env.base$dict, grpbyName = "r1stchildethnLvl3")[17,10:12]
		
ethScore4 <- tableBuilder("Base", "frequencies", "z1ScoreLvl1", dict = env.base$dict, grpbyName = "r1stchildethnLvl4")[17,10:12]
	

	
sesScore <- tableBuilder("Base", "frequencies", "z1ScoreLvl1", dict = env.base$dict, grpbyName = "SESBTHLvl3")[17,10:12]
	sesScore
	
genderScore <- tableBuilder("Base", "frequencies", "z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "z1genderLvl0")


ethScore1
ethScore2
ethScore3
ethScore4

genderScore[17,4:6]; genderScore[17,10:12]


sesScore
	
###################################################################################	
	
table(env.base$simframe$z1HearingLvl1)/5000
	
env.scenario <<- SimenvMELC$new()
	
env.scenario$cat.adjustments$z1Hearing[1,] <- c(0.8,0.2 )	

sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'env.base','prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario$simulate(10)

sfStop()


envs$scenario1 <-env.scenario
	
env.base$modules$years1_21$run_results_collated$means$IQ[16,]	
env.base$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,]
	
env.scenario$modules$years1_21$run_results_collated$means$IQ[16,]	
env.scenario$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,]

	
ethIQ1 <- tableBuilderMELC("scenario1", "means", "IQ", dict = env.base$dict, 
grpbyName = "r1stchildethn")[2:16,4:9]
ethIQ1[15,]		
	
sesIQ1 <- tableBuilderMELC("scenario1", "means", "IQ", dict = env.base$dict, 
grpbyName = "SESBTH")[2:16,7:9]
sesIQ1[15,]	



env.base$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,] - 
env.scenario$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,]

	
ethScore11 <- tableBuilder("scenario1", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl2")[17,10:12]
	ethScore11
	
	
ethScore2-	ethScore11
	
	
ethScore21 <- tableBuilder("scenario1", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl3")[17,10:12]
	ethScore21	

	ethScore3-	ethScore21
	
	
sesScore1 <- tableBuilder("scenario1", 
"frequencies", "z1ScoreLvl1", 
dict = env.base$dict, grpbyName = "SESBTHLvl3")[17,10:12]
sesScore-	sesScore1
	

###################################################################################	
	
	
table(env.base$simframe$z1ECELvl1)/5000



env.scenario2 <<- SimenvMELC$new()
	
	
env.scenario2$cat.adjustments$z1ECE[1,]=c(0.2, 0.8)

sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario2$simulateP(10)

sfStop()

envs$scenario2 <-env.scenario2
	
env.scenario <<-env.scenario2
	

env.base$modules$years1_21$run_results_collated$means$IQ[2:16,]

	
ethIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "r1stchildethn")[2:16,4:9]
	
sesIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "SESBTH")[2:16,7:9]

env.scenario2$modules$years1_21$run_results_collated$means$IQ[2:16,] - 
	env.base$modules$years1_21$run_results_collated$means$IQ[2:16,]
	

ethIQ1[15,]		
sesIQ1[15,]	


ethScore11 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl2")

	
	
ethScore21 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl3")
	
	
sesScore1 <- tableBuilder("scenario1", 
"frequencies", "z1ScoreLvl1", 
dict = env.base$dict, grpbyName = "SESBTHLvl3")

env.scenario2$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,]
	
	ethScore11[17,10:12]
	ethScore21[17,10:12]	

sesScore1[17,10:12]
	
###################################################################################	
		
	
table(env.base$simframe$BREAST)/5000

env.scenario6 <- SimenvMELC$new()


env.scenario6$cat.adjustments$BREAST[1,]=c(0.18,
	table(env.base$simframe$BREAST)[2:12]/5000, 0.257)

sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr', 'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

set.seed(220)
env.scenario6$simulateP(10)

sfStop()

envs$scenario2 <-env.scenario6
	
env.scenario <<-env.scenario6
	


	
ethIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "r1stchildethn")[2:16,4:9]
	
sesIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "SESBTH")[2:16,7:9]


env.scenario6$modules$years1_21$run_results_collated$means$IQ[16,]	

ethIQ1[15,]		
sesIQ1[15,]	


ethScore11 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl2")

	
	
ethScore21 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl3")
	
	
sesScore1 <- tableBuilder("scenario1", 
"frequencies", "z1ScoreLvl1", 
dict = env.base$dict, grpbyName = "SESBTHLvl3")

env.scenario6$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,]
	
	ethScore11[17,10:12]
	ethScore21[17,10:12]	

sesScore1[17,10:12]
	
	
###################################################################################	
	
	
table(env.base$simframe$r1ParentEducLvl1)/5000

table(env.base$simframe$r1ParentEducLvl2)/5000
table(env.base$simframe$r1ParentEducLvl3)/5000

env.scenario6 <<- SimenvMELC$new()
	
	
env.scenario6$cat.adjustments$r1ParentEduc[1,]=c(0.50, 0.50, 0.00)


sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr',  'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario6$simulateP(10)

sfStop()

		
		
	

envs$scenario2 <-env.scenario6
	
env.scenario <<-env.scenario6
	


	
ethIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "r1stchildethn")[2:16,4:9]
	
sesIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "SESBTH")[2:16,7:9]


env.scenario6$modules$years1_21$run_results_collated$means$IQ[16,]	

ethIQ1[15,]		
sesIQ1[15,]	


ethScore11 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl2")

	
	
ethScore21 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl3")
	
	
sesScore1 <- tableBuilder("scenario1", 
"frequencies", "z1ScoreLvl1", 
dict = env.base$dict, grpbyName = "SESBTHLvl3")

env.scenario6$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,]
	
	ethScore11[17,10:12]
	ethScore21[17,10:12]	

sesScore1[17,10:12]
	
############################################################################	
	
table(env.base$simframe$r1ParentEducLvl1)/5000

table(env.base$simframe$r1ParentEducLvl2)/5000
table(env.base$simframe$r1ParentEducLvl3)/5000

env.scenario6 <<- SimenvMELC$new()
	
	
env.scenario6$cat.adjustments$r1ParentEduc[1,]=c(0.50, 0.50, 0.00)


sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr',  'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario6$simulateP(10)

sfStop()

		
		
	

envs$scenario2 <-env.scenario6
	
env.scenario <<-env.scenario6
	


	
ethIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "r1stchildethn")[2:16,4:9]
	
sesIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "SESBTH")[2:16,7:9]


env.scenario6$modules$years1_21$run_results_collated$means$IQ[16,]	

ethIQ1[15,]		
sesIQ1[15,]	


ethScore11 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl2")

	
	
ethScore21 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl3")
	
	
sesScore1 <- tableBuilder("scenario1", 
"frequencies", "z1ScoreLvl1", 
dict = env.base$dict, grpbyName = "SESBTHLvl3")

env.scenario6$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,]
	
	ethScore11[17,10:12]
	ethScore21[17,10:12]	

sesScore1[17,10:12]
	
		
#############################################################

	
env.scenario <<- SimenvMELC$new()
	
env.scenario$cat.adjustments$z1Hearing[1,] <- c(0.8,0.2 )	
env.scenario$cat.adjustments$BREAST[1,]=c(0.18,
	table(env.base$simframe$BREAST)[2:12]/5000, 0.257)

sfInit(parallel=TRUE, cpus = 4, slaveOutfile="test.txt" )

	
sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'dict.MELC', 
		'check.subgroup.expr',  'prepend.paths')
			
sfExport(list=ls(simarioFun$env), namespace= "simario")
	
sfLibrary(Hmisc)
sfLibrary(snowfall)
sfLibrary(stringi)
sfLibrary(stringr)

env.scenario$simulateP(10)

sfStop()



envs$scenario2 <-env.scenario
	
env.scenario <<-env.scenario
	
	
ethIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "r1stchildethn")[2:16,4:9]
	
sesIQ1 <- tableBuilderMELC("scenario2", "means", "IQ", dict = env.base$dict, 
grpbyName = "SESBTH")[2:16,7:9]


env.scenario$modules$years1_21$run_results_collated$means$IQ[16,]	

ethIQ1[15,]		
sesIQ1[15,]	


ethScore11 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl2")

	
	
ethScore21 <- tableBuilder("scenario2", "frequencies", 
"z1ScoreLvl1", dict = env.base$dict, 
grpbyName = "r1stchildethnLvl3")
	
	
sesScore1 <- tableBuilder("scenario1", 
"frequencies", "z1ScoreLvl1", 
dict = env.base$dict, grpbyName = "SESBTHLvl3")

env.scenario$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1[17,]
	
ethScore11[17,10:12]
ethScore21[17,10:12]	

sesScore1[17,10:12]
	
		
	
	
	
	
	
	
	
	
	
	
	