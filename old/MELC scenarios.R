# Specific MELC scenarios for output testing.
# 
# Author: oman002
###############################################################################

#' testScenario()
testScenario <- function() {
	env.scenario <<- SimenvMELC$new()
	# test changes at the beginning of the simulation
	env.scenario$cat.adjustments$SESBTH[1,] <- c(0.1,0.1,0.8)
	env.scenario$cat.adjustments$z1accom[1,] <- c(0.89,0.11)
	env.scenario$cat.adjustments$catpregsmk2[1,] <- c(0.01,0.02,0.03,0.04,0.90)
	
	env.scenario$cat.adjustments$z1single0[1,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[1,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[2,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[3,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[4,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[5,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[6,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[7,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[8,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[9,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[10,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[11,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[12,] <- c(1,0)
	env.scenario$cat.adjustments$z1single[13,] <- c(.999,.001)
	
	env.scenario$cat.adjustments$z1single0[1,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[1,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[2,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[3,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[4,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[5,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[6,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[7,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[8,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[9,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[10,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[11,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[12,] <- c(.999,.001)
	env.scenario$cat.adjustments$z1single[13,] <- c(.999,.001)

	# test a change during the simulation
	env.scenario$cat.adjustments$z1accom[8,] <- c(0.56,0.44)
	env.scenario$cat.adjustments$z1single[3,] <- c(0.8,0.2)
	
	#test a time-variant continuous variable
	env.scenario$cat.adjustments$fhrswrk[1,] <- c(.05, .1, .15, .2, .25, .2, .05) 
	env.scenario$cat.adjustments$fhrswrk[2,] <- c(.05, .1, .15, .2, .25, .2, .05) 
	
	env.scenario$cat.adjustments$mhrswrk[1,] <- c(.05, .1, .15, .2, .25, .25) 
	env.scenario$cat.adjustments$mhrswrk[3,] <- c(.05, .1, .15, .2, .25, .25)
	
	#with(Simenv, debug(applyCatAdjustmentToSimframe))
	env.scenario$simulate(2)

	# output
	print(stripMeta(env.scenario$modules$years1_13$run_results_collated$freqs$z1accomLvl1))
	print(env.scenario$presim.stats$SESBTH)
	print(env.scenario$presim.stats$catpregsmk2)
}

#' env.scenario <<- SimenvMELC$new()
#' 
#' subgroupExpression <- "mhrswrk<21"	##1
#' subgroupExpression <- "welfareLvl1 == 1"	##2
#' subgroupExpression <- "r1stchildethnLvl2 == 1"	##3
#' subgroupExpression <- "r1stchildethnLvl3==1"
#' subgroupExpression <- "r1stchildethnLvl3 == 1 & mhrswrk < 21"	##4
#' subgroupExpression <- "r1stchildethnLvl3==1 & welfareLvl1==1"	##5
#' subgroupExpression <- "welfareLvl1==1 & mhrswrk < 21"	##6
#' subgroupExpression <- "r1stchildethnLvl3==1 & welfareLvl1==1 & mhrswrk < 21"	##7
#' subgroupExpression <- "pregalc<1"	##8
#' subgroupExpression <- "r1stchildethnLvl2==1|r1stchildethnLvl3==1"	##9
#' subgroupExpression <- "bwkg<3|bwkg>4" 
#' subgroupExpression <- "BREAST==0|BREAST>=6"	##10 
#' subgroupExpression <- "BREAST==0"
#' subgroupExpression <- "alcabuseLvl1==1"	##11
#' subgroupExpression <- "alcabuseLvl1==1 & welfareLvl1==1"	##12
#' subgroupExpression <- "alcabuseLvl1==1 & r1stchildethnLvl3==1"	##13	
#' subgroupExpression <- "alcabuseLvl1==1 & mhrswrk < 21"	##14	
#' 
#' subgroupExpression <- "r1stchildethn == 3" #wrong
#' 
#' setGlobalSubgroupFilterExpression(subgroupExpression)
#' 
#' env.scenario$simulate(1)
#' env.scenario$simulate(2)
#' 
#' env.scenario$cat.adjustments$z1accom[1,] <- c(0.2, 0.8)	##1
#' env.scenario$cat.adjustments$z1accom[5,] <- c(0.2, 0.8)	##2
#'  env.scenario$cat.adjustments$z1accom[14,] <- c(0.2, 0.8)	##3
#' env.scenario$cat.adjustments$z1accom[18,] <- c(0.2, 0.8)	##4
#' env.scenario$cat.adjustments$z1accom[4,] <- c(0.4, 0.6)	##5
#' env.scenario$cat.adjustments$z1single[1,] <- c(0.3, 0.7)	##6
#' env.scenario$cat.adjustments$z1single[5,] <- c(0.3, 0.7)	##7
#' env.scenario$cat.adjustments$SESBTH[1,] <- c(.4, .3, .3)	##8
#' env.scenario$cat.adjustments$fhrswrk[2,] <- c(.05, .1, .15, .2, .25, .2, .05)	##9 
#' env.scenario$cat.adjustments$fhrswrk[1,] <- c(.05, .1, .15, .2, .25, .2, .05)	##10
#' env.scenario$cat.adjustments$mhrswrk[1,] <- c(.05, .1, .15, .2, .25, .25)	##11
#'  env.scenario$cat.adjustments$mhrswrk[2,] <- c(.05, .1, .15, .2, .25, .25)	##12
#' env.scenario$cat.adjustments$fhrswrk[3,] <- c(.05, .1, .15, .2, .25, .2, .05)	##13
#' env.scenario$cat.adjustments$fhrswrk[14,] <- c(.05, .1, .15, .2, .25, .2, .05)	##14
#' env.scenario$cat.adjustments$fhrswrk[18,] <- c(.05, .1, .15, .2, .25, .2, .05)	##15
#' env.scenario$cat.adjustments$chres[1,] <- c(.9, .1, 0)	##16
#' env.scenario$cat.adjustments$chres[2,] <- c(.9, .1, 0)	##17
#' env.scenario$cat.adjustments$chres[3,] <- c(.9, .1, 0)	##18
#' 
#' env.scenario$cat.adjustments$msmoke[3,] <- c(.25, .25, .25, .25)	##19
#' env.scenario$cat.adjustments$fsmoke[3,] <- c(.5, .3, .2, .1)	##20
#' env.scenario$cat.adjustments$kids[3,] <- rep(1/5, 5)	##21
#' env.scenario$cat.adjustments$z1chpar[5,] <- c(0.3, 0.7)	##22
#' env.scenario$cat.adjustments$welfare[5,] <- c(0.3, 0.7)	##23
#' env.scenario$cat.adjustments$z1homeown[4,] <- c(0.4, 0.6)	##24
#' env.scenario$cat.adjustments$z1overcrowd[4,] <- c(0.4, 0.6)	##25
#' 
#' env.scenario$cat.adjustments$MAGE[1,] <- c(.05, .1, .15, .2, .25, .25)	##26 
#' env.scenario$cat.adjustments$pregsmk[1,] <- c(.05, .1, .15, .2, .25, .25)	##27 
#' env.scenario$cat.adjustments$pregalc[1,] <- rep(1/9, 9)	##28 
#' env.scenario$cat.adjustments$bwkg[1,] <- rep(1/5, 5)	##29 
#' env.scenario$cat.adjustments$ga[1,] <- rep(1/5, 5)	##30
#' env.scenario$cat.adjustments$BREAST[1,] <- rep(1/13, 13)	##31
#' 
#' env.scenario$cat.adjustments$r1stmeduc[1,] <- rep(1/3, 3)	##32
#'  
#' env.scenario$cat.adjustments$z1single[1,] <- c(.99, 0.01)	##33
#' env.scenario$cat.adjustments$z1single[1,] <- c(0.01, .99)	##34
#'  
#' env.scenario$cat.adjustments$fhrswrk[1,] <- c(.01, .01, .01, .94, .01, .01, .01)	##35 
#' 
#' 	env.scenario$cat.adjustments$INTERACT[1,] <- c(0, 0, 0, 0, 0, 0, 0, 0, 1)	##36
#' 	env.scenario$cat.adjustments$INTERACT[1,] <- rep(1/9, 9)	##37
#' 	env.scenario$cat.adjustments$NPRESCH[1,] <- c(.1, .1, .6, .2)	##38
#' 
#' env.scenario$cat.adjustments$PUNISH[1,] <- rep(1/6, 6)	##39
#' 
#' env.scenario$cat.adjustments$BREAST[1,] <- rep(1/13, 13)	##40
#' env.scenario$cat.adjustments$INTERACT[1,] <- rep(1/9, 9)	##41
#' 
#' env.scenario$simulate(1)
#' env.scenario$simulate(2)

#' env.scenario <<- SimenvMELC$new()
#'  setScenarioLow()
#' env.scenario$simulate(100) ; env.scenario$simulate(2) env.scenario$simulate(1) 
#' (or use env$simulate(1) if manually stepping the function)
#' printCsvOutputsRoysPaper(env.scenario)
setScenarioLow <- function(env=env.scenario) {
	
	env$name <- "Low SESBTH, single-parent, low mother's education, MAGE = 19"
	#low SESBTH, single-parent, low mother's education
	env$cat.adjustments$SESBTH[1,] <- c(0,0,1)
	env$cat.adjustments$z1single[1,] <- c(0,1)
	env$cat.adjustments$z1single0[1,] <- c(0,1)
	env$cat.adjustments$r1stmeduc[1,] <- c(0,0,1)
	
	#young mother
	env$simframe$MAGE <- 19
	env$simframe$mage_years <- 19
	env$simframe$mage_years1 <- 19
	#set father's age to missing for all fathers (because everyone is in a single-mother family)
	#(FAGE not used in simulation)
	env$simframe$fage_years <- 99
	env$simframe$fage_years1 <- 99
	
}

#' setScenarioHigh()
#' env.scenario$simulate(100) ; env.scenario$simulate(2); env.scenario$simulate(1)
#' printCsvOutputsRoysPaper(env.scenario)
setScenarioHigh <- function(env=env.scenario) {
	
	env$name <- "High SESBTH, two-parent, high mother's education, MAGE = 45"
	#high SESBTH, two-parent, high mother's education
	env$cat.adjustments$SESBTH[1,] <- c(1,0,0)
	env$cat.adjustments$z1single[1,] <- c(1,0)
	env$cat.adjustments$z1single0[1,] <- c(1,0)
	env$cat.adjustments$r1stmeduc[1,] <- c(1,0,0)
	
	#old mother
	env$simframe$MAGE <- 45
	env$simframe$mage_years <- 45
	env$simframe$mage_years1 <- 45
	
	#change missing values for father's age to a value
	#(need to have ages for all fathers since all families are two-parent)
	#but in this method the fathers' ages span the whole range of ages, unlikely if all mother's were 45
	#could set all father's to be 47 years old?
	env$simframe$fage_years[env$simframe$fage_years==99] <- 
		env$simframe$fage_imputed[env$simframe$fage_years==99]
	env$simframe$fage_years1 <- env$simframe$fage_years
	
	#setting all fathers to be 47
	env$simframe$fage_years <- 47
	env$simframe$fage_years1 <- 47
}

singleScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$z1single0[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[13,] <- c(.99, .01)

	env.scenario$simulate(num.runs)
}

kidsScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$kids[1,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[2,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[3,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[4,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[5,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[6,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[7,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[8,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[9,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[10,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[11,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[12,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$kids[13,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$simulate(num.runs)
}

fhrswrkScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$fhrswrk[1,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[2,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[3,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[4,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[5,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[6,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[7,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[8,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[9,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[10,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[11,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[12,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$fhrswrk[13,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)

	env.scenario$simulate(num.runs)
}

welfareScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$welfare[1,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[2,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[3,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[4,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[5,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[6,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[7,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[8,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[9,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[10,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[11,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[12,] <- c(1, 0)
	env.scenario$cat.adjustments$welfare[13,] <- c(.99, .01)
	
	env.scenario$simulate(num.runs)
}

allStructuralScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$z1single0[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[1,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[1,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[1,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[1,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[2,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[2,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[2,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[2,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[3,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[3,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[3,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[3,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[4,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[4,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[4,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[4,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[5,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[5,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[5,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[5,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[6,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[6,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[6,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[6,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[7,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[7,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[7,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[7,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[8,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[8,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[8,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[8,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[9,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[9,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[9,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[9,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[10,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[10,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[10,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[10,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[11,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[11,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[11,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[11,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[12,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[12,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[12,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[12,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$kids[13,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[13,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[13,] <- c(.99, .01)
	
	env.scenario$simulate(num.runs)
}
		

z1accomScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$z1accom[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1accom[13,] <- c(.99, .01)
	
	env.scenario$simulate(num.runs)
}

z1homeownScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
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
	
	env.scenario$simulate(num.runs)
}

overcrowdScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$z1overcrowd[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[13,] <- c(.99, .01)
	
	env.scenario$simulate(num.runs)
}

z1chparScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$z1chpar[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[13,] <- c(.99, .01)
	
	env.scenario$simulate(num.runs)
}

chresScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$chres[1,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[2,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[3,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[4,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[5,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[6,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[7,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[8,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[9,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[10,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[11,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[12,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$chres[13,] <- c(.99, .01/2, .01/2)
	
	env.scenario$simulate(num.runs)
}

smokeScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$fsmoke[1,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[1,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[2,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[2,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[3,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[3,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[4,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[4,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[5,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[5,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[6,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[6,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[7,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[7,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[8,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[8,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[9,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[9,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[10,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[10,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[11,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[11,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[12,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[12,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$fsmoke[13,] <- c(.99, .01/3, .01/3, .01/3)
	env.scenario$cat.adjustments$msmoke[13,] <- c(.99, .01/3, .01/3, .01/3)
	
	env.scenario$simulate(num.runs)
}

allIntermediateScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	
	env.scenario$cat.adjustments$z1accom[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[1,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[1,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[1,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[1,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[2,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[2,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[2,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[2,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[3,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[3,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[3,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[3,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[4,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[4,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[4,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[4,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[5,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[5,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[5,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[5,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[6,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[6,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[6,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[6,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[7,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[7,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[7,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[7,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[8,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[8,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[8,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[8,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[9,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[9,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[9,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[9,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[10,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[10,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[10,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[10,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[11,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[11,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[11,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[11,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[12,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[12,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[12,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[12,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$z1homeown[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$z1overcrowd[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$z1chpar[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$chres[13,] <- c(.99, .01/2, .01/2)
	env.scenario$cat.adjustments$fsmoke[13,] <- c(.99, .01/3, .01/3, .01/3)
	env.scenario$cat.adjustments$msmoke[13,] <- c(.99, .01/3, .01/3, .01/3)
	
	env.scenario$simulate(num.runs)
}

allScenario <- function(num.runs) {
	env.scenario <<- SimenvMELC$new()
	env.scenario$cat.adjustments$z1single0[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1single[1,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[1,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[1,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[1,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[2,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[2,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[2,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[2,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[3,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[3,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[3,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[3,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[4,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[4,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[4,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[4,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[5,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[5,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[5,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[5,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[6,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[6,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[6,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[6,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[7,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[7,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[7,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[7,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[8,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[8,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[8,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[8,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[9,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[9,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[9,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[9,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[10,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[10,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[10,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[10,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[11,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[11,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[11,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[11,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[12,] <- c(1, 0)
	env.scenario$cat.adjustments$kids[12,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[12,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[12,] <- c(1, 0)
	
	env.scenario$cat.adjustments$z1single[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$kids[13,] <- c(.521345437, .4786546, 0, 0, 0)
	env.scenario$cat.adjustments$fhrswrk[13,] <- c(0, .003268, .0283224, .48366016, .157952, .171024, .155773)
	env.scenario$cat.adjustments$welfare[13,] <- c(.99, .01)
	
	
	env.scenario$cat.adjustments$z1accom[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[1,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[1,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[1,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[1,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[1,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[2,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[2,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[2,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[2,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[2,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[3,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[3,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[3,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[3,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[3,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[4,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[4,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[4,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[4,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[4,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[5,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[5,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[5,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[5,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[5,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[6,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[6,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[6,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[6,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[6,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[7,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[7,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[7,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[7,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[7,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[8,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[8,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[8,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[8,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[8,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[9,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[9,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[9,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[9,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[9,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[10,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[10,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[10,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[10,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[10,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[11,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[11,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[11,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[11,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[11,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1homeown[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1overcrowd[12,] <- c(1, 0)
	env.scenario$cat.adjustments$z1chpar[12,] <- c(1, 0)
	env.scenario$cat.adjustments$chres[12,] <- c(1, 0, 0)
	env.scenario$cat.adjustments$fsmoke[12,] <- c(1, 0, 0, 0)
	env.scenario$cat.adjustments$msmoke[12,] <- c(1, 0, 0, 0)
	
	env.scenario$cat.adjustments$z1accom[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$z1homeown[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$z1overcrowd[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$z1chpar[13,] <- c(.99, .01)
	env.scenario$cat.adjustments$chres[13,] <- c(.99, .01/2, .01/2)
	env.scenario$cat.adjustments$fsmoke[13,] <- c(.99, .01/3, .01/3, .01/3)
	env.scenario$cat.adjustments$msmoke[13,] <- c(.99, .01/3, .01/3, .01/3)
	
	env.scenario$simulate(num.runs)
}