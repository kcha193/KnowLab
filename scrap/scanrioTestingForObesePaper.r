

########################################################################

codings <- dict.MELC$codings

Gender
> env.base$presim.stats$z1gender
       Mean (%)    Lower    Upper
Female    51.02 49.63438 52.40562
Male      48.98 47.59438 50.36562
attr(,"meta")
   varname 
"z1gender" 


#ethnicity 
> env.base$presim.stats$r1stchildethn
        Mean (%)     Lower     Upper
NZ Euro    57.66 56.290456 59.029544
Maori      24.94 23.740734 26.139266
Pacific     8.92  8.129946  9.710054
Asian       8.48  7.707819  9.252181
attr(,"meta")
        varname 
"r1stchildethn" 



#Parental education
table.catvar.with.CI(env.base$simframe$r1ParentEduc, codings$r1stmeduc)

                        Mean (%)     Lower     Upper
Tertiary                    33.84 32.528477 35.151523
Secondary                   58.60 57.234750 59.965250
No formal qualifications     7.56  6.827253  8.292747


#Breastfeeding
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

#In Two levels
table.catvar.with.CI(binary.levels.combine(env.base$simframe$z1breastLvl0,
						env.base$simframe$z1breastLvl1), codings$z1breastLvl1 + 1)
						
         Mean (%)    Lower    Upper
None         35.72 34.39182 37.04818
Breastfed    64.28 62.95182 65.60818
attr(,"meta")
       varname 
"z1breastLvl1" 
						

#Smoking in pregnancy
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

#In Two levels
table.catvar.with.CI(env.base$simframe$z1pregsmkLvl1, codings$z1pregsmkLvl1 )
                        Mean (%)    Lower    Upper
No smoking in pregnancy    78.34 77.19822 79.48178
Smoking in pregnancy       21.66 20.51822 22.80178


#Birthweight
> summary(env.base$simframe$bwkg)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.875   3.080   3.500   3.452   3.840   5.400 

> env.base$presim.stats$bwkg
          Mean (%)     Lower     Upper
<2500         4.08  3.531663  4.628337
2500-2999    16.08 15.061787 17.098213
3000-3499    29.62 28.354447 30.885553
3500-3999    34.18 32.865296 35.494704
4000+        16.04 15.022811 17.057189


######################################################################################


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


#######################################################################
# Breakfast from 81 to 95%
Breakfast.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

tableBuilderNew(env.base, "freq", "z1BreakfastLvl1")


Breakfast.scenario$cat.adjustments$z1Breakfast[1,] = c(0.05, 0.95)

Breakfast.scenario <- simulateSimario(Breakfast.scenario, 10, simulateKnowLab)

results <- compareFreq(env.base, Breakfast.scenario, "z1OverweightLvl1")

results

apply(results[4:20,],2, mean)

write.csv(compareFreq(env.base, Breakfast.scenario, "z1OverweightLvl1"), "compareBreakfastA5_21.csv")	
	
	
############################################################################
# Maternal Overweight + obesity ? halving to 21%

tableBuilderNew(env.base, "freq", "r1mBMI")

Maternal.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

Maternal.scenario$cat.adjustments$r1mBMI[1,] = 	c(0.77, 0.02, 0.135, 0.075)

Maternal.scenario <- simulateSimario(Maternal.scenario, 10, simulateKnowLab)

results <- compareFreq(env.base, Maternal.scenario, "z1OverweightLvl1")

apply(results[1:11,],2, mean)

write.csv(compareFreq(env.base, Maternal.scenario, "z1OverweightLvl1"), "compareMaternalBMIA2_A12.csv")	

############################################################################
# Birthweight ? halving % with high birth weight

tableBuilderNew(env.base, "freq", "bwkg")

Birthweight.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

Birthweight.scenario$cat.adjustments$bwkg[1,] = c(0.041, 0.161, 0.336, 0.412, 0.05)

Birthweight.scenario <- simulateSimario(Birthweight.scenario, 10, simulateKnowLab)

results <- compareFreq(env.base, Birthweight.scenario, "z1OverweightLvl1")
apply(results[1:17,],2, mean)


write.csv(compareFreq(env.base, Birthweight.scenario, "z1OverweightLvl1"), "compareBirthweightA2_A18.csv")	

# 2 to 18

############################################################################
# Reducing smoking during pregnancy from 21.7% to 16.5%
tableBuilderNew(env.base, "freq", "pregsmk")

pregsmk.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

pregsmk.scenario$cat.adjustments$pregsmk[1,] = c(1-0.165 , rep(0.165/5, 5))

pregsmk.scenario <- simulateSimario(pregsmk.scenario, 10, simulateKnowLab)

(results <- compareFreq(env.base, pregsmk.scenario, "z1OverweightLvl1"))
apply(results[2:13,],2, mean)

write.csv(compareFreq(env.base, pregsmk.scenario, "z1OverweightLvl1"), "comparePregsmkA3_A14.csv")	

############################################################################
# Breastfeeding ? halving % non breastfeeders
tableBuilderNew(env.base, "freq", "BREAST")

BREAST.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

BREAST.scenario$cat.adjustments$BREAST[1,]= c(0.26, rep(0.74/12, 12))

BREAST.scenario <- simulateSimario(BREAST.scenario, 10, simulateKnowLab)

(results <- compareFreq(env.base, BREAST.scenario, "z1OverweightLvl1"))
apply(results[2:13,],2, mean)

write.csv(compareFreq(env.base, BREAST.scenario, "z1OverweightLvl1"), "compareBREASTA3_A14.csv")	


############################################################################
# Parental education ? change from High 34% 58% 8%? ?to???high 50% 45% 5%

tableBuilderNew(env.base, "freq", "r1ParentEduc")

ParentEduc.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

ParentEduc.scenario$cat.adjustments$r1ParentEduc[1,]=c(0.5, 0.45, 0.05)

ParentEduc.scenario <- simulateSimario(ParentEduc.scenario, 10, simulateKnowLab)

(results <- compareFreq(env.base, ParentEduc.scenario, "z1OverweightLvl1"))
apply(results[1:14,],2, mean)

write.csv(compareFreq(env.base, ParentEduc.scenario, "z1OverweightLvl1"), "comparer1ParentEduc2_A15.csv")	

############################################################################
# Sleep 

# Year Short Normal Long
# 1     2   6.2   90.1  3.7
# 2     3   6.3   90.0  3.7
# 3     4   6.7   89.7  3.6
# 4     5   6.7   89.7  3.6
# 5     6   7.7   89.1  3.2
# 6     7   8.0   88.8  3.2
# 7     8   4.8   90.3  4.9
# 8     9   4.7   90.6  4.7
# 9    10   7.2   89.7  3.0
# 10   11   7.2   89.8  3.1
# 11   12   7.2   89.6  3.2
# 12   13   7.3   89.8  2.9
# 13   14   7.3   89.7  3.0
# 14   15   7.3   89.8  2.9
# 15   16   7.2   89.7  3.1
# 16   17   7.2   89.8  3.1
# 17   18   7.3   89.6  3.1
# 18   19   7.0   89.9  3.1

tableBuilderNew(env.base, "freq", "r1Sleep")[,1:3] %>% spread(Var, Mean)

oldLong <-
tableBuilderNew(env.base, "freq", "r1Sleep") %>% 
  filter(Var == "Long") %>% select(Mean)

Sleep.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

Sleep.scenario$cat.adjustments$r1Sleep[2,] <- c(0.0256824, 0.8871718, 0.0871458 )
Sleep.scenario$cat.adjustments$r1Sleep[3,] <- c(0.0256824, 0.8871718, 0.0871458 )
Sleep.scenario$cat.adjustments$r1Sleep[4,] <- c(0.0276766, 0.8887486, 0.0835748 )
Sleep.scenario$cat.adjustments$r1Sleep[5,] <- c(0.0276766, 0.8887486, 0.0835748 )
Sleep.scenario$cat.adjustments$r1Sleep[6,] <- c(0.0339050, 0.8894452, 0.0766498 )
Sleep.scenario$cat.adjustments$r1Sleep[7,] <- c(0.0339050, 0.8894452, 0.0766498 )
Sleep.scenario$cat.adjustments$r1Sleep[8,] <- c(0.0183554, 0.8754684, 0.1061762 )
Sleep.scenario$cat.adjustments$r1Sleep[9,] <- c(0.0183554, 0.8754684, 0.1061762 )
Sleep.scenario$cat.adjustments$r1Sleep[10:19,] <- rep(c(0.0301278, 0.8969764, 0.0728958), each = 10) 

Sleep.scenario <- simulateSimario(Sleep.scenario, 10, simulateKnowLab)

(results <- compareFreq(env.base, Sleep.scenario, "z1OverweightLvl1"))

apply(results[1:18,],2, mean)

write.csv(compareFreq(env.base, Sleep.scenario, "z1OverweightLvl1"), "comparer1SleepA2_A19.csv")	

############################################################################
# Watch tv 45 to 20

tableBuilderNew(env.base, "freq", "z1WatchTVLvl1")

WatchTV.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

WatchTV.scenario$cat.adjustments$z1WatchTVLvl1[1,]=c(0.8, 0.2)

WatchTV.scenario <- simulateSimario(WatchTV.scenario, 10, simulateKnowLab)

(results <- compareFreq(env.base, WatchTV.scenario, "z1OverweightLvl1"))
apply(results[2:11,],2, mean)

write.csv(compareFreq(env.base, WatchTV.scenario, "z1OverweightLvl1"), "comparer1WatchTVA3_A12.csv")	

############################################################################
# Mother smoked ? halving % that smoke + Breastfeeding ? halving % non breastfeeders
pregsmkBreast.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

pregsmkBreast.scenario$cat.adjustments$pregsmk[1,] = c(1-0.165 , rep(0.165/5, 5))
pregsmkBreast.scenario$cat.adjustments$BREAST[1,]= c(0.26, rep(0.74/12, 12))

pregsmkBreast.scenario <- simulateSimario(pregsmkBreast.scenario, 10, simulateKnowLab)

(results <- compareFreq(env.base, pregsmkBreast.scenario, "z1OverweightLvl1"))
apply(results[2:13,],2, mean)

write.csv(compareFreq(env.base, pregsmkBreast.scenario, "z1OverweightLvl1"), 
          "comparepregsmkBreastA3_A14.csv")	

#######################################################################
# Breakfast from 81 to 95%
BreakfastSleepWatchTV.scenario <-
  createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")


BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[2,] <- c(0.0256824, 0.8871718, 0.0871458 )
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[3,] <- c(0.0256824, 0.8871718, 0.0871458 )
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[4,] <- c(0.0276766, 0.8887486, 0.0835748 )
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[5,] <- c(0.0276766, 0.8887486, 0.0835748 )
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[6,] <- c(0.0339050, 0.8894452, 0.0766498 )
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[7,] <- c(0.0339050, 0.8894452, 0.0766498 )
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[8,] <- c(0.0183554, 0.8754684, 0.1061762 )
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[9,] <- c(0.0183554, 0.8754684, 0.1061762 )
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[10:19,] <- rep(c(0.0301278, 0.8969764, 0.0728958), each = 10) 

BreakfastSleepWatchTV.scenario$cat.adjustments$z1Breakfast[1,] = c(0.05, 0.95)

BreakfastSleepWatchTV.scenario$cat.adjustments$z1WatchTVLvl1[1,]=c(0.8, 0.2)

BreakfastSleepWatchTV.scenario <- simulateSimario(BreakfastSleepWatchTV.scenario, 10, simulateKnowLab)

(results <- compareFreq(env.base, BreakfastSleepWatchTV.scenario, "z1OverweightLvl1"))
apply(results[4:11,],2, mean)

write.csv(compareFreq(env.base, BreakfastSleepWatchTV.scenario, "z1OverweightLvl1"),
          "compareBreakfastSleepWatchTVfastA5_12.csv")	



#########################################

sleepTime <- numeric(5000000)
r1Sleep <- numeric(5000000)


increaseMin <- 0.42*0.6


z1genderLvl1 <- env.base$simframe$z1genderLvl1

#Age 2 and 3  ########		

sleepTime[z1genderLvl1 == 0] <- rnorm(sum(z1genderLvl1 == 0)*1000, 10.5 + increaseMin, 0.6)
sleepTime[z1genderLvl1 == 1] <- rnorm(sum(z1genderLvl1 == 1)*1000, 10.5 - 9/60 + increaseMin, 0.6)

r1Sleep[sleepTime>11.5] <- 3
r1Sleep[sleepTime<=11.5 & sleepTime>=9.5] <- 2
r1Sleep[sleepTime<9.5] <- 1
prop.table(table(r1Sleep))

#Age 4 and 5 ########		
  
  
  sleepTime[z1genderLvl1 == 0] <- rnorm(sum(z1genderLvl1 == 0)*1000, 12 + increaseMin, 0.6)
  sleepTime[z1genderLvl1 == 1] <- rnorm(sum(z1genderLvl1 == 1)*1000, 12 - 11/60 + increaseMin, 0.6)
  
  
  r1Sleep[sleepTime>13] <- 3
  r1Sleep[sleepTime<=13 & sleepTime>=11] <- 2
  r1Sleep[sleepTime<11] <- 1	
  
  prop.table(table(r1Sleep))
  
#Age 6 and 7 ########		
  		
  
  sleepTime[z1genderLvl1 == 0] <- rnorm(sum(z1genderLvl1 == 0)*1000, 10 + increaseMin, 0.6)
  sleepTime[z1genderLvl1 == 1] <- rnorm(sum(z1genderLvl1 == 1)*1000, 10 - 16 /60 + increaseMin, 0.6)
  
  
  r1Sleep[sleepTime>11] <- 3
  r1Sleep[sleepTime<=11& sleepTime>=9] <- 2
  r1Sleep[sleepTime<9] <- 1
  
  prop.table(table(r1Sleep))
  
#Age 8 and 9 ########				

sleepTime<- rnorm(5000*1000, 10 + increaseMin, 0.6)	


r1Sleep[sleepTime>11] <- 3
r1Sleep[sleepTime<=11 & sleepTime>=9] <- 2
r1Sleep[sleepTime<9] <- 1
  
  
prop.table(table(r1Sleep))


#Age 10 and 19 ########			
  
sleepTime <- rnorm(5000*1000, 8.875+ increaseMin, 0.6)

r1Sleep[sleepTime>10] <- 3
r1Sleep[sleepTime<=10 & sleepTime>=8] <- 2
r1Sleep[sleepTime<8] <- 1	


prop.table(table(r1Sleep))

















#########################


























env.scenario8 <- SimenvMELC$new()

attach(env.scenario8$simframe, name="simframe")

for( iteration in 2:18){ 

	sleepTime <- get(paste("SleepA", iteration, sep = ""))

	x <- numeric(5000)

  if(iteration>=2 & iteration<=3) {	
		
		sleepTime[sleepTime < quantile(sleepTime)[2]] = sleepTime[sleepTime < quantile(sleepTime)[2]] + 0.5
		
		x[sleepTime>11.5] <- 3
		x[sleepTime<=11.5 & sleepTime>=9.5] <- 2
		x[sleepTime<9.5] <- 1
			
			
		} else if(iteration>=4 & iteration<=5) {			
			
					
		sleepTime[sleepTime < quantile(sleepTime)[2]] = sleepTime[sleepTime < quantile(sleepTime)[2]] + 0.5
				
		x[sleepTime>13] <- 3
		x[sleepTime<=13 & sleepTime>=11] <- 2
		x[sleepTime<11] <- 1	
			
			
		} else if(iteration>=6 & iteration<=7) {			
	
	

		sleepTime[sleepTime < quantile(sleepTime)[2]] = sleepTime[sleepTime < quantile(sleepTime)[2]] + 0.5
			
		x[sleepTime>11] <- 3
		x[sleepTime<=11& sleepTime>=9] <- 2
		x[sleepTime<9] <- 1
		
	
			
	  } else if(iteration>=8 & iteration<=9) {			
		#browser()
		
		sleepTime[sleepTime < quantile(sleepTime)[2]] = sleepTime[sleepTime < quantile(sleepTime)[2]] + 0.5
				
		x[sleepTime>11] <- 3
		x[sleepTime<=11 & sleepTime>=9] <- 2
		x[sleepTime<9] <- 1

			
	  } else if(iteration>=10 & iteration<=19) {			
		#browser()
		
		
		sleepTime[sleepTime < quantile(sleepTime)[2]] = sleepTime[sleepTime < quantile(sleepTime)[2]] + 0.5
				
		x[sleepTime>10] <- 3
		x[sleepTime<=10 & sleepTime>=8] <- 2
		x[sleepTime<8] <- 1	
			
	  } 	  
	  	 	  
			  
		print(table(x) / 5000	  )
		
	  env.scenario8$simframe[[paste("r1SleepA", iteration, sep = "")]] = x

}

detach(simframe)



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

set.seed(417)
env.scenario8$simulateP(10)

sfStop()


compare(env.base, env.scenario8)




write.csv(compare(env.base, env.scenario8), "comparer1SleepA2_A18.csv")	








