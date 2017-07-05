

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
	
apply(results[4:20,],2, mean)

write.csv(compareFreq(env.base, Breakfast.scenario, "z1OverweightLvl1"), "compareBreakfastA5_21.csv")	
	
	
############################################################################
# Maternal Overweight + obesity ? halving to 21%

tableBuilderNew(env.base, "freq", "r1mBMI")

Maternal.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

Maternal.scenario$cat.adjustments$r1mBMI[1,] = 	c(0.77, 0.02, 0.135, 0.075)

Maternal.scenario <- simulateSimario(Maternal.scenario, 10, simulateKnowLab)

compareFreq(env.base, Maternal.scenario, "z1OverweightLvl1")

write.csv(compareFreq(env.base, Maternal.scenario, "z1OverweightLvl1"), "compareMaternalBMIA2_A9.csv")	

############################################################################
# Birthweight ? halving % with high birth weight

tableBuilderNew(env.base, "freq", "bwkg")

Birthweight.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

Birthweight.scenario$cat.adjustments$bwkg[1,] = c(0.041, 0.161, 0.336, 0.412, 0.05)

Birthweight.scenario <- simulateSimario(Birthweight.scenario, 10, simulateKnowLab)

compareFreq(env.base, Birthweight.scenario, "z1OverweightLvl1")

write.csv(compareFreq(env.base, Birthweight.scenario, "z1OverweightLvl1"), "compareBirthweightA2_A18.csv")	

# 2 to 18

############################################################################
# Mother smoked ? halving % that smoke
tableBuilderNew(env.base, "freq", "pregsmk")

pregsmk.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

pregsmk.scenario$cat.adjustments$pregsmk[1,] = c(0.90,0.02,0.02,0.02,0.02,0.02)

pregsmk.scenario <- simulateSimario(pregsmk.scenario, 10, simulateKnowLab)

compareFreq(env.base, pregsmk.scenario, "z1OverweightLvl1")

write.csv(compareFreq(env.base, pregsmk.scenario, "z1OverweightLvl1"), "comparePregsmkA3_A14.csv")	

############################################################################
# Breastfeeding ? halving % non breastfeeders
tableBuilderNew(env.base, "freq", "BREAST")

BREAST.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

BREAST.scenario$cat.adjustments$BREAST[1,]= c(0.18,rep(0.82/12, 12))

BREAST.scenario <- simulateSimario(BREAST.scenario, 10, simulateKnowLab)

compareFreq(env.base, BREAST.scenario, "z1OverweightLvl1")

write.csv(compareFreq(env.base, BREAST.scenario, "z1OverweightLvl1"), "compareBREASTA3_A14 halving non-breastfreeders.csv")	


############################################################################
# Parental education ? change from High 34% 58% 8%? ?to???high 50% 45% 5%

tableBuilderNew(env.base, "freq", "r1ParentEduc")

ParentEduc.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

ParentEduc.scenario$cat.adjustments$r1ParentEduc[1,]=c(0.5, 0.45, 0.05)

ParentEduc.scenario <- simulateSimario(ParentEduc.scenario, 10, simulateKnowLab)

compareFreq(env.base, ParentEduc.scenario, "z1OverweightLvl1")

write.csv(compareFreq(env.base, ParentEduc.scenario, "z1OverweightLvl1"), "comparer1ParentEduc2_A15.csv")	

############################################################################
# Sleep 

tableBuilderNew(env.base, "freq", "r1Sleep")[,1:3] %>% spread(Var, Mean)

oldLong <-
tableBuilderNew(env.base, "freq", "r1Sleep") %>% 
  filter(Var == "Long") %>% select(Mean)

Sleep.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

Sleep.scenario$cat.adjustments$r1Sleep[,1] = c(NA, rep(0.01, 18), NA, NA)
Sleep.scenario$cat.adjustments$r1Sleep[,3] = c(NA, oldLong$Mean/100, NA, NA)
Sleep.scenario$cat.adjustments$r1Sleep[,2] =
  apply(Sleep.scenario$cat.adjustments$r1Sleep, 1, function(x) 1 - x[1] - x[3])

Sleep.scenario <- simulateSimario(Sleep.scenario, 10, simulateKnowLab)

results <- compareFreq(env.base, Sleep.scenario, "z1OverweightLvl1")

apply(results[2:19,],2, mean)

write.csv(compareFreq(env.base, Sleep.scenario, "z1OverweightLvl1"), "comparer1SleepA2_A19.csv")	

############################################################################
# Watch tv 45 to 20

tableBuilderNew(env.base, "freq", "z1WatchTVLvl1")

WatchTV.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

WatchTV.scenario$cat.adjustments$z1WatchTVLvl1[1,]=c(0.8, 0.2)

WatchTV.scenario <- simulateSimario(WatchTV.scenario, 10, simulateKnowLab)

compareFreq(env.base, WatchTV.scenario, "z1OverweightLvl1")

write.csv(compareFreq(env.base, WatchTV.scenario, "z1OverweightLvl1"), "comparer1WatchTVA3_A12.csv")	

############################################################################
# Mother smoked ? halving % that smoke + Breastfeeding ? halving % non breastfeeders
pregsmkBreast.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

pregsmkBreast.scenario$cat.adjustments$pregsmk[1,] = c(0.90,0.02,0.02,0.02,0.02,0.02)
pregsmkBreast.scenario$cat.adjustments$BREAST[1,]= c(0.18,rep(0.82/12, 12))

pregsmkBreast.scenario <- simulateSimario(pregsmkBreast.scenario, 10, simulateKnowLab)

compareFreq(env.base, pregsmkBreast.scenario, "z1OverweightLvl1")

write.csv(compareFreq(env.base, pregsmkBreast.scenario, "z1OverweightLvl1"), 
          "comparepregsmkBreastA3_A14.csv")	

#######################################################################
# Breakfast from 81 to 95%
BreakfastSleepWatchTV.scenario <-
  createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

oldLong <-
  tableBuilderNew(env.base, "freq", "r1Sleep") %>% 
  filter(Var == "Long") %>% select(Mean)

BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[,1] = c(NA, rep(0.01, 18), NA, NA)
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[,3] = c(NA, oldLong$Mean/100, NA, NA)
BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep[,2] =
  apply(BreakfastSleepWatchTV.scenario$cat.adjustments$r1Sleep, 1, function(x) 1 - x[1] - x[3])

BreakfastSleepWatchTV.scenario$cat.adjustments$z1Breakfast[1,] = c(0.05, 0.95)

BreakfastSleepWatchTV.scenario$cat.adjustments$z1WatchTVLvl1[1,]=c(0.8, 0.2)

BreakfastSleepWatchTV.scenario <- simulateSimario(BreakfastSleepWatchTV.scenario, 10, simulateKnowLab)

compareFreq(env.base, BreakfastSleepWatchTV.scenario, "z1OverweightLvl1")

write.csv(compareFreq(env.base, BreakfastSleepWatchTV.scenario, "z1OverweightLvl1"),
          "compareBreakfastSleepWatchTVfastA5_12.csv")	























































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








