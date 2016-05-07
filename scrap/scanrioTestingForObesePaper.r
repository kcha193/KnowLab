

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


setwd("C:/Users/kcha193/workspace/MELC/data")

#source("MELC.R")

source("MELCBase.R")

env.base$modules$years1_21$run_results_collated$freqs$z1OverweightLvl1
	
load("base/FullBaseRun.rData") # loads env.base object 
	
Base <-	
	sapply(env.base$modules$years1_21$run_results, function(x) sapply(x$freqs$z1OverweightLvl1[-1], function(y) y[2]/5000))	
	

x <- t(apply(Base, 1, function(x) cumsum(x)/(1:10)))	

plot(1:10, x[1,], type = "l", ylim = c(0.25, 0.5))
for(i in 2:20) lines(1:10, 	x[i,])


 round(t(apply(Base, 1, function(x) diff(cumsum(x)/(1:10)))	), 5)

 

compare = 
function(env.base, env.scenario){
	
	Base <-	
	sapply(env.base$modules$years1_21$run_results, function(x) sapply(x$freqs$z1OverweightLvl1[-1], function(y) y[2]/5000))	

	Scenario <-
	sapply(env.scenario$modules$years1_21$run_results, function(x) sapply(x$freqs$z1OverweightLvl1[-1], function(y) y[2]/5000))	

	results <- numeric(3)

	for(i in 1:20){
		lm.fit <- lm(c(Base[i,], Scenario[i,]) ~ factor(rep(c("B", "S"), each = 10)))

		results <- 
		rbind(results,c(summary(lm.fit )$coef[2,1],confint(	lm.fit )[2,]))
	}

	results <- cbind((1:21)/100, results)
	
	colnames(results) <- c("Age", "Mean Diff", "Lower CI", "Upper CI")
	results <- results[-1,]
			
	round(results*100, 4)	
}


#######################################################################
# Breakfast from 81 to 95%
env.scenario <- SimenvMELC$new()
	
env.scenario$cat.adjustments$z1Breakfast[1,] = c(0.05, 0.95)

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

set.seed(1125)
env.scenario$simulateP(10)

sfStop()
	
compare(env.base, env.scenario)
	
	
write.csv(compare(env.base, env.scenario), "compareBreakfastA5_21.csv")	
	
	
############################################################################
# Maternal Overweight + obesity – halving to 21%

env.scenario3 <- SimenvMELC$new()

env.scenario3$cat.adjustments$r1mBMI[1,] = 	c(0.77, 0.02, 0.135, 0.075)

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

set.seed(1136)
env.scenario3$simulateP(10)

sfStop()


compare(env.base, env.scenario3)
	
write.csv(compare(env.base, env.scenario3), "compareMaternalBMIA2_A12.csv")	
	


############################################################################
# Birthweight – halving % with high birth weight

env.scenario4 <- SimenvMELC$new()

env.scenario4$cat.adjustments$z1HighBw[1,] = 	c(0.92, 0.08)

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

set.seed(1226)
env.scenario4$simulateP(10)

sfStop()


compare(env.base, env.scenario4)
	
write.csv(compare(env.base, env.scenario4), "compareBirthweightA2_A18.csv")	

# 2 to 18

############################################################################
# Mother smoked – halving % that smoke
env.scenario5 <- SimenvMELC$new()

#subgroupExpression <- "r1stchildethnLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
	
#env.scenario1$cat.adjustments$BREAST[1,]=c(0.01,rep( 0.0825,12))
env.scenario5$cat.adjustments$pregsmk[1,] = c(0.90,0.02,0.02,0.02,0.02,0.02)


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

set.seed(1242)
env.scenario5$simulateP(10)

sfStop()


compare(env.base, env.scenario5)
	
write.csv(compare(env.base, env.scenario5), "comparePregsmkA3_A14.csv")	


############################################################################
# Breastfeeding – halving % non breastfeeders
env.scenario6 <- SimenvMELC$new()

env.scenario6$cat.adjustments$BREAST[1,]=c(0.18,rep(0.82/12, 12))


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

set.seed(220)
env.scenario6$simulateP(10)

sfStop()


compare(env.base, env.scenario6)
	

write.csv(compare(env.base, env.scenario6), "compareBREASTA3_A14 halving non-breastfreeders.csv")	


env.scenario6 <- SimenvMELC$new()

env.scenario6$cat.adjustments$BREAST[1,]=c(0.01,rep(0.9/12, 12))


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

set.seed(220)
env.scenario6$simulateP(10)

sfStop()


compare(env.base, env.scenario6)
	

write.csv(compare(env.base, env.scenario6), "compareBREASTA3_A14 non-breastfeeder to 1 percent.csv")	


############################################################################
# Parental education – change from High 34% 58% 8%   to   high 50% 45% 5%


env.scenario6 <- SimenvMELC$new()

env.scenario6$cat.adjustments$r1ParentEduc[1,]=c(0.5, 0.45, 0.05)


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

set.seed(524)
env.scenario6$simulateP(10)

sfStop()


compare(env.base, env.scenario6)
	

write.csv(compare(env.base, env.scenario6), "comparer1ParentEduc2_A15.csv")	


############################################################################
# Sleep – add 30 minutes to everyone in the bottom 25% of distribution


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








