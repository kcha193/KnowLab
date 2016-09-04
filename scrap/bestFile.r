


setwd("C:/Users/kcha193/workspace/MELC/data")

#source("MELC.R")
source("MELCBase.R")

load("base/FullBaseRun.rData") # loads env.base object 


attach(env.base$simframe, name="simframe")

###################################################################
#Creating basefile for new set of variable


table(z1pregsmkLvl1 )/5000

hist(bwkg)

table(z1pregsmkLvl1 )/5000

table(SESBTHLvl1)
table(SESBTHLvl2)
table(SESBTHLvl3)

table(BREAST)
table(z1breastLvl1)

table(z1genderLvl1)



r1SESBTH <- numeric(5000)

r1SESBTH[which(as.logical(SESBTHLvl1))] <- "Professional"
r1SESBTH[which(as.logical(SESBTHLvl2))] <- "Clerical"
r1SESBTH[which(as.logical(SESBTHLvl3))] <- "Semi-skilled"
table(r1SESBTH)

r1stchildethn <-  numeric(5000)

r1stchildethn[which(as.logical(r1stchildethnLvl1))] <- "Euro"
r1stchildethn[which(as.logical(r1stchildethnLvl2))] <- "Pacific"
r1stchildethn[which(as.logical(r1stchildethnLvl3))] <- "Maori"
table(r1stchildethn)



#######################################################
#Create z1BreakfastLvl1
z1BreakfastLvl1 <- numeric(5000)


z1BreakfastLvl1[SESBTHLvl1 == 1 & z1genderLvl1 == 0 & r1stchildethn == 0] <- rbinom(sum(SESBTHLvl1 == 1 & z1genderLvl1 == 0 & r1stchildethn == 0), 1, (0.924 * 0.796 * 0.917)^(1/3))
z1BreakfastLvl1[SESBTHLvl2 == 1 & z1genderLvl1 == 0 & r1stchildethn == 0] <- rbinom(sum(SESBTHLvl2 == 1 & z1genderLvl1 == 0 & r1stchildethn == 0), 1, (0.867 * 0.796 * 0.917)^(1/3))
z1BreakfastLvl1[SESBTHLvl3 == 1 & z1genderLvl1 == 0 & r1stchildethn == 0] <- rbinom(sum(SESBTHLvl3 == 1 & z1genderLvl1 == 0 & r1stchildethn == 0), 1 , (0.664 * 0.796 * 0.917)^(1/3))


z1BreakfastLvl1[SESBTHLvl1 == 1 & z1genderLvl1 == 1 & r1stchildethn == 0] <- rbinom(sum(SESBTHLvl1 == 1 & z1genderLvl1 == 1 & r1stchildethn == 0), 1 , (0.924 * 0.872 * 0.917)^(1/3))
z1BreakfastLvl1[SESBTHLvl2 == 1 & z1genderLvl1 == 1 & r1stchildethn == 0] <- rbinom(sum(SESBTHLvl2 == 1 & z1genderLvl1 == 1 & r1stchildethn == 0), 1 , (0.867 * 0.872 * 0.917)^(1/3))
z1BreakfastLvl1[SESBTHLvl3 == 1 & z1genderLvl1 == 1 & r1stchildethn == 0] <- rbinom(sum(SESBTHLvl3 == 1 & z1genderLvl1 == 1 & r1stchildethn == 0), 1 , (0.664 * 0.872 * 0.917)^(1/3))



z1BreakfastLvl1[SESBTHLvl1 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Euro"] <- rbinom(sum(SESBTHLvl1 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Euro"), 1, (0.924 * 0.796* 0.917)^(1/3))
z1BreakfastLvl1[SESBTHLvl2 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Euro"] <- rbinom(sum(SESBTHLvl2 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Euro"), 1, (0.867 * 0.796* 0.917)^(1/3))
z1BreakfastLvl1[SESBTHLvl3 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Euro"] <- rbinom(sum(SESBTHLvl3 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Euro"), 1 , (0.664 * 0.796* 0.917)^(1/3))


z1BreakfastLvl1[SESBTHLvl1 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Euro"] <- rbinom(sum(SESBTHLvl1 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Euro"), 1 , (0.924 * 0.872* 0.917)^(1/3))
z1BreakfastLvl1[SESBTHLvl2 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Euro"] <- rbinom(sum(SESBTHLvl2 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Euro"), 1 , (0.867 * 0.872* 0.917)^(1/3))
z1BreakfastLvl1[SESBTHLvl3 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Euro"] <- rbinom(sum(SESBTHLvl3 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Euro"), 1 , (0.664 * 0.872* 0.917)^(1/3))




z1BreakfastLvl1[SESBTHLvl1 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Pacific"] <- rbinom(sum(SESBTHLvl1 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Pacific"), 1, (0.924 * 0.796* 0.524)^(1/3))
z1BreakfastLvl1[SESBTHLvl2 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Pacific"] <- rbinom(sum(SESBTHLvl2 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Pacific"), 1, (0.867 * 0.796* 0.524)^(1/3))
z1BreakfastLvl1[SESBTHLvl3 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Pacific"] <- rbinom(sum(SESBTHLvl3 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Pacific"), 1 , (0.664 * 0.796* 0.524)^(1/3))


z1BreakfastLvl1[SESBTHLvl1 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Pacific"] <- rbinom(sum(SESBTHLvl1 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Pacific"), 1 , (0.924 * 0.872* 0.524)^(1/3))
z1BreakfastLvl1[SESBTHLvl2 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Pacific"] <- rbinom(sum(SESBTHLvl2 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Pacific"), 1 , (0.867 * 0.872* 0.524)^(1/3))
z1BreakfastLvl1[SESBTHLvl3 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Pacific"] <- rbinom(sum(SESBTHLvl3 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Pacific"), 1 , (0.664 * 0.872* 0.524)^(1/3))



z1BreakfastLvl1[SESBTHLvl1 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Maori"] <- rbinom(sum(SESBTHLvl1 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Maori"), 1, (0.924 * 0.796* 0.705)^(1/3))
z1BreakfastLvl1[SESBTHLvl2 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Maori"] <- rbinom(sum(SESBTHLvl2 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Maori"), 1, (0.867 * 0.796* 0.705)^(1/3))
z1BreakfastLvl1[SESBTHLvl3 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Maori"] <- rbinom(sum(SESBTHLvl3 == 1 & z1genderLvl1 == 0 & r1stchildethn == "Maori"), 1 , (0.664 * 0.796* 0.705)^(1/3))


z1BreakfastLvl1[SESBTHLvl1 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Maori"] <- rbinom(sum(SESBTHLvl1 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Maori"), 1 , (0.924 * 0.872* 0.705)^(1/3))
z1BreakfastLvl1[SESBTHLvl2 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Maori"] <- rbinom(sum(SESBTHLvl2 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Maori"), 1 , (0.867 * 0.872* 0.705)^(1/3))
z1BreakfastLvl1[SESBTHLvl3 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Maori"] <- rbinom(sum(SESBTHLvl3 == 1 & z1genderLvl1 == 1 & r1stchildethn == "Maori"), 1 , (0.664 * 0.872* 0.705)^(1/3))



table(z1BreakfastLvl1)/5000


apply(table(z1genderLvl1, z1BreakfastLvl1), 1, function(x) x/sum(x))

apply(table(r1stchildethn, z1BreakfastLvl1), 1, function(x) x/sum(x))



###############################################################
#Create Maternal BMI
#First use Birthweight

hist(bwkg)
abline(v = 2.3, col = "red")
abline(v = 3.53, col = "red")
abline(v = 3.99, col = "red")

sum(bwkg <= 2.3)/5000
sum(bwkg <= 3.53 & bwkg > 2.3)/5000
sum(bwkg <= 3.99 & bwkg > 3.53)/5000
sum(bwkg > 3.99)/5000

r1mBMI <- numeric(5000)

r1mBMI[bwkg <= 2.3] <- 1
r1mBMI[bwkg <= 3.53 & bwkg > 2.3] <- 0
r1mBMI[bwkg <= 3.99 & bwkg > 3.53] <- 2
r1mBMI[bwkg > 3.99] <- 3

table(r1mBMI)/5000

mean(bwkg[r1mBMI == 1])
mean(bwkg[r1mBMI == 0])
mean(bwkg[r1mBMI == 2])
mean(bwkg[r1mBMI == 3])

(MBMI.baseRate <- table(r1mBMI)/5000)

MBMIpregsmk <- c(0.091, 0.001, 0.089, 0.161)

table(r1mBMI)/5000

table(r1mBMI, z1pregsmkLvl1)

apply(as.matrix(table(r1mBMI, z1pregsmkLvl1)), 1, function(x) x/sum(x))

r1mBMI[bwkg <= 4.1 & bwkg > 3.8 & z1pregsmkLvl1==0] <- 2
r1mBMI[bwkg <= 4.1 & bwkg > 3.8 & z1pregsmkLvl1==1] <- 3

apply(as.matrix(table(r1mBMI, z1pregsmkLvl1)), 1, function(x) x/sum(x))

table(r1mBMI)/5000

r1mBMI[bwkg <= 3.57 & bwkg > 3.5 & z1pregsmkLvl1==0] <- 0
r1mBMI[bwkg <= 3.57 & bwkg > 3.5 & z1pregsmkLvl1==1] <- 2

apply(as.matrix(table(r1mBMI, z1pregsmkLvl1)), 1, function(x) x/sum(x))



apply(as.matrix(table(r1mBMI, r1SESBTH)), 1, function(x) x/sum(x))

apply(as.matrix(table(r1stchildethn, r1mBMI)), 1, function(x) x/sum(x))

table(r1mBMI[r1stchildethn==0])

r1mBMI[r1stchildethn==0][r1mBMI[r1stchildethn==0] ==3][1:50] = 0

r1mBMI[r1stchildethn=="Euro"][r1mBMI[r1stchildethn=="Euro"] ==0][1:50] = 3

r1mBMI[r1stchildethn=="Pacific"][r1mBMI[r1stchildethn=="Pacific"] ==0][1:170] = 2
r1mBMI[r1stchildethn=="Maori"][r1mBMI[r1stchildethn=="Maori"] ==0][1:45] = 2
r1mBMI[r1stchildethn=="Maori"][r1mBMI[r1stchildethn=="Maori"] ==3][1:20] = 0
      

apply(as.matrix(table(r1mBMI, r1SESBTH)), 1, function(x) x/sum(x))
    
  
table(r1mBMI)/5000

r1mBMILvl1 <- numeric(5000)
r1mBMILvl2 <- numeric(5000)
r1mBMILvl3 <- numeric(5000)

r1mBMILvl1[r1mBMI == 1] <- 1
r1mBMILvl2[r1mBMI == 2] <- 1
r1mBMILvl3[r1mBMI == 3] <- 1

###############################################################
#Create Cesarean

euroP <- c(5471, 676)
euroP <- euroP/sum(euroP)

maoriP <- c(690, 20)
maoriP <- maoriP/sum(maoriP)

pacificP <- c(1146, 25)
pacificP <- pacificP/sum(pacificP)

asianP <- c(1676 + 833 + 785, 77 + 47 + 56)
asianP <- asianP/sum(asianP)


z1CaesareanLvl1 <- numeric(5000)

z1CaesareanLvl1[r1mBMI == 0 & r1stchildethn == 0] <- rbinom(sum(r1mBMI == 0 & r1stchildethn == 0), 1, (0.18 *asianP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 1 & r1stchildethn == 0] <- rbinom(sum(r1mBMI == 1 & r1stchildethn == 0), 1, (0.18 *asianP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 2 & r1stchildethn == 0] <- rbinom(sum(r1mBMI == 2 & r1stchildethn == 0), 1, (0.23 *asianP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 3 & r1stchildethn == 0] <- rbinom(sum(r1mBMI == 3 & r1stchildethn == 0), 1, (0.36 *asianP[2])^(1/2))

z1CaesareanLvl1[r1mBMI == 0 & r1stchildethn == "Euro"] <- rbinom(sum(r1mBMI == 0 & r1stchildethn == "Euro"), 1, (0.18 *euroP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 1 & r1stchildethn == "Euro"] <- rbinom(sum(r1mBMI == 1 & r1stchildethn == "Euro"), 1, (0.18 *euroP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 2 & r1stchildethn == "Euro"] <- rbinom(sum(r1mBMI == 2 & r1stchildethn == "Euro"), 1, (0.23 *euroP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 3 & r1stchildethn == "Euro"] <- rbinom(sum(r1mBMI == 3 & r1stchildethn == "Euro"), 1, (0.36 *euroP[2])^(1/2))

z1CaesareanLvl1[r1mBMI == 0 & r1stchildethn == "Pacific"] <- rbinom(sum(r1mBMI == 0 & r1stchildethn == "Pacific"), 1, (0.18 *pacificP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 1 & r1stchildethn == "Pacific"] <- rbinom(sum(r1mBMI == 1 & r1stchildethn == "Pacific"), 1, (0.18 *pacificP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 2 & r1stchildethn == "Pacific"] <- rbinom(sum(r1mBMI == 2 & r1stchildethn == "Pacific"), 1, (0.23 *pacificP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 3 & r1stchildethn == "Pacific"] <- rbinom(sum(r1mBMI == 3 & r1stchildethn == "Pacific"), 1, (0.36 *pacificP[2])^(1/2))

z1CaesareanLvl1[r1mBMI == 0 & r1stchildethn == "Maori"] <- rbinom(sum(r1mBMI == 0 & r1stchildethn == "Maori"), 1, (0.18 *maoriP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 1 & r1stchildethn == "Maori"] <- rbinom(sum(r1mBMI == 1 & r1stchildethn == "Maori"), 1, (0.18 *maoriP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 2 & r1stchildethn == "Maori"] <- rbinom(sum(r1mBMI == 2 & r1stchildethn == "Maori"), 1, (0.23 *maoriP[2])^(1/2))
z1CaesareanLvl1[r1mBMI == 3 & r1stchildethn == "Maori"] <- rbinom(sum(r1mBMI == 3 & r1stchildethn == "Maori"), 1, (0.36 *maoriP[2])^(1/2))


table(z1CaesareanLvl1)/5000

apply(as.matrix(table(r1mBMI, z1CaesareanLvl1)), 1, function(x) x/sum(x))



#########################################################################
#Create Parental education level <- the highest amoung each parent

r1stfeduc <- numeric(5000)
r1stmeduc <- numeric(5000)

r1stfeduc[r1stfeducLvl1 == 1] <- 1
r1stfeduc[r1stfeducLvl2 == 1] <- 2
r1stfeduc[r1stfeducLvl3 == 1] <- 3

r1stmeduc[r1stmeducLvl1 == 1] <- 1
r1stmeduc[r1stmeducLvl2 == 1] <- 2
r1stmeduc[r1stmeducLvl3 == 1] <- 3


table(r1stfeduc)
table(r1stmeduc)

r1ParentEduc <- apply(cbind(r1stfeduc, r1stmeduc),1, min)

r1ParentEducLvl1 <- numeric(5000)
r1ParentEducLvl2 <- numeric(5000)
r1ParentEducLvl3 <- numeric(5000)

r1ParentEducLvl1[r1ParentEduc == 1] <- 1
r1ParentEducLvl2[r1ParentEduc == 2] <- 1
r1ParentEducLvl3[r1ParentEduc == 3] <- 1


table(r1ParentEducLvl1)
table(r1ParentEducLvl2)
table(r1ParentEducLvl3)



getwd()

write.csv(cbind(r1ParentEduc, r1mBMI, z1BreakfastLvl1, z1CaesareanLvl1), "BaseFile.csv")



####################################################################

z1genderLvl1 <- simframe.master$z1genderLvl1
sleepBase <- matrix(0, nrow = 5000, ncol = 18) 
r1SleepBase <- matrix(0, nrow = 5000, ncol = 18)

for( iteration in 2:18){ 

	sleepTime <- numeric(5000)

	r1Sleep <- numeric(5000)

  if(iteration>=2 & iteration<=3) {			
		#browser()
			
		sleepTime[z1genderLvl1 == 0] <- rnorm(sum(z1genderLvl1 == 0), 10.5, 0.6)
		sleepTime[z1genderLvl1 == 1] <- rnorm(sum(z1genderLvl1 == 1), 10.5 - 9/60, 0.6)
		
	
		r1Sleep[sleepTime>11.5] <- 3
		r1Sleep[sleepTime<=11.5 & sleepTime>=9.5] <- 2
		r1Sleep[sleepTime<9.5] <- 1
			
			
		} else if(iteration>=4 & iteration<=5) {			
			
		
		sleepTime[z1genderLvl1 == 0] <- rnorm(sum(z1genderLvl1 == 0), 12, 0.6)
		sleepTime[z1genderLvl1 == 1] <- rnorm(sum(z1genderLvl1 == 1), 12 - 11/60, 0.6)
		

		r1Sleep[sleepTime>13] <- 3
		r1Sleep[sleepTime<=13 & sleepTime>=11] <- 2
		r1Sleep[sleepTime<11] <- 1	
			
			
		} else if(iteration>=6 & iteration<=7) {			
	
		sleepTime[z1genderLvl1 == 0] <- rnorm(sum(z1genderLvl1 == 0), 10, 0.6)
		sleepTime[z1genderLvl1 == 1] <- rnorm(sum(z1genderLvl1 == 1), 10 - 16 /60, 0.6)
	

	
		r1Sleep[sleepTime>11] <- 3
		r1Sleep[sleepTime<=11& sleepTime>=9] <- 2
		r1Sleep[sleepTime<9] <- 1
		
	
			
	  } else if(iteration>=8 & iteration<=9) {			
		#browser()
		
		sleepTime<- rnorm(5000, 10, 0.6)	

		r1Sleep[sleepTime>11] <- 3
		r1Sleep[sleepTime<=11 & sleepTime>=9] <- 2
		r1Sleep[sleepTime<9] <- 1

			
	  } else if(iteration>=10 & iteration<=19) {			
		#browser()
		
		sleepTime <- rnorm(5000, 8.875, 0.6)
		
	
		r1Sleep[sleepTime>10] <- 3
		r1Sleep[sleepTime<=10 & sleepTime>=8] <- 2
		r1Sleep[sleepTime<8] <- 1	
			
	  } 	  
	  
	  r1SleepBase[,iteration] <- r1Sleep
	  sleepBase[,iteration] <- sleepTime
	  

}

apply(r1SleepBase, 2, function(x) table(x)/5000)

write.csv(r1SleepBase, "sleepBase.csv")




#####################################################################

setwd("C:/Users/kcha193/workspace/MELC/data")


base <- read.csv("base/synthBasefile_MhrswrkFixed_5000_New.csv")



base$pregsmk[base$pregsmk>0] <- 1
base$BREAST[base$BREAST>0] <- 1


base <- base[,c("r1mBMI", "r1ParentEduc", "SESBTH", "z1CaesareanLvl1", "z1BreakfastLvl1", "r1stchildethn", "z1gender")]



apply(base, 2, function(x) table(x)/5000)

#r1mBMI
apply(base[,-1], 2, function(x) apply(as.matrix(table(base[,1], x)), 1, 
			function(y) round(y/sum(y), 2)))

apply(table(base$r1mBMI, base$r1stchildethn),2 , function(y) round(y/sum(y), 2))
		
			
#pregsmk			
apply(base[,-c(1:2)], 2, function(x) apply(as.matrix(table(base[,2], x)), 1, 
			function(y) round(y/sum(y), 2)))

#BREAST			
apply(base[,-c(1:3)], 2, function(x) apply(as.matrix(table(base[,3], x)), 1, 
			function(y) round(y/sum(y), 2)))

#r1ParentEduc			
apply(base[,-c(1:4)], 2, function(x) apply(as.matrix(table(base[,3], x)), 1, 
			function(y) round(y/sum(y), 2)))
			
			
#SESBTH			
 apply(as.matrix(table(base[,5], base[,6])), 1, 
			function(y) round(y/sum(y), 2))

 apply(as.matrix(table(base[,5], base[,7])), 1, 
			function(y) round(y/sum(y), 2))

#z1CaesareanLvl1			
 apply(as.matrix(table(base[,6], base[,7])), 1, 
			function(y) round(y/sum(y), 2))

 ##############################################################################

 
 attach(env.base$simframe, name="simframe")
 
 library(bindata)
 set.seed(2092016)
 
 
 alcoholModel <- 
   data.frame( Age = c(29.5, 39.5, 49.5, 59.5, 69.5, 79.5), 
               male = c(28.6, 27.1, 25.4, 20.8, 14.8, 5.4),
               female = c(15.5, 12.5, 11.7, 6.1, 3.1, 0.8))
 
 parentAlcohol <- data.frame(Age = 25:90 )
 
 parentAlcohol$father <- predict(lm(male ~ I(Age) + I(Age^2), alcoholModel), parentAlcohol)/100
 parentAlcohol$mother <-predict(lm(female ~ I(Age) + I(Age^2), alcoholModel),parentAlcohol)/100
 parentAlcohol$mother[parentAlcohol$mother < 0.002] <- 0.002
 parentAlcohol$father[parentAlcohol$father < 0.034] <- 0.034
 
 
 x <- matrix(NA, ncol = 2, nrow = 5000)
 
for( i in 15:21){
  print(i)
  ParentAlc <- 
   cbind(parentAlcohol$mother[match(MAGE+i, parentAlcohol$Age)], 
         parentAlcohol$father[match(fage_imputed+i, parentAlcohol$Age)])
  
  ParentAlc[ParentAlc == 0] <- 0.004
  
  indexFULL <- !apply(ParentAlc, 1, function(y) any(is.na(y)))
  
  temp <- apply(ParentAlc[indexFULL,], 1,  function(y) 
   rmvbin(1, margprob =y, bincorr = matrix(c(1,0.12,0.12,1), ncol = 2, nrow = 2)))
  x[indexFULL,] <- t(temp)
  
  indexAnyNA <- apply(ParentAlc, 1, function(y) any(is.na(y)))
  
  x[indexAnyNA,] <- suppressWarnings(apply(ParentAlc[indexAnyNA,], 2, 
                                           function(y) sapply(y, function(z) rbinom(1,1,z))))
  
  print(cor(x, use = "pairwise.complete.obs", method="spearman"))
  
  assign(paste0("z1MotherAlc", i, "Lvl1"), x[,1])
  assign(paste0("z1FatherAlc", i, "Lvl1"), x[,2])
}
 

 
 
 total = c(8.1, 14.1, 17.3, 17.6, 18.0, 15.0, 10.9)
 age = c(19.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5)
 
 pDepress <- predict(lm(total ~ I(age) + I(age^2)),  data.frame(age = 15:21 ))/100
 
 
 depressModel <- 
   data.frame( Age = c(29.5, 39.5, 49.5, 59.5, 69.5, 79.5), 
               male = c(9.2, 11.8, 13.3, 13.3, 10.4, 7.6),
               female = c(18.8, 22.3, 21.6, 22.4, 19.3, 13.4)              )
 
 parentDepress <- data.frame(Age = 25:90 )
 
 parentDepress$father <- predict(lm(male ~ I(Age) + I(Age^2) , depressModel), parentDepress)/100
 parentDepress$mother <-predict(lm(female ~ I(Age) + I(Age^2), depressModel),parentDepress)/100
 
 x <- matrix(NA, ncol = 2, nrow = 5000)
 
 for( i in 15:21){
   print(i)
   ParentDep <- 
     cbind(parentDepress$mother[match(MAGE+i, parentDepress$Age)], 
           parentDepress$father[match(fage_imputed+i, parentDepress$Age)])
   
   ParentDep[ParentDep == 0] <- 0.004
   
   indexFULL <- !apply(ParentDep, 1, function(y) any(is.na(y)))
   
   temp <- apply(ParentDep[indexFULL,], 1,  function(y) 
     rmvbin(1, margprob =y, bincorr = matrix(c(1,0.12,0.12,1), ncol = 2, nrow = 2)))
   x[indexFULL,] <- t(temp)
   
   indexAnyNA <- apply(ParentDep, 1, function(y) any(is.na(y)))
   
   x[indexAnyNA,] <- suppressWarnings(apply(ParentDep[indexAnyNA,], 2, 
                                            function(y) sapply(y, function(z) rbinom(1,1,z))))
   
   print(cor(x, use = "pairwise.complete.obs", method="spearman"))
  
   assign(paste0("z1MotherDepress", i, "Lvl1"), x[,1])
   assign(paste0("z1FatherDepress", i, "Lvl1"), x[,2])
 }
 
 pAD <- data.frame(X = 1:5000)
 
 colNames <- c()
 
 for(i in 15:21){
   pAD <- cbind(pAD, 
         get(paste0("z1MotherAlc", i, "Lvl1")),
         get(paste0("z1FatherAlc", i, "Lvl1")),
         get(paste0("z1MotherDepress", i, "Lvl1")),
         get(paste0("z1FatherDepress", i, "Lvl1")))
   
   colNames <- c(colNames, paste0("z1MotherAlc", i, "Lvl1"), paste0("z1FatherAlc", i, "Lvl1"),
     paste0("z1MotherDepress", i, "Lvl1"), paste0("z1FatherDepress", i, "Lvl1"))
   
 }
 
pAD <- pAD[,-1]

names(pAD) <- colNames

apply(pAD, 2,  function(x) prop.table(table(x)))

cor(pAD, use = "pairwise.complete.obs", method="spearman")

write.csv(pAD, "parentalAlcDepress.csv")
 

 
##########################################################################################################


detach("simframe")


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 