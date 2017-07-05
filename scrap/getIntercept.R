
modelCoef <-function(px, py, OR, RR = FALSE, n = 5000){
  
  if(RR)
    OR <- OR *(1-px + OR * px - py)/( 1 - px + OR*px -OR*py)
  #ZOCCHETTI et al (1997),  International Journal of Epidemiology; 26(1):220-3
  
  f<-function(x)
    (px - (1-py))*n*x + x^2 - OR*(x^2 +(1-px)*(1-py)*n^2 - (2-px-py)*n*x)
  
  a <- try(uniroot(f, c(0,n)), silent = TRUE)
  
  if(!is.null( attr(a, "class")))
    return(c(intercept =  0, OR=log(OR)))	
  
  a <- a$root		
  c <-(1-py)*n-a
  b <-(py-px)*n+c
  d <-px*n-c
  
  return(c(intercept =  b/a, OR=log(OR)))		
}


sumInt <- function(intercept, baseRate)		
  sum(intercept/(1+intercept)*baseRate/sum(baseRate))


ORtoRR <-function(px, py, OR, n = 5000){
  
  #ZOCCHETTI et al (1997),  International Journal of Epidemiology; 26(1):220-3
  
  return(OR *(1-px + OR * px - py)/( 1 - px + OR*px -OR*py))		
  
}


ORtoRR(0.08032883, 0.1558, 3.06, 1369.582)
ORtoRR(0.1558,0.08032883,  3.06)



ORtoRR(prop.table(table(env.base$simframe$r1stchildethnLvl2))[2], 0.33,  1.41)




setwd("C:\\Users\\kcha193\\workspace\\KnowLab")

#source("MELC.R")
env.base  <- readRDS("base/FullBaseRun.rds")

################################################################################

ORtoRR <-function(px, py, OR, n = 5000){
  
  #ZOCCHETTI et al (1997),  International Journal of Epidemiology; 26(1):220-3
  
  return(OR *(1-px + OR * px - py)/( 1 - px + OR*px -OR*py))		
  
}

getIntercept <- function(base.rate, model.glm, envir = parent.frame(), set = NULL){
  
  vars <- attr(delete.response(terms(model.glm)), "variables")
  
  vars.evaluated <- eval(vars, envir)
  names(vars.evaluated) <- as.character(vars)[-1]
  
  if (!is.null(set)) 
    vars.evaluated <- lapply(vars.evaluated, 
                             function(x) x[set, drop = FALSE])		 
  
  joinP.df <- as.data.frame(table( vars.evaluated ) )		 
  joinP <- joinP.df[,ncol(joinP.df)]/sum( joinP.df[,ncol(joinP.df)]	)		 
  
  names(joinP) <- apply(joinP.df[,1:ncol(joinP.df)-1], 1, 
                        function(x) paste(x, sep = "", collapse = "."))
  
  joinP <- joinP[joinP>1e-7]
  
  conP <- as.numeric(t(sapply(strsplit(names(joinP), "\\." ), 
                              function(x) as.numeric(x))) %*% 
                       model.glm$coefficients[-1])
  
  f <-
    function(alpha)
      base.rate - sum(exp(alpha + conP)/(1+exp(alpha + conP)) *joinP) 
  
  return(uniroot(f, c(-100,100))$root)
}

scaleOR <- 
  function(P_OB, P_OW, P_yes, OR_OB, OR_OW, n){
    
    targetFun <- function(b)
      (P_yes * n -b)*((1-P_OW)*n-b) - OR_OW*((P_OW-P_yes)*n+b)*b
    
    b = uniroot(targetFun, lower = 0, upper = n - n*P_OW, tol = 0.000001)$root
    b
    
    c = (1-P_OW)*n - b
    c
    
    targetFun <- function(x)
      (1-P_OB-P_yes)*n*x  + x^2 - OR_OB * (P_yes * n -x)*(P_OB * n -x)
    
    x = uniroot(targetFun, lower = 0, upper = n*P_OB, tol = 0.000001)$root
    x
    y = P_OB*n - x
    y 
    
    z = P_yes * n - x -b
    z
    a = (P_OW-P_OB)*n-z
    a
    
    (x*a)/(z*y)
  }

#############################################################################

modelfiledir <- paste(getwd(),"/models/",sep="")

#models <- loadMELCModels(modelfiledir)
#checkModelVars(models, simframe.master)



fileName <- list.files(modelfiledir)
modelName <- sapply(strsplit(list.files(modelfiledir),"\\."), function(x) x[1])
models <- modeldf <- vector(length(modelName), mode = "list")
names(models) <- names(modeldf) <- modelName

for(i in 1:length(modelName)) {
  
  models[[i]] <-  loadGLMCSV(modelfiledir, fileName[i])
  
  modeldf[[i]] <-  read.csv(paste(modelfiledir, fileName[i], sep =""))
  
}	


attach(env.base$simframe, name="simframe")

#detach(simframe)
########################################################################
#Overweight and obese


pOverweight <- c(NA, 29.02058, 29.27779, 29.60634, 30.00623, 30.47746, 31.02002,
                 31.63393, 32.31917, 33.07576, 33.90369, 34.80295, 35.77356, 36.81550,
                 37.92878, 39.11341, 40.36937, 41.69667, 43.09531, 44.56530, 46.10662)/100


pObese <-
  c(NA, 8.340484,  8.764238,  9.208993,  9.674749, 10.161506, 10.669264, 11.198024, 11.747785, 
    12.318547, 12.910310, 13.523074, 14.156840, 14.811607, 15.487375, 16.184144, 16.901914, 
    17.640686, 18.400458, 19.181232, 19.983007)/100 - 0.01

pObeseOW <- pObese/pOverweight

z1WatchTVLvl1 <- env.base$simframe$z1WatchTVLvl1

temp <- models$zz1OverweightA2


z1RuralLvl1.list <- sapply(env.base$modules$run_results, function(x) x$z1RuralLvl1[,1])


sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,"2"])

temp.inter <- numeric(10)

for(i in 1:10) {
  r1SleepLvl1 <- numeric(5000)
  r1SleepLvl2 <- numeric(5000)
  r1SleepLvl3 <- numeric(5000)
  
  r1SleepLvl3[sleepTime[,i] == 3] <- 1
  r1SleepLvl2[sleepTime[,i] == 2] <- 1
  r1SleepLvl1[sleepTime[,i] == 1] <- 1
  
  
  z1RuralLvl1 <- z1RuralLvl1.list[,i]
  
  r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                           z1RuralLvl1 == 1, 1, 0)
  
  temp.inter[i] <- getIntercept(pOverweight[2], temp)
}

modeldf$zz1OverweightA2[1, 3] <- mean(temp.inter)
  

temp$coefficients[1] <- modeldf$zz1OverweightA2[1, 3]

z1OverweightLvl1 <- predSimBinom(temp)

print(table(z1OverweightLvl1)[2]/5000)

write.csv(modeldf$zz1OverweightA2, paste(modelfiledir, "z1OverweightA", 2, ".csv", sep = ""), row.names = FALSE) 

#########################################################################################################

temp <- models$zz1ObeseA2


tempOW <- models$zz1OverweightA2


OR <- sapply(names(temp$coefficients)[-1], function(x) 
scaleOR(P_OB = pObese[2], 
        P_OW = pOverweight[2], 
        P_yes = prop.table(table(get(x)))[2], 
        OR_OB = exp(temp$coefficients[x]),
        OR_OW = exp(tempOW$coefficients[x]),
        n = 5000))

temp$coefficients[-1] <- log(OR)



modeldf$zz1ObeseA2[1, 3] <- getIntercept( pObeseOW[2], temp)


temp$coefficients[1] <- modeldf$zz1ObeseA2[1, 3]

z1ObeseLvl1 <- rep(0, 5000)

z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)

print(table(z1ObeseLvl1)[2]/5000)


modeldf$zz1ObeseA2$Estimate <- temp$coefficients

write.csv(modeldf$zz1ObeseA2, paste(modelfiledir, "z1obeseA", 2, ".csv", sep = ""), row.names = FALSE) 



#########################################################################################################


temp <- models$zz1OverweightA3

sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,"3"])

temp.inter <- numeric(10)

for(i in 1:10) {
  r1SleepLvl1 <- numeric(5000)
  r1SleepLvl2 <- numeric(5000)
  r1SleepLvl3 <- numeric(5000)
  
  r1SleepLvl3[sleepTime[,i] == 3] <- 1
  r1SleepLvl2[sleepTime[,i] == 2] <- 1
  r1SleepLvl1[sleepTime[,i] == 1] <- 1
  
  
  z1RuralLvl1 <- z1RuralLvl1.list[,i]
  
  r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                           z1RuralLvl1 == 1, 1, 0)
  
  temp.inter[i] <- getIntercept(pOverweight[3], temp)
}

modeldf$zz1OverweightA3[1, 3] <- mean(temp.inter)



temp$coefficients[1] <- modeldf$zz1OverweightA3[1, 3]

z1OverweightLvl1 <- predSimBinom(temp)

print(table(z1OverweightLvl1)[2]/5000)


write.csv(modeldf$zz1OverweightA3, paste(modelfiledir, "z1OverweightA", 3, ".csv", sep = ""), row.names = FALSE) 

#########################################################################################################

temp <- models$zz1ObeseA3


tempOW <- models$zz1OverweightA3


OR <- sapply(names(temp$coefficients)[-1], function(x) 
  scaleOR(P_OB = pObese[3], 
          P_OW = pOverweight[3], 
          P_yes = prop.table(table(get(x)))[2], 
          OR_OB = exp(temp$coefficients[x]),
          OR_OW = exp(tempOW$coefficients[x]),
          n = 5000))

temp$coefficients[-1] <- log(OR)



modeldf$zz1ObeseA3[1, 3] <- getIntercept( pObeseOW[3], temp)


temp$coefficients[1] <- modeldf$zz1ObeseA3[1, 3]

z1ObeseLvl1 <- rep(0, 5000)

z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)

print(table(z1ObeseLvl1)[2]/5000)


modeldf$zz1ObeseA3$Estimate <- temp$coefficients

write.csv(modeldf$zz1ObeseA3, paste(modelfiledir, "z1obeseA", 3, ".csv", sep = ""), row.names = FALSE) 



#########################################################################################################


temp <- models$zz1OverweightA4

sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,"4"])

temp.inter <- numeric(10)

for(i in 1:10) {
  r1SleepLvl1 <- numeric(5000)
  r1SleepLvl2 <- numeric(5000)
  r1SleepLvl3 <- numeric(5000)
  
  r1SleepLvl3[sleepTime[,i] == 3] <- 1
  r1SleepLvl2[sleepTime[,i] == 2] <- 1
  r1SleepLvl1[sleepTime[,i] == 1] <- 1
  
  
  z1RuralLvl1 <- z1RuralLvl1.list[,i]
  
  r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                           z1RuralLvl1 == 1, 1, 0)
  
  temp.inter[i] <- getIntercept(pOverweight[4], temp)
}

modeldf$zz1OverweightA4[1, 3] <- mean(temp.inter)

temp$coefficients[1] <- modeldf$zz1OverweightA4[1, 3]

z1OverweightLvl1 <- predSimBinom(temp)

print(table(z1OverweightLvl1)[2]/5000)


write.csv(modeldf$zz1OverweightA4, paste(modelfiledir, "z1OverweightA", 4, ".csv", sep = ""), row.names = FALSE) 

#########################################################################################################

temp <- models$zz1ObeseA4


tempOW <- models$zz1OverweightA4


OR <- sapply(names(temp$coefficients)[-1], function(x) 
  scaleOR(P_OB = pObese[4], 
          P_OW = pOverweight[4], 
          P_yes = prop.table(table(get(x)))[2], 
          OR_OB = exp(temp$coefficients[x]),
          OR_OW = exp(tempOW$coefficients[x]),
          n = 5000))

temp$coefficients[-1] <- log(OR)



modeldf$zz1ObeseA4[1, 3] <- getIntercept( pObeseOW[4], temp)


temp$coefficients[1] <- modeldf$zz1ObeseA4[1, 3]

z1ObeseLvl1 <- rep(0, 5000)

z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)

print(table(z1ObeseLvl1)[2]/5000)


modeldf$zz1ObeseA4$Estimate <- temp$coefficients

write.csv(modeldf$zz1ObeseA4, paste(modelfiledir, "z1obeseA", 4, ".csv", sep = ""), row.names = FALSE) 



#########################################################################################################

temp <- models$zz1OverweightA5

sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,"5"])

temp.inter <- numeric(10)

for(i in 1:10) {
  r1SleepLvl1 <- numeric(5000)
  r1SleepLvl2 <- numeric(5000)
  r1SleepLvl3 <- numeric(5000)
  
  r1SleepLvl3[sleepTime[,i] == 3] <- 1
  r1SleepLvl2[sleepTime[,i] == 2] <- 1
  r1SleepLvl1[sleepTime[,i] == 1] <- 1
  
  
  z1RuralLvl1 <- z1RuralLvl1.list[,i]
  
  r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                           z1RuralLvl1 == 1, 1, 0)
  
  temp.inter[i] <- getIntercept(pOverweight[5], temp)
}

modeldf$zz1OverweightA5[1, 3] <- mean(temp.inter)



temp$coefficients[1] <- modeldf$zz1OverweightA5[1, 3]

z1OverweightLvl1 <- predSimBinom(temp)

print(table(z1OverweightLvl1)[2]/5000)


write.csv(modeldf$zz1OverweightA5, paste(modelfiledir, "z1OverweightA", 5, ".csv", sep = ""), row.names = FALSE) 

#########################################################################################################

temp <- models$zz1ObeseA5


tempOW <- models$zz1OverweightA5


OR <- sapply(names(temp$coefficients)[-1], function(x) 
  scaleOR(P_OB = pObese[5], 
          P_OW = pOverweight[5], 
          P_yes = prop.table(table(get(x)))[2], 
          OR_OB = exp(temp$coefficients[x]),
          OR_OW = exp(tempOW$coefficients[x]),
          n = 5000))

temp$coefficients[-1] <- log(OR)



modeldf$zz1ObeseA5[1, 3] <- getIntercept( pObeseOW[5], temp)


temp$coefficients[1] <- modeldf$zz1ObeseA5[1, 3]

z1ObeseLvl1 <- rep(0, 5000)

z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)

print(table(z1ObeseLvl1)[2]/5000)


modeldf$zz1ObeseA5$Estimate <- temp$coefficients

write.csv(modeldf$zz1ObeseA5, paste(modelfiledir, "z1obeseA", 5, ".csv", sep = ""), row.names = FALSE) 



#########################################################################################################


temp <- models$zz1OverweightA6_7


for( i in 6:7){
  
  temp <- models$zz1OverweightA6_7	
  
  sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,as.character(i)])
  
  temp.inter <- numeric(10)
  
  for(j in 1:10) {
    r1SleepLvl1 <- numeric(5000)
    r1SleepLvl2 <- numeric(5000)
    r1SleepLvl3 <- numeric(5000)
    
    r1SleepLvl3[sleepTime[,j] == 3] <- 1
    r1SleepLvl2[sleepTime[,j] == 2] <- 1
    r1SleepLvl1[sleepTime[,j] == 1] <- 1
    
    
    z1RuralLvl1 <- z1RuralLvl1.list[,j]
    
    r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                             z1RuralLvl1 == 1, 1, 0)
    
    temp.inter[j] <- getIntercept(pOverweight[i], temp)
  }
  
  modeldf$zz1OverweightA6_7[1, 3] <- mean(temp.inter)
  
  
  temp$coefficients[1] <- modeldf$zz1OverweightA6_7[1, 3]
  
  z1OverweightLvl1 <- predSimBinom(temp)
  
  print(table(z1OverweightLvl1)[2]/5000)
  
  write.csv(modeldf$zz1OverweightA6_7, paste(modelfiledir, "z1OverweightA", i, ".csv", sep = ""), row.names = FALSE) 
  

  
  temp <- models$zz1ObeseA6_7
  
  
  tempOW <- models$zz1OverweightA6_7
  
  
  OR <- sapply(names(temp$coefficients)[-1], function(x) 
    scaleOR(P_OB = pObese[i], 
            P_OW = pOverweight[i], 
            P_yes = prop.table(table(get(x)))[2], 
            OR_OB = exp(temp$coefficients[x]),
            OR_OW = exp(tempOW$coefficients[x]),
            n = 5000))
  
  temp$coefficients[-1] <- log(OR)
  
  
  
  modeldf$zz1ObeseA6_7[1, 3] <- getIntercept( pObeseOW[i], temp)
  
  
  temp$coefficients[1] <- modeldf$zz1ObeseA6_7[1, 3]
  
  z1ObeseLvl1 <- rep(0, 5000)
  
  z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)
  
  print(table(z1ObeseLvl1)[2]/5000)
  
  
  modeldf$zz1ObeseA6_7$Estimate <- temp$coefficients
  
  write.csv(modeldf$zz1ObeseA6_7, paste(modelfiledir, "z1obeseA", i, ".csv", sep = ""), row.names = FALSE) 
  
  
}

		


temp <- models$zz1OverweightA8

sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,"8"])

temp.inter <- numeric(10)

for(j in 1:10) {
  r1SleepLvl1 <- numeric(5000)
  r1SleepLvl2 <- numeric(5000)
  r1SleepLvl3 <- numeric(5000)
  
  r1SleepLvl3[sleepTime[,j] == 3] <- 1
  r1SleepLvl2[sleepTime[,j] == 2] <- 1
  r1SleepLvl1[sleepTime[,j] == 1] <- 1
  
  
  z1RuralLvl1 <- z1RuralLvl1.list[,j]
  
  r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                           z1RuralLvl1 == 1, 1, 0)
  
  temp.inter[j] <- getIntercept(pOverweight[8], temp)
}

modeldf$zz1OverweightA8[1, 3] <- mean(temp.inter)

temp$coefficients[1] <- modeldf$zz1OverweightA8[1, 3]

z1OverweightLvl1 <- predSimBinom(temp)

print(table(z1OverweightLvl1)[2]/5000)


write.csv(modeldf$zz1OverweightA8, paste(modelfiledir, "z1OverweightA", 8, ".csv", sep = ""), row.names = FALSE) 
#########################################################################################################

temp <- models$zz1ObeseA8


tempOW <- models$zz1OverweightA8


OR <- sapply(names(temp$coefficients)[-1], function(x) 
  scaleOR(P_OB = pObese[8], 
          P_OW = pOverweight[8], 
          P_yes = prop.table(table(get(x)))[2], 
          OR_OB = exp(temp$coefficients[x]),
          OR_OW = exp(tempOW$coefficients[x]),
          n = 5000))

temp$coefficients[-1] <- log(OR)



modeldf$zz1ObeseA8[1, 3] <- getIntercept( pObeseOW[8], temp)


temp$coefficients[1] <- modeldf$zz1ObeseA8[1, 3]

z1ObeseLvl1 <- rep(0, 5000)

z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)

print(table(z1ObeseLvl1)[2]/5000)


modeldf$zz1ObeseA8$Estimate <- temp$coefficients

write.csv(modeldf$zz1ObeseA8, paste(modelfiledir, "z1obeseA", 8, ".csv", sep = ""), row.names = FALSE) 

#########################################################################################################

temp <- models$zz1OverweightA9

sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,"9"])

temp.inter <- numeric(10)

for(j in 1:10) {
  r1SleepLvl1 <- numeric(5000)
  r1SleepLvl2 <- numeric(5000)
  r1SleepLvl3 <- numeric(5000)
  
  r1SleepLvl3[sleepTime[,j] == 3] <- 1
  r1SleepLvl2[sleepTime[,j] == 2] <- 1
  r1SleepLvl1[sleepTime[,j] == 1] <- 1
  
  
  z1RuralLvl1 <- z1RuralLvl1.list[,j]
  
  r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                           z1RuralLvl1 == 1, 1, 0)
  
  temp.inter[j] <- getIntercept(pOverweight[9], temp)
}

modeldf$zz1OverweightA9[1, 3] <- mean(temp.inter)


temp$coefficients[1] <- modeldf$zz1OverweightA9[1, 3]

z1OverweightLvl1 <- predSimBinom(temp)

print(table(z1OverweightLvl1)[2]/5000)


write.csv(modeldf$zz1OverweightA9, paste(modelfiledir, "z1OverweightA", 9, ".csv", sep = ""), row.names = FALSE) 

#########################################################################################################

temp <- models$zz1ObeseA9


tempOW <- models$zz1OverweightA9


OR <- sapply(names(temp$coefficients)[-1], function(x) 
  scaleOR(P_OB = pObese[9], 
          P_OW = pOverweight[9], 
          P_yes = prop.table(table(get(x)))[2], 
          OR_OB = exp(temp$coefficients[x]),
          OR_OW = exp(tempOW$coefficients[x]),
          n = 5000))

temp$coefficients[-1] <- log(OR)



modeldf$zz1ObeseA9[1, 3] <- getIntercept( pObeseOW[9], temp)


temp$coefficients[1] <- modeldf$zz1ObeseA9[1, 3]

z1ObeseLvl1 <- rep(0, 5000)

z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)

print(table(z1ObeseLvl1)[2]/5000)


modeldf$zz1ObeseA9$Estimate <- temp$coefficients

write.csv(modeldf$zz1ObeseA9, paste(modelfiledir, "z1obeseA", 9, ".csv", sep = ""), row.names = FALSE) 

#########################################################################################################


temp <- models$zz1OverweightA10_12

for( i in 10:12){
  
  temp <- models$zz1OverweightA10_12	
  
  sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,as.character(i)])
  
  temp.inter <- numeric(10)
  
  for(j in 1:10) {
    r1SleepLvl1 <- numeric(5000)
    r1SleepLvl2 <- numeric(5000)
    r1SleepLvl3 <- numeric(5000)
    
    r1SleepLvl3[sleepTime[,j] == 3] <- 1
    r1SleepLvl2[sleepTime[,j] == 2] <- 1
    r1SleepLvl1[sleepTime[,j] == 1] <- 1
    
    
    z1RuralLvl1 <- z1RuralLvl1.list[,j]
    
    r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                             z1RuralLvl1 == 1, 1, 0)
    
    temp.inter[j] <- getIntercept(pOverweight[i], temp)
  }
  
  modeldf$zz1OverweightA10_12[1, 3] <- mean(temp.inter)
  
  
  temp$coefficients[1] <- modeldf$zz1OverweightA10_12[1, 3]
  
  z1OverweightLvl1 <- predSimBinom(temp)
  
  print(table(z1OverweightLvl1)[2]/5000)
  
  write.csv(modeldf$zz1OverweightA10_12, paste(modelfiledir, "z1OverweightA", i, ".csv", sep = ""), row.names = FALSE) 
  

  
  
  temp <- models$zz1ObeseA10_12
  
  
  tempOW <- models$zz1OverweightA10_12
  
  
  OR <- sapply(names(temp$coefficients)[-1], function(x) 
    scaleOR(P_OB = pObese[i], 
            P_OW = pOverweight[i], 
            P_yes = prop.table(table(get(x)))[2], 
            OR_OB = exp(temp$coefficients[x]),
            OR_OW = exp(tempOW$coefficients[x]),
            n = 5000))
  
  temp$coefficients[-1] <- log(OR)
  
  
  
  modeldf$zz1ObeseA10_12[1, 3] <- getIntercept( pObeseOW[i], temp)
  
  
  temp$coefficients[1] <- modeldf$zz1ObeseA10_12[1, 3]
  
  z1ObeseLvl1 <- rep(0, 5000)
  
  z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)
  
  print(table(z1ObeseLvl1)[2]/5000)
  
  
  modeldf$zz1ObeseA10_12$Estimate <- temp$coefficients
  
  write.csv(modeldf$zz1ObeseA10_12, paste(modelfiledir, "z1obeseA", i, ".csv", sep = ""), row.names = FALSE) 
  
  
}



temp <- models$zz1OverweightA13_14

for( i in 13:14){
  temp <- models$zz1OverweightA13_14
  
  sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,as.character(i)])
  
  temp.inter <- numeric(10)
  
  for(j in 1:10) {
    r1SleepLvl1 <- numeric(5000)
    r1SleepLvl2 <- numeric(5000)
    r1SleepLvl3 <- numeric(5000)
    
    r1SleepLvl3[sleepTime[,j] == 3] <- 1
    r1SleepLvl2[sleepTime[,j] == 2] <- 1
    r1SleepLvl1[sleepTime[,j] == 1] <- 1
    
    
    z1RuralLvl1 <- z1RuralLvl1.list[,j]
    
    r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                             z1RuralLvl1 == 1, 1, 0)
    
    temp.inter[j] <- getIntercept(pOverweight[i], temp)
  }
  
  modeldf$zz1OverweightA13_14[1, 3] <- mean(temp.inter)
  
  
  
  temp$coefficients[1] <- modeldf$zz1OverweightA13_14[1, 3]
  
  z1OverweightLvl1 <- predSimBinom(temp)
  
  print(table(z1OverweightLvl1)[2]/5000)
  
  write.csv(modeldf$zz1OverweightA13_14, paste(modelfiledir, "z1OverweightA", i, ".csv", sep = ""), row.names = FALSE) 
  
  
  
  temp <- models$zz1ObeseA13_14
  
  
  tempOW <- models$zz1OverweightA13_14
  
  
  OR <- sapply(names(temp$coefficients)[-1], function(x) 
    scaleOR(P_OB = pObese[i], 
            P_OW = pOverweight[i], 
            P_yes = prop.table(table(get(x)))[2], 
            OR_OB = exp(temp$coefficients[x]),
            OR_OW = exp(tempOW$coefficients[x]),
            n = 5000))
  
  temp$coefficients[-1] <- log(OR)
  
  
  
  modeldf$zz1ObeseA13_14[1, 3] <- getIntercept( pObeseOW[i], temp)
  
  
  temp$coefficients[1] <- modeldf$zz1ObeseA13_14[1, 3]
  
  z1ObeseLvl1 <- rep(0, 5000)
  
  z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)
  
  print(table(z1ObeseLvl1)[2]/5000)
  
  
  modeldf$zz1ObeseA13_14$Estimate <- temp$coefficients
  
  write.csv(modeldf$zz1ObeseA13_14, paste(modelfiledir, "z1obeseA", i, ".csv", sep = ""), row.names = FALSE) 
  
}



temp <- models$zz1OverweightA15_18

for( i in 15:18){
  temp <- models$zz1OverweightA15_18	
  
  sleepTime <- sapply(env.base$modules$run_results, function(x) x$r1Sleep[,as.character(i)])
  
  temp.inter <- numeric(10)
  
  for(j in 1:10) {
    r1SleepLvl1 <- numeric(5000)
    r1SleepLvl2 <- numeric(5000)
    r1SleepLvl3 <- numeric(5000)
    
    r1SleepLvl3[sleepTime[,j] == 3] <- 1
    r1SleepLvl2[sleepTime[,j] == 2] <- 1
    r1SleepLvl1[sleepTime[,j] == 1] <- 1
    
    
    z1RuralLvl1 <- z1RuralLvl1.list[,j]
    
    r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                             z1RuralLvl1 == 1, 1, 0)
    
    temp.inter[j] <- getIntercept(pOverweight[i], temp)
  }
  
  modeldf$zz1OverweightA15_18[1, 3] <- mean(temp.inter)
  
  
  temp$coefficients[1] <- modeldf$zz1OverweightA15_18[1, 3]
  
  z1OverweightLvl1 <- predSimBinom(temp)
  
  print(table(z1OverweightLvl1)[2]/5000)
  
  write.csv(modeldf$zz1OverweightA15_18, paste(modelfiledir, "z1OverweightA", i, ".csv", sep = ""), row.names = FALSE) 
  
   
  temp <- models$zz1ObeseA15_18
  
  
  tempOW <- models$zz1OverweightA15_18
  
  
  OR <- sapply(names(temp$coefficients)[-1], function(x) 
    scaleOR(P_OB = pObese[i], 
            P_OW = pOverweight[i], 
            P_yes = prop.table(table(get(x)))[2], 
            OR_OB = exp(temp$coefficients[x]),
            OR_OW = exp(tempOW$coefficients[x]),
            n = 5000))
  
  temp$coefficients[-1] <- log(OR)
  
  
  
  modeldf$zz1ObeseA15_18[1, 3] <- getIntercept( pObeseOW[i], temp)
  
  
  temp$coefficients[1] <- modeldf$zz1ObeseA15_18[1, 3]
  
  z1ObeseLvl1 <- rep(0, 5000)
  
  z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)
  
  print(table(z1ObeseLvl1)[2]/5000)
  
  
  modeldf$zz1ObeseA15_18$Estimate <- temp$coefficients
  
  write.csv(modeldf$zz1ObeseA15_18, paste(modelfiledir, "z1obeseA", i, ".csv", sep = ""), row.names = FALSE) 
  
}

temp <- models$zz1OverweightA19_21


for( i in 19:21){
  temp <- models$zz1OverweightA19_21
  
  temp.inter <- numeric(10)
  
  for(j in 1:10) {

    z1RuralLvl1 <- z1RuralLvl1.list[,j]
    
    r1stchildethnLvl2z1RuralLvl1 <- ifelse(r1stchildethn == 2 &
                                             z1RuralLvl1 == 1, 1, 0)
    
    temp.inter[j] <- getIntercept(pOverweight[i], temp)
  }
  
  modeldf$zz1OverweightA19_21[1, 3] <- mean(temp.inter)
  
  
  temp$coefficients[1] <- modeldf$zz1OverweightA19_21[1, 3]
  
  z1OverweightLvl1 <- predSimBinom(temp)
  
  print(table(z1OverweightLvl1)[2]/5000)
  
  write.csv(modeldf$zz1OverweightA19_21, paste(modelfiledir, "z1OverweightA", i, ".csv", sep = ""), row.names = FALSE) 
  
  temp <- models$zz1ObeseA19_21
  
  
  tempOW <- models$zz1OverweightA19_21
  
  
  OR <- sapply(names(temp$coefficients)[-1], function(x) 
    scaleOR(P_OB = pObese[i], 
            P_OW = pOverweight[i], 
            P_yes = prop.table(table(get(x)))[2], 
            OR_OB = exp(temp$coefficients[x]),
            OR_OW = exp(tempOW$coefficients[x]),
            n = 5000))
  
  temp$coefficients[-1] <- log(OR)
  
  
  
  modeldf$zz1ObeseA19_21[1, 3] <- getIntercept( pObeseOW[i], temp)
  
  
  temp$coefficients[1] <- modeldf$zz1ObeseA19_21[1, 3]
  
  z1ObeseLvl1 <- rep(0, 5000)
  
  z1ObeseLvl1[z1OverweightLvl1 == 1] <- predSimBinom(temp, set = z1OverweightLvl1 == 1)
  
  print(table(z1ObeseLvl1)[2]/5000)
  
  
  modeldf$zz1ObeseA19_21$Estimate <- temp$coefficients
  
  write.csv(modeldf$zz1ObeseA19_21, paste(modelfiledir, "z1obeseA", i, ".csv", sep = ""), row.names = FALSE) 
  
}

#########################################################################	

attach(env.base$simframe, name="simframe")

mean(bwkg*1000) - 124.38 * 0.036
temp <- (bwkg*1000-  3447.981)/124.38
mean(temp)

temp1 <- temp

temp1[temp1 < 0] <- 0
mean(temp1)

temp1[temp1 > 0] <- 1

mean(temp1)

for( i in seq(7.6, 7.7, length = 100)){
  print(i)
  print(mean(ifelse(temp< i, 0, 1)))
}


mean(ifelse(temp<  7.656566, 0,1))


temp1 <- ifelse(temp< 7.656566, 0,1)


temp2 <- sapply(3447.981 + 124.38 * temp1, 
                function(x) rnorm(1, x, sd(bwkg*1000)))
bwkg*1000 - temp2

mean(bwkg*1000)

z1stmDiabete <- temp1


###################################################################

r1stchildethn <-  numeric(5000)

r1stchildethn[which(as.logical(r1stchildethnLvl1))] <- "Euro"
r1stchildethn[which(as.logical(r1stchildethnLvl2))] <- "Pacific"
r1stchildethn[which(as.logical(r1stchildethnLvl3))] <- "Maori"
table(r1stchildethn)

z1HearingLvl1 <- numeric(5000)


z1HearingLvl1 <- rbinom(5000, 1, .4)

table(z1HearingLvl1)/5000

z1HearingLvl1[z1genderLvl1 == 0] <- rbinom(sum(z1genderLvl1 == 0), 1, 0.36)
z1HearingLvl1[z1genderLvl1 == 1] <- rbinom(sum(z1genderLvl1 == 1), 1, 0.44)


table(z1HearingLvl1[z1genderLvl1 == 0])/sum(z1genderLvl1 == 0)


table(z1HearingLvl1[z1genderLvl1 == 1])/sum(z1genderLvl1 == 1)




z1HearingLvl1[z1genderLvl1 == 0 & r1stchildethn == 0] <- 
  rbinom(sum(z1genderLvl1 == 0 & r1stchildethn == 0), 1, 
         (0.36 * 0.231)^(1/2))

z1HearingLvl1[z1genderLvl1 == 1 & r1stchildethn == 0] <- 
  rbinom(sum(z1genderLvl1 == 1 & r1stchildethn == 0), 1, 
         (0.44 * 0.231)^(1/2))

z1HearingLvl1[z1genderLvl1 == 0 & r1stchildethn == "Euro"] <- 
  rbinom(sum(z1genderLvl1 == 0 & r1stchildethn == "Euro"), 1, 
         (0.36 * 0.35)^(1/2))

z1HearingLvl1[z1genderLvl1 == 1 & r1stchildethn == "Euro"] <- 
  rbinom(sum(z1genderLvl1 == 1 & r1stchildethn == "Euro"), 1, 
         (0.44 * 0.35)^(1/2))

z1HearingLvl1[z1genderLvl1 == 0 & r1stchildethn == "Pacific"] <- 
  rbinom(sum(z1genderLvl1 == 0 & r1stchildethn == "Pacific"), 1, 
         (0.36 * 0.5355)^(1/2))

z1HearingLvl1[z1genderLvl1 == 1 & r1stchildethn == "Pacific"] <- 
  rbinom(sum(z1genderLvl1 == 1 & r1stchildethn == "Pacific"), 1, 
         (0.44 * 0.5355)^(1/2))

z1HearingLvl1[z1genderLvl1 == 0 & r1stchildethn == "Maori"] <- 
  rbinom(sum(z1genderLvl1 == 0 & r1stchildethn == "Maori"), 1, 
         (0.36 *  0.518)^(1/2))

z1HearingLvl1[z1genderLvl1 == 1 & r1stchildethn == "Maori"] <- 
  rbinom(sum(z1genderLvl1 == 1 & r1stchildethn == "Maori"), 1, 
         (0.44 *  0.518)^(1/2))


table(z1HearingLvl1)/5000

###################################
z1ECELvl1 <- numeric(5000)


z1ECELvl1[r1stchildethn == 0] <- rbinom(sum( r1stchildethn == 0), 1, .973)
z1ECELvl1[r1stchildethn == "Euro"] <- rbinom(sum( r1stchildethn == "Euro"), 1, .98)
z1ECELvl1[r1stchildethn == "Pacific"] <- rbinom(sum( r1stchildethn == "Pacific"), 1, .912)
z1ECELvl1[r1stchildethn == "Maori"] <- rbinom(sum( r1stchildethn == "Maori"), 1, .94)

table(z1ECELvl1)/5000


###################################
z1PrintExpLvl1 <- numeric(5000)


z1bthorderLvl1 <- ifelse(bthorder ==1, 1,  0)


z1PrintExpLvl1[z1bthorderLvl1==1] <- rbinom(sum( z1bthorderLvl1==1), 1, 1-0.02652867)


z1PrintExpLvl1[z1bthorderLvl1==0] <- rbinom(sum( z1bthorderLvl1==0), 1, 1- 0.007751165)

table(z1PrintExpLvl1)/5000



###################################

z1DrinkLvl1 <- ifelse(pregalc >= 5, 1, 0)

table(z1DrinkLvl1)/5000



write.csv(cbind(z1stmDiabeteLvl1, z1HearingLvl1, z1ECELvl1, z1PrintExpLvl1, z1DrinkLvl1), file = "BaseFileIQ.csv") 
###################################

attach(env.base$simframe, name="simframe")

z1GALvl1 <- as.integer(ga<37)


z1BreastLvl1 <- ifelse(BREAST == 1, 1, 0)
z1BreastLvl2 <- ifelse(BREAST == 2, 1, 0)
z1BreastLvl3 <- ifelse(BREAST == 3 | BREAST == 4, 1, 0)
z1BreastLvl4 <- ifelse(BREAST == 5 | BREAST == 6, 1, 0)
z1BreastLvl5 <- ifelse(BREAST >= 7, 1, 0)
z1stmDiabeteLvl1 <- z1stmDiabeteLvl1
z1HearingLvl1 <- z1HearingLvl1  
z1ECELvl1 <- z1ECELvl1 
z1PrintExpLvl1<- z1PrintExpLvl1
z1DrinkLvl1 <- z1DrinkLvl1 

d <- function(r) 2*r/(1-r^2)



inter = 100 - (-11.94 * mean(z1GALvl1) - 4.98* mean(z1LowBwLvl1) - 
                 0.02 * mean(z1BreastLvl1) + 1.68* mean(z1BreastLvl2) + 
                 2.15 * mean(z1BreastLvl3) + 2.78 * mean(z1BreastLvl4) + 
                 2.91 * mean(z1BreastLvl5) - 0.78 * 15 * mean(z1stmDiabeteLvl1) -
                 6.3 * mean(z1HearingLvl1) +
                 d(0.18) * 15 * mean(z1PrintExpLvl1) - 0.13 * 15 * mean(z1DrinkLvl1))
inter

eMean <-  inter - 11.94*z1GALvl1 - 4.98*z1LowBwLvl1 - 
  0.02 * z1BreastLvl1 + 1.68* z1BreastLvl2 + 
  2.15 * z1BreastLvl3 + 2.78 * z1BreastLvl4 + 
  2.91 * z1BreastLvl5 - 0.78* 15 * z1stmDiabeteLvl1 - 
  6.3 * z1HearingLvl1 +
  d(0.18) * 15 * z1PrintExpLvl1 - 0.13 * 15 *z1DrinkLvl1		

set.seed(1)
simIQ <- sapply(eMean, function(x) rnorm(1, x, sqrt(abs(15^2 - sd(eMean)^2))))

simIQ <- round(simIQ)

mean(simIQ)
sd(simIQ)

##########################################################################
# Using a correlation matrix (let' assume that all variables
# have unit variance
R <- matrix(c(1,0.86, 0.86,1 ),2,2)

U = t(chol(R))
nvars = dim(U)[1]
numobs = length(simIQ)



random.normal = rbind(simIQ, rnorm(5000, simIQ, sqrt(abs(15^2 - sd(simIQ)^2))))
cor(t(random.normal))

X = U %*% random.normal
newX = t(X)
raw = as.data.frame(newX)
orig.raw = as.data.frame(t(random.normal))
cor(raw)

colMeans(raw)

sd(raw[,1])
sd(raw[,2])

raw[,2] <- (scale(raw[,2]))*15  + 100

colMeans(raw)

cor(raw)

##########################################################################
# Using a correlation matrix (let' assume that all variable have unit variance

temp <- c(1,0.86,0.8, 0.8, rep(0.74, 5), rep(0.70,6) )

cycleDes <- function(x, start) {
  index = 1:(length(x)-start+1) 
  
  index1 = 1:(length(x)-start) 
  
  c(rev(x[-1])[-index1],x[index])
}

R = matrix( rbind(t(sapply(1:14, function(x)  
  cycleDes(temp, x))), rev(temp)),nrow=15)

U = t(chol(R))
nvars = dim(U)[1]
numobs = 1000
set.seed(1)
random.normal = matrix(rnorm(nvars*numobs,100,15), nrow=nvars, ncol=numobs);
X = U %*% random.normal
newX = t(X)
raw = as.data.frame(newX)
orig.raw = as.data.frame(t(random.normal))
names(raw) = paste("A", 2:16, sep = "")
cor(raw)


summary(lm(A3 ~ A2, raw))
mean(raw$A3)
mean(raw$A2)

summary(lm(A4 ~ A3, raw))
mean(raw$A4)
mean(raw$A3)

apply(raw, 2, mean)

raw2 = apply(raw, 2, function(x) scale(x) *15 + 100)

round(cor(raw2), 4)

apply(raw2, 2, mean)
apply(raw2, 2, sd)


raw2 <- as.data.frame(raw2)

summary(lm(A3 ~ A2, raw2))


temp <- summary(lm(A3 ~ A2, raw2))$coef[,1]

100 - (temp[2]*mean(raw2$A2) + 0.27*15 * mean(z1ECELvl1))


summary(lm(A4 ~ A3 + A2 , raw2))

temp <- summary(lm(A4 ~ A3 + A2 , raw2))$coef[,1]

100 - (temp[2]*mean(raw2$A3) + temp[3]*mean(raw2$A2) + 0.27*15 * mean(z1ECELvl1))


summary(lm(A5 ~ A4 + A3 + A2 , raw2))
summary(lm(A6 ~ A5 + A4 + A3 + A2 , raw2))
summary(lm(A7 ~ A6 + A5 + A4 + A3 + A2, raw2))
summary(lm(A8 ~ A7 + A6 + A5 + A4 + A3 + A2, raw2))
summary(lm(A9 ~ A8 + A7 +A6 + A5 + A4 + A3 + A2, raw2))
summary(lm(A10 ~ A9 + A8 +A8 + A7 +A6 + A5 + A4 + A3 + A2, raw2))


temp <- 
  rbind(
    summary(lm(A11 ~ A10 + A9 + A8 +A8 + A7 +A6 + A5 + A4 + A3 + A2, raw2))$coef[,1],
    summary(lm(A12 ~ A11 + A10 + A9 + A8 +A8 + A7 +A6 + A5 + A4 + A3 , raw2))$coef[,1],
    summary(lm(A13 ~ A12+ A11 + A10 + A9 + A8 +A8 + A7 +A6 + A5 + A4  , raw2))$coef[,1],
    summary(lm(A14 ~ A13 + A12+ A11 + A10 + A9 + A8 +A8 + A7 +A6 + A5 , raw2))$coef[,1],
    summary(lm(A15 ~ A14 + A13 + A12+ A11 + A10 + A9 + A8 +A8 + A7 +A6, raw2))$coef[,1],
    summary(lm(A16 ~ A15 + A14 + A13 + A12+ A11 + A10 + A9 + A8 +A8 + A7, raw2))$coef[,1])

apply(temp, 2, mean)


mean(
  c(
    summary(lm(A11 ~ A10 + A9 + A8 +A8 + A7 +A6 + A5 + A4 + A3 + A2, raw2))$sigma,
    summary(lm(A12 ~ A11 + A10 + A9 + A8 +A8 + A7 +A6 + A5 + A4 + A3 , raw2))$sigma,
    summary(lm(A13 ~ A12+ A11 + A10 + A9 + A8 +A8 + A7 +A6 + A5 + A4  , raw2))$sigma,
    summary(lm(A14 ~ A13 + A12+ A11 + A10 + A9 + A8 +A8 + A7 +A6 + A5 , raw2))$sigma,
    summary(lm(A15 ~ A14 + A13 + A12+ A11 + A10 + A9 + A8 +A8 + A7 +A6, raw2))$sigma,
    summary(lm(A16 ~ A15 + A14 + A13 + A12+ A11 + A10 + A9 + A8 +A8 + A7, raw2))$sigma
  )
)



################################################################################	


r1stchildethn <-  numeric(5000)

r1stchildethn[which(as.logical(r1stchildethnLvl1))] <- "Euro"
r1stchildethn[which(as.logical(r1stchildethnLvl2))] <- "Pacific"
r1stchildethn[which(as.logical(r1stchildethnLvl3))] <- "Maori"
table(r1stchildethn)

z1ParentInvolveLvl1 <- numeric(5000)


z1ParentInvolveLvl1[r1stchildethn == 0] <- rbinom(sum( r1stchildethn == 0), 1, .75)
z1ParentInvolveLvl1[r1stchildethn == "Euro"] <- rbinom(sum( r1stchildethn == "Euro"), 1, .884)
z1ParentInvolveLvl1[r1stchildethn == "Pacific"] <- rbinom(sum( r1stchildethn == "Pacific"), 1, .812)
z1ParentInvolveLvl1[r1stchildethn == "Maori"] <- rbinom(sum( r1stchildethn == "Maori"), 1, .777)

table(z1ParentInvolveLvl1)/5000

z1ADHDLvl1 <- numeric(5000)


z1ADHDLvl1[z1genderLvl1 == 0 & r1stchildethn == 0] <- 
  rbinom(sum(z1genderLvl1 == 0 & r1stchildethn == 0), 1, 
         0.003)


z1ADHDLvl1[z1genderLvl1 == 1 & r1stchildethn == 0] <- 
  rbinom(sum(z1genderLvl1 == 1 & r1stchildethn == 0), 1, 
         0.013)


z1ADHDLvl1[z1genderLvl1 == 0 & r1stchildethn == "Euro"] <- 
  rbinom(sum(z1genderLvl1 == 0 & r1stchildethn == "Euro"), 1, 
         0.01)


z1ADHDLvl1[z1genderLvl1 == 1 & r1stchildethn == "Euro"] <- 
  rbinom(sum(z1genderLvl1 == 1 & r1stchildethn == "Euro"), 1, 
         0.035)


z1ADHDLvl1[z1genderLvl1 == 0 & r1stchildethn == "Pacific"] <- 
  rbinom(sum(z1genderLvl1 == 0 & r1stchildethn == "Pacific"), 1, 
         0.005)


z1ADHDLvl1[z1genderLvl1 == 1 & r1stchildethn == "Pacific"] <- 
  rbinom(sum(z1genderLvl1 == 1 & r1stchildethn == "Pacific"), 1, 
         0.023)


z1ADHDLvl1[z1genderLvl1 == 0 & r1stchildethn == "Maori"] <- 
  rbinom(sum(z1genderLvl1 == 0 & r1stchildethn == "Maori"), 1, 
         0.012)


z1ADHDLvl1[z1genderLvl1 == 1 & r1stchildethn == "Maori"] <- 
  rbinom(sum(z1genderLvl1 == 1 & r1stchildethn == "Maori"), 1, 
         0.04)

table(z1ADHDLvl1)/5000



write.csv(cbind(z1ADHDLvl1,z1ParentInvolveLvl1), file = "BaseFileScore.csv") 



table(env.base$simframe$z1ADHDLvl1)/5000



table(env.base$simframe$z1ParentInvolveLvl1)/5000

###############################################################

score <- read.csv(file.choose())

score %>% group_by(ETHN) %>% summarise(mean(SCORE))
score %>% group_by(GENDER) %>% summarise(mean(SCORE))
score %>% group_by(GENDER, ETHN) %>% summarise(mean(SCORE))

scoreOther <- score %>% filter(ETHN != "M")

ethnlvl3 <- ifelse(scoreOther$ETHN == "P", 1, 0)
ethnlvl4 <- ifelse(scoreOther$ETHN == "A", 1, 0)

summary(lm(SCORE ~ factor(GENDER) + ethnlvl3 + ethnlvl4, data = scoreOther))

scoreM <- score %>% filter(ETHN == "M")

ethnlvl2 <- ifelse(scoreM$ETHN == "M", 1, 0)

summary(lm(SCORE ~ factor(GENDER), data = scoreM))



foo <- function(){
  FE <- 105
  score.FE <- rnorm(1476, FE, 15.5)
  
  c(table(score.FE > 90)[2]/1476, 
    table(score.FE <= 90 & score.FE >82)[2]/1476,
    table(score.FE <= 82)[2]/1476)
}

rowMeans(replicate(1000,foo()))


foo <- function(){
  ME <- 102.15
  score.ME <- rnorm(1407, ME, 15.5)
  
  c(table(score.ME > 90)[2]/1407,
    table(score.ME <= 90 & score.ME >82)[2]/1407,
    table(score.ME <= 82)[2]/1407)
}

rowMeans(replicate(1000,foo()))



foo <- function(){
  
  FP <- 101.5
  score.FP <- rnorm(226, FP, 15.5)
  c(table(score.FP > 90)[2]/226,
    table(score.FP <= 90 & score.FP >82)[2]/226,
    table(score.FP <= 82)[2]/226)
}

rowMeans(replicate(1000,foo()))


foo <- function(){
  
  MP <- 97.5
  score.MP <- rnorm(220, MP, 15.5)
  c(table(score.MP > 90)[2]/220,
    table(score.MP <= 90 & score.MP >82)[2]/220,
    table(score.MP <= 82)[2]/220)
  
}

rowMeans(replicate(100,foo()))



foo <- function(){
  
  FA <- 112.5
  score.FA <- rnorm(221, FA, 15.5)
  
  c(table(score.FA > 90)[2]/221,
    table(score.FA <= 90 & score.FA >82)[2]/221,
    table(score.FA <= 82)[2]/221)
  
}

temp <- replicate(1000,foo())
rowMeans(temp)





foo <- function(){
  
  MA <- 106.9
  score.MA <- rnorm(203, MA, 15.5)
  
  c(table(score.MA > 90)[2]/203,
    table(score.MA <= 90 & score.MA >82)[2]/203,
    table(score.MA <= 82)[2]/203)
}

temp <- replicate(1000,foo())
rowMeans(temp)




score.df <- data.frame(
  ETH = factor(rep(c("E", "P", "A"),  c(1476 +1407, 226 + 220, 221 + 203))),
  GENDER =  factor( c(rep(c("F", "M"),  c(1476, 1407)),  
                      rep(c("F", "M"),  c(226, 220)),
                      rep(c("F", "M"),  c(221, 203)))),
  SCORE = c(score.FE, score.ME, score.FP, score.MP, score.FA, score.MA)
)

ethnlvl3 <- ifelse(score.df$ETH == "P", 1, 0)
ethnlvl4 <- ifelse(score.df$ETH == "A", 1, 0)

fit <- lm(SCORE ~ GENDER + ethnlvl3 + ethnlvl4, data = score.df)
summary(fit)

FE <- 105
ME <- 102.15
FP <- 101.5
MP <- 97.5
FA <- 112.5
MA <- 106.9


mean(c(102.15,97.5,106.9 ,105,101.5,112.5 ))

mean(c(102.15,97.5,106.9 )) - mean(c(105,101.5,112.5 ))

mean(c(101.5,97.5 )) - mean(c(105,102.15 ))
mean(c(112.5,106.9 )) - mean(c(105,102.15 ))


score.df$ETH <- factor(as.character(score.df$ETH), levels = c("E", "P", "A"))
summary(lm(SCORE ~ GENDER + ETH, data = score.df))


FM <- 92.5
score.FM <- rnorm(628, FM, 9)
table(score.FM > 90)/628
table(score.FM <= 90 & score.FM >82)/628
table(score.FM <= 82)/628


MM <- 91
score.MM <- rnorm(619, MM, 9)
table(score.MM > 90)/619
table(score.MM <= 90 & score.MM >82)/619
table(score.MM <= 82)/619

score.maori <- data.frame(
  GENDER <- factor(rep(c("F", "M"),  c(628, 619))),
  SCORE <- c(score.FM, score.MM)
)

summary(lm(SCORE ~ GENDER, data = score.maori))


##############################################################

attach(env.base$simframe, name="simframe")

simScore <- numeric(5000)

(meanScore = mean(score.df$SCORE ))
(sdScore = sd(score.df$SCORE ))

# > (meanScore = mean(score.df$SCORE ))
# [1] 103.3947
# > (sdScore = sd(score.df$SCORE ))
# [1] 15.73345

d <- function(r) 2*r/(1-r^2)

inter = meanScore - (-4.15 * mean(z1genderLvl1[r1stchildethn != 2])  -
                       4.075 * mean(r1stchildethnLvl3[r1stchildethn != 2]) +
                       6.125  * mean(r1stchildethnLvl4[r1stchildethn != 2]) - 
                       0.27 * sdScore * mean(SESBTHLvl3[r1stchildethn != 2]) -  
                       0.3 * sdScore * mean(r1ParentEducLvl3[r1stchildethn != 2]) +
                       d(0.3) * sdScore * mean(z1PrintExpLvl1[r1stchildethn != 2]) + 
                       0.137*sdScore * mean(z1ECELvl1[r1stchildethn != 2]) -
                       d(-0.32) * sdScore * mean(z1ADHDLvl1[r1stchildethn != 2]) + 
                       0.35* sdScore* mean(z1ParentInvolveLvl1[r1stchildethn != 2]) + 0.6 * 100 )

inter

eMean <-  inter -4.15 * (z1genderLvl1[r1stchildethn != 2]) -0 * r1stchildethnLvl2[r1stchildethn != 2] -
  4.075 * r1stchildethnLvl3[r1stchildethn != 2] + 6.125 * r1stchildethnLvl4[r1stchildethn != 2] - 
  0.27 * sdScore * (SESBTHLvl3[r1stchildethn != 2]) -  0.3 * sdScore * (r1ParentEducLvl3[r1stchildethn != 2]) +
  d(0.3) * sdScore * (z1PrintExpLvl1[r1stchildethn != 2]) + 0.137*sdScore * (z1ECELvl1[r1stchildethn != 2])-
  d(-0.32) * sdScore * (z1ADHDLvl1[r1stchildethn != 2]) +  0.35* sdScore* mean(z1ParentInvolveLvl1[r1stchildethn != 2]) + 0.6 * 100



simScore[r1stchildethn != 2] <- sapply(eMean, function(x) rnorm(1, x, sdScore))


(meanScore = mean(c(score.MM, score.FM)))
(sdScore = sd(c(score.MM, score.FM)))

# > (meanScore = mean(c(score.MM, score.FM)))
# [1] 91.6008
# > (sdScore = sd(c(score.MM, score.FM)))
# [1] 8.936842

inter = meanScore - (-1.5 * mean(z1genderLvl1[r1stchildethn == 2])  - 
                       0.27 * sdScore * mean(SESBTHLvl3[r1stchildethn == 2]) -  
                       0.3 * sdScore * mean(r1ParentEducLvl3[r1stchildethn == 2]) +
                       d(0.3) * sdScore * mean(z1PrintExpLvl1[r1stchildethn == 2]) + 
                       0.137*sdScore * mean(z1ECELvl1[r1stchildethn == 2]) -
                       d(-0.32) * sdScore * mean(z1ADHDLvl1[r1stchildethn == 2]) + 
                       0.35* sdScore* mean(z1ParentInvolveLvl1[r1stchildethn == 2]) + 0.6 * 100 )

inter


eMean <-  inter -1.5 * (z1genderLvl1[r1stchildethn == 2])  - 
  0.27 * sdScore * (SESBTHLvl3[r1stchildethn == 2]) -  
  0.3 * sdScore * (r1ParentEducLvl3[r1stchildethn == 2]) +
  d(0.3) * sdScore * (z1PrintExpLvl1[r1stchildethn == 2]) + 
  0.137*sdScore * (z1ECELvl1[r1stchildethn == 2])-
  d(0.32) * sdScore * (z1ADHDLvl1[r1stchildethn == 2]) +  
  0.35* sdScore* mean(z1ParentInvolveLvl1[r1stchildethn == 2]) + 0.6 * 100



simScore[r1stchildethn == 2]  <- sapply(eMean, function(x) rnorm(1, x, sdScore))


#simScore <- scale(simScore) * sdScore + mean(simScore)

z1ScoreLvl1 <- ifelse(simScore > 90, 1,0)
z1FailLvl1 <- ifelse(simScore <= 82, 1,0)
z1DropLvl1 <- ifelse(simScore > 82 & simScore <= 90, 1,0)

table(z1ScoreLvl1)[2]/5000
table(z1DropLvl1)[2]/5000
table(z1FailLvl1)[2]/5000

proptable <- function(x) round(table(x)[2]/length(x), 3)

tapply(z1ScoreLvl1, interaction(z1genderLvl1, r1stchildethn),  proptable)
tapply(z1DropLvl1, interaction(z1genderLvl1, r1stchildethn), proptable)
tapply(z1FailLvl1, interaction(z1genderLvl1, r1stchildethn), proptable)





############################################################################


env.base$modules$years1_21$run_results_collated$means$IQ

env.base$modules$years1_21$run_results_collated$means$Score

env.base$modules$years1_21$run_results_collated$freqs$z1ScoreLvl1


env.base$modules$years1_21$run_results$means$IQ

cor(env.base$modules$years1_21$run_results$IQ)

apply(env.base$modules$years1_21$run_results$IQ, 2, sd)
apply(env.base$modules$years1_21$run_results$Score, 2, sd)

round(cor(env.base$modules$years1_21$run_results$IQ)[2:16,2:16], 4)

round(cor(env.base$modules$years1_21$run_results[[5]]$IQ)[2:16,2:16], 4)


cor(env.base$modules$years1_21$run_results[[2]]$IQ[,16],
    env.base$modules$years1_21$run_results[[2]]$Score[,17])


compare = 
  function(env.base, env.scenario){
    
    Base <-	
      sapply(env.base$modules$years1_21$run_results, function(x) sapply(x$means$IQ[-1], function(y) y))	
    
    Scenario <-
      sapply(env.scenario$modules$years1_21$run_results, function(x) sapply(x$means$IQ[-1], function(y) y))	
    
    results <- numeric(3)
    
    for(i in 1:20){
      lm.fit <- lm(c(Base[i,], Scenario[i,]) ~ factor(rep(c("B", "S"), each = 10)))
      
      results <- 
        rbind(results,c(summary(lm.fit )$coef[2,1],confint(	lm.fit )[2,]))
    }
    
    results <- cbind((1:21), results)
    
    colnames(results) <- c("Age", "Mean Diff", "Lower CI", "Upper CI")
    results <- results[2:16,]
    
    round(results, 4)	
  }

############################################################################


write.csv(cbind(z1stmDiabeteLvl1, z1HearingLvl1, z1ECELvl1, z1PrintExpLvl1, z1DrinkLvl1), file = "BaseFileIQ.csv") 


table(env.base$simframe$z1stmDiabeteLvl1)/5000


table(env.base$simframe$z1HearingLvl1)/5000


table(env.base$simframe$z1ECELvl1)/5000


table(env.base$simframe$z1PrintExpLvl1)/5000

table(env.base$simframe$z1LowBwLvl1)/5000


table(env.base$simframe$z1BreastLvl1)/5000
table(env.base$simframe$z1BreastLvl2)/5000
table(env.base$simframe$z1BreastLvl3)/5000
table(env.base$simframe$z1BreastLvl4)/5000
table(env.base$simframe$z1BreastLvl5)/5000

#############################################################################

modelfiledir <- paste(getwd(),"/models/",sep="")

#models <- loadMELCModels(modelfiledir)
#checkModelVars(models, simframe.master)


########################################################################
#Female

pNEET <- c(2.3375, 5.5625, 8.7875,12.0125,15.2375,18.4625)/100		

temp <- models$zz1NEETGender0A16_17	

modeldf$zz1NEETGender0A16_17[1, 3]<- getIntercept( pNEET[1], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender0A16_17[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender0A16_17, paste(modelfiledir, "z1NEETGender0A16.csv", sep = ""), row.names = FALSE) 

temp <- models$zz1NEETGender0A16_17	

modeldf$zz1NEETGender0A16_17[1, 3]<- getIntercept( pNEET[2], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender0A16_17[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))


write.csv(modeldf$zz1NEETGender0A16_17, paste(modelfiledir, "z1NEETGender0A17.csv", sep = ""), row.names = FALSE) 


temp <- models$zz1NEETGender0A18_21

z1ScoreLvl1 <- env.base$modules$run_results$run1$z1ScoreLvl1[,1]


modeldf$zz1NEETGender0A18_21[1, 3]<- getIntercept( pNEET[3], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender0A18_21[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender0A18_21, paste(modelfiledir, "z1NEETGender0A18.csv", sep = ""), row.names = FALSE) 



temp <- models$zz1NEETGender0A18_21	

z1ScoreLvl1 <- env.base$modules$run_results$run1$z1ScoreLvl1[,1]


modeldf$zz1NEETGender0A18_21[1, 3]<- getIntercept( pNEET[4], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender0A18_21[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender0A18_21, paste(modelfiledir, "z1NEETGender0A19.csv", sep = ""), row.names = FALSE) 



temp <- models$zz1NEETGender0A18_21

z1ScoreLvl1 <- env.base$modules$run_results$run1$z1ScoreLvl1[,1]


modeldf$zz1NEETGender0A18_21[1, 3]<- getIntercept( pNEET[5], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender0A18_21[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender0A18_21, paste(modelfiledir, "z1NEETGender0A20.csv", sep = ""), row.names = FALSE) 



temp <- models$zz1NEETGender0A18_21

z1ScoreLvl1 <- env.base$modules$run_results$run1$z1ScoreLvl1[,1]


modeldf$zz1NEETGender0A18_21[1, 3]<- getIntercept( pNEET[6], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender0A18_21[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender0A18_21, paste(modelfiledir, "z1NEETGender0A21.csv", sep = ""), row.names = FALSE) 



#Male

pNEET <- c(2.9625, 5.1375, 7.3125, 9.4875, 11.6625, 13.8375)/100		


temp <- models$zz1NEETGender1A16_17	

modeldf$zz1NEETGender1A16_17[1, 3]<- getIntercept( pNEET[1], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender1A16_17[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender1A16_17, paste(modelfiledir, "z1NEETGender1A16.csv", sep = ""), row.names = FALSE) 

temp <- models$zz1NEETGender1A16_17	

modeldf$zz1NEETGender1A16_17[1, 3]<- getIntercept( pNEET[2], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender1A16_17[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))


write.csv(modeldf$zz1NEETGender1A16_17, paste(modelfiledir, "z1NEETGender1A17.csv", sep = ""), row.names = FALSE) 


temp <- models$zz1NEETGender1A18_21

z1ScoreLvl1 <- env.base$modules$run_results$run1$z1ScoreLvl1[1]


modeldf$zz1NEETGender1A18_21[1, 3]<- getIntercept( pNEET[3], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender1A18_21[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender1A18_21, paste(modelfiledir, "z1NEETGender1A18.csv", sep = ""), row.names = FALSE) 



temp <- models$zz1NEETGender1A18_21	

z1ScoreLvl1 <- env.base$modules$run_results$run1$z1ScoreLvl1[,1]


modeldf$zz1NEETGender1A18_21[1, 3]<- getIntercept( pNEET[4], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender1A18_21[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender1A18_21, paste(modelfiledir, "z1NEETGender1A19.csv", sep = ""), row.names = FALSE) 



temp <- models$zz1NEETGender1A18_21

z1ScoreLvl1 <- env.base$modules$run_results$run1$z1ScoreLvl1[,1]


modeldf$zz1NEETGender1A18_21[1, 3]<- getIntercept( pNEET[5], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender1A18_21[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender1A18_21, paste(modelfiledir, "z1NEETGender1A20.csv", sep = ""), row.names = FALSE) 



temp <- models$zz1NEETGender1A18_21

z1ScoreLvl1 <- env.base$modules$run_results$run1$z1ScoreLvl1[,1]


modeldf$zz1NEETGender1A18_21[1, 3]<- getIntercept( pNEET[6], temp, set = z1genderLvl1 == 0)

temp$coefficients[1] <- modeldf$zz1NEETGender1A18_21[1, 3]

z1NEETLvl1 <- predSimBinom(temp, set = z1genderLvl1 == 0)


print(table(z1NEETLvl1)[2]/ sum(z1genderLvl1 == 0))

write.csv(modeldf$zz1NEETGender1A18_21, paste(modelfiledir, "z1NEETGender1A21.csv", sep = ""), row.names = FALSE) 

#############################################################################

modelfiledir <- paste(getwd(),"/models/",sep="")

#models <- loadMELCModels(modelfiledir)
#checkModelVars(models, simframe.master)



########################################################################
#Bully


pBully <- c(7.3, 7.9, 6.8, 4.2, 3.5)/100

temp <- models$zz1BullyA15	

tempInter <- numeric(10)

for(i in 1:10) {
  
  r1SchoolFunding<-env.base$modules$run_results[[paste0("run", i)]]$r1SchoolFunding[,1]
  r1SchoolFundingLvl1 <- ifelse(r1SchoolFunding == 1, 1,0)
  r1SchoolFundingLvl2 <- ifelse(r1SchoolFunding == 2, 1,0)
  
  tempInter[i]<- getIntercept( pBully[1], temp)
}

modeldf$zz1BullyA15[1, 3] <- mean(tempInter)

temp$coefficients[1] <- modeldf$zz1BullyA15[1, 3]

z1BullyLvl1 <- predSimBinom(temp)

print(table(z1BullyLvl1)[2]/5000)

write.csv(modeldf$zz1BullyA15, paste(modelfiledir, "z1BullyA13.csv", sep = ""), row.names = FALSE) 


temp <- models$zz1BullyA15	

tempInter <- numeric(10)

for(i in 1:10) {
  
  r1SchoolFunding<-env.base$modules$run_results[[paste0("run", i)]]$r1SchoolFunding[,1]
  r1SchoolFundingLvl1 <- ifelse(r1SchoolFunding == 1, 1,0)
  r1SchoolFundingLvl2 <- ifelse(r1SchoolFunding == 2, 1,0)
  
  tempInter[i]<- getIntercept( pBully[2], temp)
}

modeldf$zz1BullyA15[1, 3] <- mean(tempInter)

temp$coefficients[1] <- modeldf$zz1BullyA15[1, 3]

z1BullyLvl1 <- predSimBinom(temp)


print(table(z1BullyLvl1)[2]/5000)

write.csv(modeldf$zz1BullyA15, paste(modelfiledir, "z1BullyA14.csv", sep = ""), row.names = FALSE) 


temp <- models$zz1BullyA15	

tempInter <- numeric(10)

for(i in 1:10) {
  
  r1SchoolFunding<-env.base$modules$run_results[[paste0("run", i)]]$r1SchoolFunding[,1]
  r1SchoolFundingLvl1 <- ifelse(r1SchoolFunding == 1, 1,0)
  r1SchoolFundingLvl2 <- ifelse(r1SchoolFunding == 2, 1,0)
  
  tempInter[i]<- getIntercept( pBully[3], temp)
}

modeldf$zz1BullyA15[1, 3] <- mean(tempInter)

temp$coefficients[1] <- modeldf$zz1BullyA15[1, 3]

z1BullyLvl1 <- predSimBinom(temp)


print(table(z1BullyLvl1)[2]/5000)

write.csv(modeldf$zz1BullyA15, paste(modelfiledir, "z1BullyA15.csv", sep = ""), row.names = FALSE) 

temp <- models$zz1BullyA16	


tempInter <- numeric(10)

for(i in 1:10) {
  
  r1SchoolFunding<-env.base$modules$run_results[[paste0("run", i)]]$r1SchoolFunding[,1]
  r1SchoolFundingLvl1 <- ifelse(r1SchoolFunding == 1, 1,0)
  r1SchoolFundingLvl2 <- ifelse(r1SchoolFunding == 2, 1,0)
  
  tempInter[i]<- getIntercept( pBully[4], temp)
}

modeldf$zz1BullyA16[1, 3] <- mean(tempInter)

temp$coefficients[1] <- modeldf$zz1BullyA16[1, 3]

z1BullyLvl1 <- predSimBinom(temp)


print(table(z1BullyLvl1)[2]/5000)

write.csv(modeldf$zz1BullyA16, paste(modelfiledir, "z1BullyA16.csv", sep = ""), row.names = FALSE) 

temp <- models$zz1BullyA17_21	

tempInter <- numeric(10)

for(i in 1:10) {
  
  r1SchoolFunding<-env.base$modules$run_results[[paste0("run", i)]]$r1SchoolFunding[,1]
  r1SchoolFundingLvl1 <- ifelse(r1SchoolFunding == 1, 1,0)
  r1SchoolFundingLvl2 <- ifelse(r1SchoolFunding == 2, 1,0)
  
  tempInter[i]<- getIntercept( pBully[5], temp)
}

modeldf$zz1BullyA17_21[1, 3] <- mean(tempInter)

temp$coefficients[1] <- modeldf$zz1BullyA17_21[1, 3]

z1BullyLvl1 <- predSimBinom(temp)


print(table(z1BullyLvl1)[2]/5000)

write.csv(modeldf$zz1BullyA17_21, paste(modelfiledir, "z1BullyA17.csv", sep = ""), row.names = FALSE) 



###########################################################################################################

modelfiledir <- paste(getwd(),"/models/",sep="")

#models <- loadMELCModels(modelfiledir)
#checkModelVars(models, simframe.master)



fileName <- list.files(modelfiledir)
modelName <- sapply(strsplit(list.files(modelfiledir),"\\."), function(x) x[1])
models <- modeldf <- vector(length(modelName), mode = "list")
names(models) <- names(modeldf) <- modelName

for(i in 1:length(modelName)) {
  print(i)
  models[[i]] <-  loadGLMCSV(modelfiledir, fileName[i])
  
  modeldf[[i]] <-  read.csv(paste(modelfiledir, fileName[i], sep =""))
  
}	

attach(env.base$simframe, name="simframe")


#############################################################################


pAlcAbuse <- c(3, 6, 23.4, 30, 33, 35, 35)/100

temp <- models$zz1AlcAbuseA15	

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentAlcLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentAlcLvl1[,"15"]
  
  tempInter[i]<- getIntercept(pAlcAbuse[1], temp)
}

modeldf$zz1AlcAbuseA15[1, 3] <- mean(tempInter)

temp$coefficients[1] <- modeldf$zz1AlcAbuseA15[1, 3]

z1AlcAbuseLvl1 <- predSimBinom(temp)

print(table(z1AlcAbuseLvl1)[2]/ 5000)

write.csv(modeldf$zz1AlcAbuseA15, paste(modelfiledir, "z1AlcAbuseA15.csv", sep = ""), row.names = FALSE) 


temp <- models$zz1AlcAbuseA16	

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentAlcLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentAlcLvl1[,"16"]
  
  tempInter[i]<- getIntercept(pAlcAbuse[2], temp)
}

modeldf$zz1AlcAbuseA16[1, 3] <- mean(tempInter)

temp$coefficients[1] <- modeldf$zz1AlcAbuseA16[1, 3]

z1AlcAbuseLvl1 <- predSimBinom(temp)

print(table(z1AlcAbuseLvl1)[2]/ 5000)

write.csv(modeldf$zz1AlcAbuseA16, paste(modelfiledir, "z1AlcAbuseA16.csv", sep = ""), row.names = FALSE) 





temp <- models$zz1AlcAbuseA17	

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentAlcLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentAlcLvl1[,"17"]
  
  tempInter[i]<- getIntercept(pAlcAbuse[3], temp)
}

modeldf$zz1AlcAbuseA17[1, 3] <- mean(tempInter)

temp$coefficients[1] <- modeldf$zz1AlcAbuseA17[1, 3]

z1AlcAbuseLvl1 <- predSimBinom(temp)


print(table(z1AlcAbuseLvl1)[2]/ 5000)

write.csv(modeldf$zz1AlcAbuseA17, paste(modelfiledir, "z1AlcAbuseA17.csv", sep = ""), row.names = FALSE) 


temp <- models$zz1AlcAbuseA18	

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentAlcLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentAlcLvl1[,"18"]
  
  tempInter[i]<- getIntercept(pAlcAbuse[4], temp)
}

modeldf$zz1AlcAbuseA18[1, 3] <- mean(tempInter)

temp$coefficients[1] <- mean(tempInter)

z1AlcAbuseLvl1 <- predSimBinom(temp)


print(table(z1AlcAbuseLvl1)[2]/ 5000)

write.csv(modeldf$zz1AlcAbuseA18, paste(modelfiledir, "z1AlcAbuseA18.csv", sep = ""), row.names = FALSE) 






temp <- models$zz1AlcAbuseA19	

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentAlcLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentAlcLvl1[,"19"]
  
  tempInter[i]<- getIntercept(pAlcAbuse[5], temp)
}

modeldf$zz1AlcAbuseA19[1, 3] <- mean(tempInter)

temp$coefficients[1] <- mean(tempInter)

z1AlcAbuseLvl1 <- predSimBinom(temp)


print(table(z1AlcAbuseLvl1)[2]/ 5000)

write.csv(modeldf$zz1AlcAbuseA19, paste(modelfiledir, "z1AlcAbuseA19.csv", sep = ""), row.names = FALSE) 





temp <- models$zz1AlcAbuseA20

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentAlcLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentAlcLvl1[,"20"]
  
  tempInter[i]<- getIntercept(pAlcAbuse[6], temp)
}

modeldf$zz1AlcAbuseA20[1, 3] <- mean(tempInter)

temp$coefficients[1] <- mean(tempInter)

z1AlcAbuseLvl1 <- predSimBinom(temp)


print(table(z1AlcAbuseLvl1)[2]/ 5000)

write.csv(modeldf$zz1AlcAbuseA20, paste(modelfiledir, "z1AlcAbuseA20.csv", sep = ""), row.names = FALSE) 




temp <- models$zz1AlcAbuseA21	

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentAlcLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentAlcLvl1[,"21"]
  
  tempInter[i]<- getIntercept(pAlcAbuse[6], temp)
}

modeldf$zz1AlcAbuseA21[1, 3] <- mean(tempInter)

temp$coefficients[1] <- modeldf$zz1AlcAbuseA21[1, 3]

z1AlcAbuseLvl1 <- predSimBinom(temp)


print(table(z1AlcAbuseLvl1)[2]/ 5000)

write.csv(modeldf$zz1AlcAbuseA21, paste(modelfiledir, "z1AlcAbuseA21.csv", sep = ""), row.names = FALSE) 


##############################################################################################################



total = c(8.1, 14.1, 17.3, 17.6, 18.0, 15.0, 10.9)
age = c(19.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5)

pDepress <- predict(lm(total ~ I(age) + I(age^2)),  data.frame(age = 15:21 ))/100
#pDepress <- pDepress/ 1.185185 #<- have to check it again


# 1          2          3          4          5          6          7 
# 0.05437045 0.06134116 0.06811830 0.07470188 0.08109188 0.08728830 0.09329116 



temp <- models$zz1DepressA15	

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentDepressLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentDepressLvl1[,"15"]
  
  z1INTERACTLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1INTERACTLvl1[,1]
  z1PUNISHLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1PUNISHLvl1[,1]
  
  z1BullyLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1BullyLvl1[,"15"]
  z1ObeseLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ObeseLvl1[,"15"]
  
  tempInter[i]<- getIntercept(pDepress[1], temp)
}

modeldf$zz1DepressA15[1, 3] <-mean(tempInter)


temp$coefficients[1] <- modeldf$zz1DepressA15[1, 3]

z1DepressLvl1 <- predSimBinom(temp)


print(table(z1DepressLvl1)[2]/ 5000)

write.csv(modeldf$zz1DepressA15, paste(modelfiledir, "z1DepressA15.csv", sep = ""), row.names = FALSE) 





temp <- models$zz1DepressA15	

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentDepressLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentDepressLvl1[,"16"]
  
  z1INTERACTLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1INTERACTLvl1[,1]
  z1PUNISHLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1PUNISHLvl1[,1]
  
  z1BullyLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1BullyLvl1[,"16"]
  z1ObeseLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ObeseLvl1[,"16"]
  
  tempInter[i]<- getIntercept(pDepress[2], temp)
}

modeldf$zz1DepressA15[1, 3] <- mean(tempInter)


temp$coefficients[1] <- modeldf$zz1DepressA15[1, 3]

z1DepressLvl1 <- predSimBinom(temp)


print(table(z1DepressLvl1)[2]/ 5000)

write.csv(modeldf$zz1DepressA15, paste(modelfiledir, "z1DepressA16.csv", sep = ""), row.names = FALSE) 


temp <- models$zz1DepressA15	

tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentDepressLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentDepressLvl1[,"17"]
  
  z1INTERACTLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1INTERACTLvl1[,1]
  z1PUNISHLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1PUNISHLvl1[,1]
  
  z1BullyLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1BullyLvl1[,"17"]
  z1ObeseLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ObeseLvl1[,"17"]
  
  tempInter[i]<- getIntercept(pDepress[3], temp)
}

modeldf$zz1DepressA15[1, 3] <- mean(tempInter)


temp$coefficients[1] <- modeldf$zz1DepressA15[1, 3]

z1DepressLvl1 <- predSimBinom(temp)


print(table(z1DepressLvl1)[2]/ 5000)

write.csv(modeldf$zz1DepressA15, paste(modelfiledir, "z1DepressA17.csv", sep = ""), row.names = FALSE) 




temp <- models$zz1DepressA15	


tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentDepressLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentDepressLvl1[,"18"]
  
  z1INTERACTLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1INTERACTLvl1[,1]
  z1PUNISHLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1PUNISHLvl1[,1]
  
  z1BullyLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1BullyLvl1[,"18"]
  z1ObeseLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ObeseLvl1[,"18"]
  
  tempInter[i]<- getIntercept(pDepress[4], temp)
}

modeldf$zz1DepressA15[1, 3] <- mean(tempInter)


temp$coefficients[1] <- modeldf$zz1DepressA15[1, 3]

z1DepressLvl1 <- predSimBinom(temp)


print(table(z1DepressLvl1)[2]/ 5000)

write.csv(modeldf$zz1DepressA15, paste(modelfiledir, "z1DepressA18.csv", sep = ""), row.names = FALSE) 




temp <- models$zz1DepressA15	


tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentDepressLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentDepressLvl1[,"19"]
  
  z1INTERACTLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1INTERACTLvl1[,1]
  z1PUNISHLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1PUNISHLvl1[,1]
  
  z1BullyLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1BullyLvl1[,"19"]
  z1ObeseLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ObeseLvl1[,"19"]
  
  tempInter[i]<- getIntercept(pDepress[5], temp)
}

modeldf$zz1DepressA15[1, 3] <- mean(tempInter)


temp$coefficients[1] <- modeldf$zz1DepressA15[1, 3]

z1DepressLvl1 <- predSimBinom(temp)


print(table(z1DepressLvl1)[2]/ 5000)

write.csv(modeldf$zz1DepressA15, paste(modelfiledir, "z1DepressA19.csv", sep = ""), row.names = FALSE) 




temp <- models$zz1DepressA15	


tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentDepressLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentDepressLvl1[,"20"]
  
  z1INTERACTLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1INTERACTLvl1[,1]
  z1PUNISHLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1PUNISHLvl1[,1]
  
  z1BullyLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1BullyLvl1[,"20"]
  z1ObeseLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ObeseLvl1[,"20"]
  
  tempInter[i]<- getIntercept(pDepress[6], temp)
}

modeldf$zz1DepressA15[1, 3] <- mean(tempInter)


temp$coefficients[1] <- modeldf$zz1DepressA15[1, 3]

z1DepressLvl1 <- predSimBinom(temp)


print(table(z1DepressLvl1)[2]/ 5000)

write.csv(modeldf$zz1DepressA15, paste(modelfiledir, "z1DepressA20.csv", sep = ""), row.names = FALSE) 





temp <- models$zz1DepressA15	


tempInter <- numeric(10)

for(i in 1:10) {
  
  z1ParentDepressLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ParentDepressLvl1[,"21"]
  
  z1INTERACTLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1INTERACTLvl1[,1]
  z1PUNISHLvl1<- env.base$modules$run_results[[paste0("run", i)]]$z1PUNISHLvl1[,1]
  
  z1BullyLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1BullyLvl1[,"21"]
  z1ObeseLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1ObeseLvl1[,"21"]
  
  z1DepressLvl1 <- env.base$modules$run_results[[paste0("run", i)]]$z1DepressLvl1[,"21"]

  tempInter[i]<- getIntercept(pDepress[7], temp)
}

modeldf$zz1DepressA15[1, 3] <- mean(tempInter)


temp$coefficients[1] <- modeldf$zz1DepressA15[1, 3]

z1DepressLvl1 <- predSimBinom(temp)


print(table(z1DepressLvl1)[2]/ 5000)

write.csv(modeldf$zz1DepressA15, paste(modelfiledir, "z1DepressA21.csv", sep = ""), row.names = FALSE) 



##########################################################################################################


detach("simframe")



