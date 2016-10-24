



bmiFun <- 
function(BMI){
  
  print(
  cbind(
    apply(BMI[BMI$r1stchildethn==1, paste0("BMI", 2:21)], 2, mean),
    apply(BMI[BMI$r1stchildethn==2, paste0("BMI", 2:21)], 2, mean),
    apply(BMI[BMI$r1stchildethn==3, paste0("BMI", 2:21)], 2, mean),
    apply(BMI[BMI$r1stchildethn==4, paste0("BMI", 2:21)], 2, mean)))
  
  print(
  cbind(
    apply(BMI[BMI$r1stchildethn==1, paste0("BMI", 2:21)], 2, sd),
    apply(BMI[BMI$r1stchildethn==2, paste0("BMI", 2:21)], 2, sd),
    apply(BMI[BMI$r1stchildethn==3, paste0("BMI", 2:21)], 2, sd),
    apply(BMI[BMI$r1stchildethn==4, paste0("BMI", 2:21)], 2, sd)))
}




overweightCutoff <<-
  matrix(
    c(c(18.4, 17.9, 17.6, 17.4, 17.6, 17.9, 18.4, 19.1, 19.8, 20.6, 21.2, 21.9, 22.6, 
        23.3, 23.9, 24.5, rep(25, 4)),
      c(18, 17.6, 17.3, 17.1, 17.3, 17.8, 18.3, 19.1, 19.9, 20.7, 21.7, 22.6, 23.3, 
        23.9, 24.4, 24.7, rep(25, 4))),
    ncol = 2)

obeseCutoff <<-
  matrix(c(20.1, 19.6, 19.3, 19.3, 19.8, 20.6, 21.6, 22.8, 24, 25.1, 26, 26.8, 27.6, 
           28.3, 28.9, 29.4, rep(30, 4)),
         c(20.1, 19.4, 19.1, 19.2, 19.7, 20.5, 21.6, 22.8, 24.1, 25.4, 26.7, 27.8, 
           28.6, 29.1, 29.4, 29.7, rep(30, 4)),
         ncol = 2)


children <- read.csv("base//synthBasefile_MhrswrkFixed_5000_New.csv") 

BMI <- children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])]

OW <- cbind(19+(1:21)*0.9, 34+(1:21)*0.9, 51+(1:21)*0.9, 18+(1:21)*0.8)
OB <- cbind(3+(1:21)*0.8, 6+(1:21)*1.2, 13+(1:21)*1.5, 3+(1:21)*0.7) 


propTable <- function(x) round(prop.table(table(x))[2], 3)*100


tempBMI <- BMI

#####################################################################################


funToOpt <-
  function(x, i,j){
    
    tempBMI[tempBMI$r1stchildethn==j, paste0("BMI", i)] <- 
      (tempBMI[tempBMI$r1stchildethn==j, paste0("BMI", i)] +x[1]) * x[2]
    
    z1OverweightBMILvl1 <- z1ObeseLvl1 <- tempBMI
    BMIcur <- z1OverweightBMILvl1[, i + 1]
    
    BMIboy <- BMIcur[children$z1gender==1]
    BMIgirl<- BMIcur[children$z1gender==0]
    
    z1OverweightBMILvl1[children$z1gender==1, i + 1] <- 
      ifelse(BMIboy >= overweightCutoff[i - 1, 1], 1, 0)
    
    z1ObeseLvl1[children$z1gender==1, i + 1] <- 
      ifelse(BMIboy >= obeseCutoff[i - 1, 1], 1, 0)
    
    z1OverweightBMILvl1[children$z1gender==0, i + 1] <- 
      ifelse(BMIgirl >= overweightCutoff[i - 1, 2], 1, 0)
    
    z1ObeseLvl1[children$z1gender==0, i + 1] <- 
      ifelse(BMIgirl >= obeseCutoff[i - 1, 2], 1, 0)
    
    currOW <- 
      t(apply(z1OverweightBMILvl1[,-c(1,2)], 2, function(x) 
        tapply(x, children$r1stchildethn,  propTable)))
    
    
    currOB <-
      t(apply(z1ObeseLvl1[,-c(1,2)], 2, function(x) 
        tapply(x, children$r1stchildethn,  propTable)))
  
    abs((currOW[i-1, j] - OW[i-1, j])) + abs((currOB[i-1, j] - OB[i-1, j]))
  }

for( i in 2:21) {
  cat("age:", i, "\n")
  for(j in 1:4){
    cat("Ethn:", j, "\n")
    
    par <- optim(c(0, 1), funToOpt, i = i, j = j)

    print(par$value)
    
    BMI[BMI$r1stchildethn==j, paste0("BMI", i)] <-
      (BMI[BMI$r1stchildethn==j, paste0("BMI", i)] + par$par[1])*  par$par[2]
  }
}


########################################################################################


z1OverweightBMILvl1 <- z1ObeseLvl1 <- BMI
    
for(i in 2:21){
  BMIcur <- z1OverweightBMILvl1[, i + 1]
  
  BMIboy <- BMIcur[children$z1gender==1]
  
  z1OverweightBMILvl1[children$z1gender==1, i + 1] <- 
    ifelse(BMIboy >= overweightCutoff[i-1, 1],1,0)
  
  z1ObeseLvl1[children$z1gender==1, i + 1] <- 
    ifelse(BMIboy >= obeseCutoff[i-1, 1],1,0)
  
  BMIgirl<- BMIcur[children$z1gender==0]
  
  z1OverweightBMILvl1[children$z1gender==0, i + 1] <- 
    ifelse(BMIgirl >= overweightCutoff[i-1, 2],1,0)
  
  z1ObeseLvl1[children$z1gender==0, i + 1] <- 
    ifelse(BMIgirl >= obeseCutoff[i-1, 2],1,0)
  
}

print(t(apply(z1OverweightBMILvl1[,-c(1,2)], 2, function(x) 
  tapply(x, children$r1stchildethn,  propTable))))
OW


print(t(apply(z1ObeseLvl1[,-c(1,2)], 2, function(x) 
  tapply(x, children$r1stchildethn,  propTable))))
OB

print(apply(z1OverweightBMILvl1[,-c(1,2)], 2, propTable))
print(apply(z1ObeseLvl1[,-c(1,2)], 2,propTable))




#Write back to basefile
 children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])] <- BMI 
 write.csv(children, file = "base//synthBasefile_MhrswrkFixed_5000_New1.csv")





#Euro
19+(1:21)*0.9
3+(1:21)*0.6


#Maori
34+(1:21)*0.9
6+(1:21)*0.9

#Pacific
51+(1:21)*0.9
13+(1:21)*1.3


#Asian
18+(1:21)*0.8
3+(1:21)*0.5














