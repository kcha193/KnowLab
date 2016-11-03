
library(memoise)

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

sdBMI <<-  matrix(c( 2.318561061,	2.207931975,
                     2.116476357,	2.066416278,
                     2.057399303,	2.1583464,
                     2.33275033,	2.496312829,
                     2.636535808,	2.725100429,
                     3.116483382,	3.141235808,
                     3.564080752,	3.635437662,
                     4.02828569,	4.222055377,
                     4.405528134,	4.741941784,
                     4.763114459,	5.269560519,
                     4.962362565,	5.514722333,
                     5.342412979,	5.929690456,
                     5.428023871,	6.013529644,
                     5.670758792,	6.221955633,
                     5.771324301,	6.284254227,
                     5.683160601,	6.219164444,
                     5.708957279,	6.37075478,
                     5.779369385,	6.584913605,
                     5.627971777,	6.502829121,
                     5.453944955,	6.38980461), ncol = 2, byrow = TRUE)


children <- read.csv("base//synthBasefile_MhrswrkFixed_5000_New.csv") 

BMI <- children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])]

OW <- cbind(21+(1:21)*0.9, 35+(1:21)*0.9, 55+(1:21)*0.9, 20+(1:21)*0.8)
OB <- cbind(3+(1:21)*0.6, 6+(1:21)*0.9,13+(1:21)*1.3, 2.5+(1:21)*0.5) 

propTable <- function(x) round(prop.table(table(x))[2], 3)*100

tempBMI <- BMI

#####################################################################################


funToOpt <-
  function(x, i,j){
    
    tempBMI[tempBMI$r1stchildethn==j, paste0("BMI", i)] <- 
      (tempBMI[tempBMI$r1stchildethn==j, paste0("BMI", i)] +x[1]) * x[2]
    
    z1OverweightBMILvl1 <- z1ObeseLvl1 <- tempBMI
    BMIcur <- z1OverweightBMILvl1[, i + 1]
    
    BMIboy <- sapply(BMIcur[children$z1gender==1], function(x) rnorm(1, mean = x , sd = sdBMI[i-1, 1]))
    BMIgirl<- sapply(BMIcur[children$z1gender==0], function(x) rnorm(1, mean = x, sd = sdBMI[i-1, 2]))
    
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
  
    mean(c(abs((currOW[i-1, j] - OW[i-1, j])), 2*abs((currOB[i-1, j] - OB[i-1, j]))))
  }

funToOpt <- memoise(funToOpt)

old <- proc.time()

for( i in 2:21) {
  cat("age:", i, "\n")
  for(j in 1:4){
    cat("Ethn:", j, "\n")
    
    par <- optim(c(-0.5, 1),funToOpt, i = i, j = j)

    print(par$value)
    
    BMI[BMI$r1stchildethn==j, paste0("BMI", i)] <-
      (BMI[BMI$r1stchildethn==j, paste0("BMI", i)] + par$par[1])*  par$par[2]
  }
}

print(proc.time()-old)


########################################################################################


# z1OverweightBMILvl1 <- z1ObeseLvl1 <- BMI
# 
# for(i in 2:21){
#   BMIcur <- z1OverweightBMILvl1[, i + 1]
# 
#   BMIboy <- sapply(BMIcur[children$z1gender==1], function(x) rnorm(1, mean = x , sd = sdBMI[i-1, 1]))
#    
#   z1OverweightBMILvl1[children$z1gender==1, i + 1] <-
#     ifelse(BMIboy >= overweightCutoff[i-1, 1],1,0)
# 
#   z1ObeseLvl1[children$z1gender==1, i + 1] <-
#     ifelse(BMIboy >= obeseCutoff[i-1, 1],1,0)
# 
#   BMIgirl<- sapply(BMIcur[children$z1gender==0], function(x) rnorm(1, mean = x, sd = sdBMI[i-1, 2]))
#   
#   z1OverweightBMILvl1[children$z1gender==0, i + 1] <-
#     ifelse(BMIgirl >= overweightCutoff[i-1, 2],1,0)
# 
#   z1ObeseLvl1[children$z1gender==0, i + 1] <-
#     ifelse(BMIgirl >= obeseCutoff[i-1, 2],1,0)
# 
# }
# 
# print(t(apply(z1OverweightBMILvl1[,-c(1,2)], 2, function(x)
#   tapply(x, children$r1stchildethn,  propTable))))
# OW
# 
# 
# print(t(apply(z1ObeseLvl1[,-c(1,2)], 2, function(x)
#   tapply(x, children$r1stchildethn,  propTable))))
# OB
# 
# print(apply(z1OverweightBMILvl1[,-c(1,2)], 2, propTable))
# print(apply(z1ObeseLvl1[,-c(1,2)], 2,propTable))


#Write back to basefile
 children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])] <- BMI 
 write.csv(children, file = "base//synthBasefile_MhrswrkFixed_5000_New1.csv")




 # children <- read.csv("base//synthBasefile_MhrswrkFixed_5000_New1.csv") 
 # 
 # BMI <- children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])]
 # 
 # BMI <- BMI + 1
 # 
 # 
 # #Write back to basefile
 # children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])] <- BMI 
 # write.csv(children, file = "base//synthBasefile_MhrswrkFixed_5000_New1.csv")
 






