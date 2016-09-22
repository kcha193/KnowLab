
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
    c(c(18.4, 17.9, 17.6, 17.4, 17.6, 17.9, 18.4, 19.1, 19.8, 20.6, 21.2, 21.9, 22.6, 23.3, 23.9, 24.5, rep(25, 4)),
      c(18, 17.6, 17.3, 17.1, 17.3, 17.8, 18.3, 19.1, 19.9, 20.7, 21.7, 22.6, 23.3, 23.9, 24.4, 24.7, rep(25, 4))),
    ncol = 2)

obeseCutoff <<-
  matrix(c(20.1, 19.6, 19.3, 19.3, 19.8, 20.6, 21.6, 22.8, 24, 25.1, 26, 26.8, 27.6, 28.3, 28.9, 29.4, rep(30, 4)),
         c(20.1, 19.4, 19.1, 19.2, 19.7, 20.5, 21.6, 22.8, 24.1, 25.4, 26.7, 27.8, 28.6, 29.1, 29.4, 29.7, rep(30, 4)),
         ncol = 2)


children <- read.csv("base//synthBasefile_MhrswrkFixed_5000_New.csv") 

BMI <- children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])]

#Reduce Asian BMI Age 3 to 21


BMI[BMI$r1stchildethn==4, paste0("BMI", 3)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 3)] * 0.966
BMI[BMI$r1stchildethn==4, paste0("BMI", 4)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 4)] * 0.973
BMI[BMI$r1stchildethn==4, paste0("BMI", 5)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 5)] * 0.963
BMI[BMI$r1stchildethn==4, paste0("BMI", 6)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 6)] * 0.968

BMI[BMI$r1stchildethn==4, paste0("BMI", 7)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 7)] * 0.959
BMI[BMI$r1stchildethn==4, paste0("BMI", 8)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 8)] * 0.95
BMI[BMI$r1stchildethn==4, paste0("BMI", 9)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 9)] * 0.938
BMI[BMI$r1stchildethn==4, paste0("BMI", 10)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 10)] * 0.938


BMI[BMI$r1stchildethn==4, paste0("BMI", 11)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 11)] * 0.94
BMI[BMI$r1stchildethn==4, paste0("BMI", 12)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 12)] * 0.94
BMI[BMI$r1stchildethn==4, paste0("BMI", 13)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 13)] * 0.94
BMI[BMI$r1stchildethn==4, paste0("BMI", 14)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 14)] * 0.938

BMI[BMI$r1stchildethn==4, paste0("BMI", 15)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 15)] * 0.933
BMI[BMI$r1stchildethn==4, paste0("BMI", 16)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 16)] * 0.92
BMI[BMI$r1stchildethn==4, paste0("BMI", 17)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 17)] * 0.92
BMI[BMI$r1stchildethn==4, paste0("BMI", 18)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 18)] * 0.915
BMI[BMI$r1stchildethn==4, paste0("BMI", 19)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 19)] * 0.915
BMI[BMI$r1stchildethn==4, paste0("BMI", 20)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 20)] * 0.915
BMI[BMI$r1stchildethn==4, paste0("BMI", 21)] <- BMI[BMI$r1stchildethn==4, paste0("BMI", 21)] * 0.905


#Increase Maori BMI Age 2 to 11
BMI[BMI$r1stchildethn==1, paste0("BMI", 2)] <- BMI[BMI$r1stchildethn==1, paste0("BMI", 2)] * 1.02
BMI[BMI$r1stchildethn==1, paste0("BMI", 3)] <- BMI[BMI$r1stchildethn==1, paste0("BMI", 3)] * .99
BMI[BMI$r1stchildethn==1, paste0("BMI", 4)] <- BMI[BMI$r1stchildethn==1, paste0("BMI", 4)] * .994
BMI[BMI$r1stchildethn==1, paste0("BMI", 5)] <- BMI[BMI$r1stchildethn==1, paste0("BMI", 5)] * .994
BMI[BMI$r1stchildethn==1, paste0("BMI", 6)] <- BMI[BMI$r1stchildethn==1, paste0("BMI", 6)] * 1.0013


#Increase Maori BMI Age 2 to 11
BMI[BMI$r1stchildethn==2, paste0("BMI", 2)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 2)] * 1.05
BMI[BMI$r1stchildethn==2, paste0("BMI", 3)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 3)] * 1.012
BMI[BMI$r1stchildethn==2, paste0("BMI", 4)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 4)] * 1.015
BMI[BMI$r1stchildethn==2, paste0("BMI", 5)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 5)] * 1.025
BMI[BMI$r1stchildethn==2, paste0("BMI", 6)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 6)] * 1.02

BMI[BMI$r1stchildethn==2, paste0("BMI", 7)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 7)] * 1.015
BMI[BMI$r1stchildethn==2, paste0("BMI", 8)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 8)] * 1.012
BMI[BMI$r1stchildethn==2, paste0("BMI", 9)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 9)] * 1.014
BMI[BMI$r1stchildethn==2, paste0("BMI", 10)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 10)] * 1.015

BMI[BMI$r1stchildethn==2, paste0("BMI", 11)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 11)] * 1.01


BMI[BMI$r1stchildethn==2, paste0("BMI", 14)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 14)] * 0.98
BMI[BMI$r1stchildethn==2, paste0("BMI", 15)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 15)] * 0.98
BMI[BMI$r1stchildethn==2, paste0("BMI", 16)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 16)] * 0.98
BMI[BMI$r1stchildethn==2, paste0("BMI", 17)] <- BMI[BMI$r1stchildethn==2, paste0("BMI", 17)] * 0.99

#Increase PAcific BMI Age 2 to 11
BMI[BMI$r1stchildethn==3, paste0("BMI", 2)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 2)] * 1.08
BMI[BMI$r1stchildethn==3, paste0("BMI", 3)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 3)] * 1.03
BMI[BMI$r1stchildethn==3, paste0("BMI", 4)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 4)] * 1.015
BMI[BMI$r1stchildethn==3, paste0("BMI", 5)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 5)] * 1.018
BMI[BMI$r1stchildethn==3, paste0("BMI", 6)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 6)] * 1.02
BMI[BMI$r1stchildethn==3, paste0("BMI", 7)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 7)] * 1.01

BMI[BMI$r1stchildethn==3, paste0("BMI", 9)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 9)] * 0.98
BMI[BMI$r1stchildethn==3, paste0("BMI", 10)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 10)] * 0.97
BMI[BMI$r1stchildethn==3, paste0("BMI", 11)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 11)] * 0.96
BMI[BMI$r1stchildethn==3, paste0("BMI", 12)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 12)] * 0.95
BMI[BMI$r1stchildethn==3, paste0("BMI", 13)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 13)] * 0.94
BMI[BMI$r1stchildethn==3, paste0("BMI", 14)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 14)] * 0.934
BMI[BMI$r1stchildethn==3, paste0("BMI", 15)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 15)] * 0.938
BMI[BMI$r1stchildethn==3, paste0("BMI", 16)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 16)] * 0.955
BMI[BMI$r1stchildethn==3, paste0("BMI", 17)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 17)] * 0.955
BMI[BMI$r1stchildethn==3, paste0("BMI", 18)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 18)] * 0.97
BMI[BMI$r1stchildethn==3, paste0("BMI", 19)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 19)] * 0.987
BMI[BMI$r1stchildethn==3, paste0("BMI", 20)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 20)] * 0.99
BMI[BMI$r1stchildethn==3, paste0("BMI", 21)] <- BMI[BMI$r1stchildethn==3, paste0("BMI", 21)] * 0.99



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
    
propTable <- function(x) round(prop.table(table(x))[2], 3)


print(t(apply(z1OverweightBMILvl1[,-c(1,2)], 2, function(x) tapply(x, children$r1stchildethn,  propTable))))
print(t(apply(z1ObeseLvl1[,-c(1,2)], 2, function(x) tapply(x, children$r1stchildethn,  propTable))))

#Write back to basefile
# children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])] <- BMI 
# write.csv(children, file = "base//synthBasefile_MhrswrkFixed_5000_New1.csv")


