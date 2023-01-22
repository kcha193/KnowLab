
























#gamma girl 


overweightCutoff <<-
  matrix(
    c(c(18.4, 17.9, 17.6, 17.4, 17.6, 17.9, 18.4, 19.1, 19.8, 20.6, 21.2, 21.9, 22.6, 23.3, 23.9, 24.5, rep(25, 4)),
      c(18, 17.6, 17.3, 17.1, 17.3, 17.8, 18.3, 19.1, 19.9, 20.7, 21.7, 22.6, 23.3, 23.9, 24.4, 24.7, rep(25, 4))),
    ncol = 2)

obeseCutoff <<-
  matrix(c(20.1, 19.6, 19.3, 19.3, 19.8, 20.6, 21.6, 22.8, 24, 25.1, 26, 26.8, 27.6, 28.3, 28.9, 29.4, rep(30, 4)),
         c(20.1, 19.4, 19.1, 19.2, 19.7, 20.5, 21.6, 22.8, 24.1, 25.4, 26.7, 27.8, 28.6, 29.1, 29.4, 29.7, rep(30, 4)),
         ncol = 2)

gammaParGirl <-  matrix(c(54.38538539,	0.30481481,
                    63.15015015,	0.26099099,
                    58.36936937,	0.27891892,
                    40.83983984,	0.39246246,
                    32.67267267,	0.49604605,
                    28.09109109,	0.59365365,
                    20.12312312,	0.84862863,
                    17.73273273,	1.00400400,
                    15.14314314,	1.22312312,
                    13.35035035,	1.44423423,
                    12.95195195,	1.56574575,
                    12.75275275,	1.66534535,
                    12.55355355,	1.75498498,
                    13.35035035,	1.70717718,
                    14.54554555,	1.61355355,
                    14.54554555,	1.64741742,
                    14.74474474,	1.66136136,
                    14.94394394,	1.65538539,
                    15.14314314,	1.64940941,
                    15.54154154,	1.62351351), ncol = 2, byrow = TRUE)


gammaParBoy <-  matrix(c(69.70000,	0.25200,
                          63.20000,	0.26700,
                          60.30000,	0.27400,
                          47.60000,	0.33900,
                          37.50000,	0.43000,
                          26.90000,	0.60200,
                          21.10000,	0.78400,
                          18.20000,	0.94700,
                          15.90000,	1.13000,
                          15.40000,	1.22000,
                          14.50000,	1.35100,
                          14.80000,	1.39200,
                          14.80000,	1.44900,
                          15.65000,	1.43700,
                          16.50000,	1.41000,
                          18.10000,	1.32700,
                          18.70000,	1.32200,
                          19.30000,	1.28500,
                          20.20000,	1.23500,
                          21.20000,	1.18700), ncol = 2, byrow = TRUE)





children <- read.csv("base//synthBasefile_MhrswrkFixed_5000_New.csv") 

fun <-
  function(age){
    z1OverweightBMILvl1 <- z1ObeseLvl1 <- rep(NA, 5000)
    
    BMIboy <- rgamma(sum(children$z1gender==1), 
                     shape = gammaParBoy[age - 1, 1], 
                     scale = gammaParBoy[age - 1, 2])
    
    z1OverweightBMILvl1[children$z1gender==1] <- 
      ifelse(BMIboy >= overweightCutoff[ age - 1, 1],1,0)
    
    z1ObeseLvl1[children$z1gender==1] <- 
      ifelse(BMIboy >= obeseCutoff[ age - 1, 1],1,0)
    
    BMIgirl<- rgamma(sum(children$z1gender==0), 
                     shape = gammaParGirl[age - 1, 1], 
                     scale = gammaParGirl[age - 1, 2])
    
    z1OverweightBMILvl1[children$z1gender==0] <- 
      ifelse(BMIgirl >= overweightCutoff[ age - 1, 2],1,0)
    
    z1ObeseLvl1[children$z1gender==0] <- 
      ifelse(BMIgirl >= obeseCutoff[ age - 1, 2],1,0)
    
    propTable <- function(x) prop.table(table(x))[2]
    
    
    OverweightNZHS = predict(lm(OverweightObese ~ I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
    ObeseNZHS = predict(lm(Obese  ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
    
    c(OverweightNZHS[age-1], propTable(z1OverweightBMILvl1)*100, ObeseNZHS[age-1], propTable(z1ObeseLvl1) * 100)
  }

temp <- t(sapply(2:21, fun))
temp

meanGirl <- gammaParGirl[,1]*gammaParGirl[,2]
varGirl <- gammaParGirl[,1]*gammaParGirl[,2]^2

meanBoy <- gammaParBoy[,1]*gammaParBoy[,2]
varBoy <- gammaParBoy[,1]*gammaParBoy[,2]^2

cbind(meanBoy, meanGirl)
#####################################################################################################


                        #Maori Pac Asian
ethnDiffGirl <- matrix(c(c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9),
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9)), ncol = 3, byrow = TRUE)

#Maori Pac Asian
ethnDiffBoy <- matrix(c( c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9),
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9), 
                         c(0.9, 1.6, 0.9)), ncol = 3, byrow = TRUE)
################################################################################################################


age = 2

z1OverweightBMILvl1 <- z1ObeseLvl1 <- rep(NA, 5000)

children$z1gender1 <- ifelse(children$z1gender == 1, 1, 0)
children$r1stchildethn2 <- ifelse(children$r1stchildethn == 2, 1, 0)
children$r1stchildethn3 <- ifelse(children$r1stchildethn == 3, 1, 0)
children$r1stchildethn4 <- ifelse(children$r1stchildethn == 4, 1, 0)




inter <- meanGirl[age - 1] - (ethnDiffGirl[age - 1,1] * mean(children$r1stchildethn2[children$z1gender==0]) + 
                                ethnDiffGirl[age - 1,2] * mean(children$r1stchildethn3[children$z1gender==0]) - 
                                ethnDiffGirl[age - 1,3] * mean(children$r1stchildethn4[children$z1gender==0]))
inter
eMean <-  inter +  (    ethnDiffGirl[age - 1,1] * (children$r1stchildethn2[children$z1gender==0]) + 
                       ethnDiffGirl[age - 1,2] * (children$r1stchildethn3[children$z1gender==0]) - 
                       ethnDiffGirl[age - 1,3] * (children$r1stchildethn4[children$z1gender==0]))

BMIgirl <- sapply(eMean, function(x) rgamma(1, shape = x^2/varGirl[age - 1], 
                                            scale =  varGirl[age - 1]/x))



z1OverweightBMILvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= overweightCutoff[ age - 1, 2],1,0)

z1ObeseLvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= obeseCutoff[ age - 1, 2],1,0)



inter <- meanBoy[age - 1] - (ethnDiffBoy[age - 1,1] * mean(children$r1stchildethn2[children$z1gender==1]) + 
                               ethnDiffBoy[age - 1,2] * mean(children$r1stchildethn3[children$z1gender==1]) - 
                               ethnDiffBoy[age - 1,3] * mean(children$r1stchildethn4[children$z1gender==1]))
inter 
eMean <-  inter +  (    ethnDiffBoy[age - 1,1] * (children$r1stchildethn2[children$z1gender==1]) + 
                       ethnDiffBoy[age - 1,2] * (children$r1stchildethn3[children$z1gender==1]) - 
                       ethnDiffBoy[age - 1,3] * (children$r1stchildethn4[children$z1gender==1]))


BMIboy <- sapply(eMean, function(x) rgamma(1, shape = x^2/varBoy[age - 1], 
                                           scale = varBoy[age - 1]/x))

z1OverweightBMILvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= overweightCutoff[ age - 1, 1],1,0)

z1ObeseLvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= obeseCutoff[ age - 1, 1],1,0)


mean(c(BMIboy, BMIgirl))

mean(BMIboy)
mean(BMIgirl)

OverweightNZHS = predict(lm(OverweightObese ~ I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
ObeseNZHS = predict(lm(Obese  ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))

c(OverweightNZHS[age-1], propTable(z1OverweightBMILvl1)*100, ObeseNZHS[age-1], propTable(z1ObeseLvl1) * 100)

tapply(z1OverweightBMILvl1, children$z1gender, propTable)
tapply(z1ObeseLvl1, children$z1gender, propTable)


tapply(z1OverweightBMILvl1, children$r1stchildethn, propTable)
tapply(z1ObeseLvl1, children$r1stchildethn, propTable)



#           Var Year Female Male
# 1  Overweight    2   27.7 30.0
# 2  Overweight    3   28.1 30.4
# 3  Overweight    4   28.4 30.2
# 4  Overweight    5   29.1 31.1
# 5  Overweight    6   29.2 33.0
# 6  Overweight    7   29.5 32.5
# 7  Overweight    8   30.3 32.7
# 8  Overweight    9   30.9 33.2
# 9  Overweight   10   31.4 34.1
# 10 Overweight   11   32.7 35.1
# 11 Overweight   12   33.5 35.9
# 12 Overweight   13   34.6 36.8
# 13 Overweight   14   36.3 36.7
# 14 Overweight   15   37.3 38.3
# 15 Overweight   16   38.5 39.0
# 16 Overweight   17   39.9 41.1
# 17 Overweight   18   41.3 42.0
# 18 Overweight   19   43.4 43.1
# 19 Overweight   20   44.6 44.9
# 20 Overweight   21   46.5 46.5


#           Var Year Asian Maori NZ Euro Pacific
# 1  Overweight    2  21.2  35.2    24.3    47.6
# 2  Overweight    3  19.6  37.6    24.1    47.9
# 3  Overweight    4  20.1  37.9    24.0    48.0
# 4  Overweight    5  21.0  40.1    24.0    50.3
# 5  Overweight    6  21.1  41.7    24.7    51.5
# 6  Overweight    7  21.7  41.4    24.8    50.1
# 7  Overweight    8  22.0  42.0    25.1    52.6
# 8  Overweight    9  23.9  42.9    25.1    54.0
# 9  Overweight   10  23.5  43.3    26.2    54.2
# 10 Overweight   11  23.3  45.4    27.1    54.6
# 11 Overweight   12  23.7  46.5    27.9    55.8
# 12 Overweight   13  27.0  46.0    29.6    54.2
# 13 Overweight   14  26.5  47.0    30.4    55.7
# 14 Overweight   15  28.4  46.5    32.4    57.1
# 15 Overweight   16  29.2  47.1    33.5    58.2
# 16 Overweight   17  29.6  49.1    35.4    60.2
# 17 Overweight   18  31.2  50.8    36.2    60.8
# 18 Overweight   19  30.4  52.2    38.5    60.7
# 19 Overweight   20  31.2  54.4    39.9    62.2
# 20 Overweight   21  33.3  55.5    41.8    64.5















OverweightBMI = z1OverweightBMILvl1 %>% select(-r1stchildethn, -z1gender) %>% summarise_all(funs(propTable)) %>% as.numeric()
ObeseBMI = z1ObeseLvl1 %>% select(-r1stchildethn, -z1gender) %>% summarise_all(funs(propTable)) %>% as.numeric()

abs(OverweightBMI[age-1]*100 - OverweightNZHS[age-1] ) +  abs(ObeseBMI[age-1]*100  - ObeseNZHS[age-1] )



fun <-
  function(age){
    z1OverweightBMILvl1 <- z1ObeseLvl1 <- rep(NA, 5000)
    
    BMIboy <- rgamma(sum(children$z1gender==1), 
                     shape = gammaParBoy[age - 1, 1], 
                     scale = gammaParBoy[age - 1, 2])
    
    z1OverweightBMILvl1[children$z1gender==1] <- 
      ifelse(BMIboy >= overweightCutoff[ age - 1, 1],1,0)
    
    z1ObeseLvl1[children$z1gender==1] <- 
      ifelse(BMIboy >= obeseCutoff[ age - 1, 1],1,0)
    
    BMIgirl<- rgamma(sum(children$z1gender==0), 
                     shape = gammaParGirl[age - 1, 1], 
                     scale = gammaParGirl[age - 1, 2])
    
    z1OverweightBMILvl1[children$z1gender==0] <- 
      ifelse(BMIgirl >= overweightCutoff[ age - 1, 2],1,0)
    
    z1ObeseLvl1[children$z1gender==0] <- 
      ifelse(BMIgirl >= obeseCutoff[ age - 1, 2],1,0)
    
    propTable <- function(x) prop.table(table(x))[2]
    
    
    OverweightNZHS = predict(lm(OverweightObese ~ I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
    ObeseNZHS = predict(lm(Obese  ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
    
    c(OverweightNZHS[age-1], propTable(z1OverweightBMILvl1)*100, ObeseNZHS[age-1], propTable(z1ObeseLvl1) * 100)
  }

temp <- t(sapply(2:21, fun))
temp


#Write back to basefile
children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])] <- BMI 
write.csv(children, file = "base//synthBasefile_MhrswrkFixed_5000_New1.csv")



pnorm(20,  17.929171, 0.20, lower.tail = FALSE)


fun <- 
  function(x)
    pnorm(overweightCutoff[iteration-1, 1],  17.929171, x, lower.tail = FALSE) - 30.05354/100

uniroot(fun, lower = 0, upper = 2,   tol = 0.0001)


fun <- 
  function(x)
    pnorm(obeseCutoff[iteration-1, 1],  17.929171,  x, lower.tail = FALSE) - 8.202138/100

uniroot(fun, lower = 0, upper = 2,   tol = 0.0001)


plot( seq(10, 25, length = 100) , dnorm(seq(10, 25, length = 100), 17.929171,  1.1 ), type = "l")


pnorm(overweightCutoff[iteration-1, 1], 17.929171,  1.1 , lower.tail = FALSE)
pnorm(obeseCutoff[iteration-1, 1], 17.929171,  1.1 ,   lower.tail = FALSE)


pnorm(overweightCutoff[iteration-1, 1], 24,  1.1 ,   lower.tail = FALSE)


fun <- function(x) 
  pnorm(overweightCutoff[iteration-1, 1], x,  1.1 ,   lower.tail = FALSE) - 0.3

uniroot(fun, lower = 0, upper = overweightCutoff[iteration-1, 1],   tol = 0.0001)




fun <- 
  function(x)
    abs(abs(pnorm(log(overweightCutoff[iteration-1, 1]),  
                  mean = log(17.929171), sd = x, 
                  lower.tail = FALSE) - 
              pnorm(log(obeseCutoff[iteration-1, 1]), 
                    mean = log(17.929171), sd = x, 
                    lower.tail = FALSE)) - 
          (30.05354 - 8.202138)/100)

optimize( fun, lower = 0, upper = 100)

fun(0.1362605)

pnorm(log(overweightCutoff[iteration-1, 1]),  log(17.929171), 0.1362605, lower.tail = FALSE)
pnorm(log(obeseCutoff[iteration-1, 1]),  log(17.929171), 0.1362605, lower.tail = FALSE)




pnorm(log(25),  log(25), 5.67, lower.tail = FALSE)

pnorm(log(30),  log(25), 5.67, lower.tail = FALSE)





fun <- 
  function(x)
    abs( (pnorm(log(overweightCutoff[iteration-1, 1]),  
                mean = log(17.929171), sd = x, 
                lower.tail = FALSE) - 30.05354/100)) + 
  abs((pnorm(log(obeseCutoff[iteration-1, 1]), 
             mean = log(17.929171), sd = x, 
             lower.tail = FALSE) - 8.202138/100))

optimize( fun, lower = 0, upper = 100000)

fun(0.04541205)



pnorm(log(overweightCutoff[iteration-1, 1]),  
      mean = log(17.929171), sd = 0.04541205, 
      lower.tail = FALSE)

pnorm(log(obeseCutoff[iteration-1, 1]), 
      mean = log(17.929171), sd = 0.04541205, 
      lower.tail = FALSE)


fun <- 
  function(x)
    abs( (pnorm(log(overweightCutoff[iteration-1, 1], x),  
                mean = log(17.929171, x), sd = 0.04541205, 
                lower.tail = FALSE) - 30.05354/100)) + 
  abs((pnorm(log(obeseCutoff[iteration-1, 1], x), 
             mean = log(17.929171, x), sd = 0.04541205, 
             lower.tail = FALSE) - 8.202138/100))

optimize( fun, lower = 0, upper = 10)








fun <- 
  function(x)
    abs(pnorm(log(overweightCutoff[iteration-1, 1], x[1]),  
              mean = log(17.929171, x[1]), sd = x[2], 
              lower.tail = FALSE) - 30.05354/100) +
  abs(pnorm(log(obeseCutoff[iteration-1, 1], x[1]), 
            mean = log(17.929171, x[1]), sd = x[2], 
            lower.tail = FALSE) - 8.202138/100)*1.01


optim(c(2, 0.1), fun, method = "SANN", control = list(maxit = 100000, trace = TRUE, temp = 1))


fun(c(4.60392777, 0.05131472))


pnorm(log(overweightCutoff[iteration-1, 1], 1.3511183),  log(17.929171, 1.3511183), 0.1505342, lower.tail = FALSE)
pnorm(log(obeseCutoff[iteration-1, 1], 1.3511183),  log(17.929171, 1.3511183), 0.1505342, lower.tail = FALSE)





fun <- 
  function(x)
    abs( (pnorm(log(overweightCutoff[iteration-1, 1]),  
                mean = log(17.929171), sd = x, 
                lower.tail = FALSE) - 30.05354/100)) + 
  abs((pnorm(log(obeseCutoff[iteration-1, 1]), 
             mean = log(17.929171), sd = x, 
             lower.tail = FALSE) - 8.202138/100))

optimize( fun, lower = 0, upper = 100000)

fun <- 
  function(x)
    abs(pnbinom((overweightCutoff[iteration-1, 1]),  
                mu = (17.929171), size = x, 
                lower.tail = FALSE) - 30.05354/100) +
  abs(pnbinom((obeseCutoff[iteration-1, 1]), 
              mu = (17.929171), size = x, 
              lower.tail = FALSE) - 8.202138/100)

fun(10)

optimize( fun, lower = 0, upper = 10000)

optim(c(2), fun, method = "SANN", control = list(maxit = 10000000, trace = TRUE, temp = 10))



pnbinom((overweightCutoff[iteration-1, 1]),  
        mu = (17.929171), size =0.4443289, 
        lower.tail = FALSE)
pnbinom((obeseCutoff[iteration-1, 1]), 
        mu = (17.929171), size = 0.4443289, 
        lower.tail = FALSE)


fun <- 
  function(x)
    pnorm(log(obeseCutoff[iteration-1, 1]),  log(17.929171),  x, lower.tail = FALSE) - 8.202138/100

uniroot(fun, lower = 0, upper = 2,   tol = 0.0001)



fun <- 
  function(x)
    pnorm(log(overweightCutoff[iteration-1, 1]),  log(17.929171), x, lower.tail = FALSE) - 30.05354/100

uniroot(fun, lower = 0, upper = 2,   tol = 0.0001)


fun <- 
  function(x)
    pnorm(log(obeseCutoff[iteration-1, 1]),  log(17.929171),  x, lower.tail = FALSE) - 8.202138/100

uniroot(fun, lower = 0, upper = 2,   tol = 0.0001)





plot( seq(10, 25, length = 100) , dnbinom(seq(10, 25, length = 100), mu =17.929171, size = 0.0292805 ), type = "l")


pnorm(obeseCutoff[iteration-1, 1],  17.929171,  0.8239838, lower.tail = FALSE)


NZHS2014 <- read.csv("scrap/NZHS2014.csv")

predict(lm(OverweightObese ~ I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(OverweightBoys ~ I(Age) + I(Age^2) , NZHS2014[1:7,]), data.frame(Age = 2:21))

predict(lm(OverweightGirls ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))

predict(lm(Obese  ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(ObeseBoys  ~ I(Age) + I(Age^2) , NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(ObeseGirls ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))



sdBMI <-  matrix(c( 2.318561061,	2.207931975,
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









iteration = 2

apply(children[,names(children)[grep("^BMI", names(children))]],2,sd)


BMI <- children[,paste0("BMI", iteration)]
sd(BMI)

tapply( BMI, children$z1gender, sd)


z1OverweightBMILvl1 <- numeric(5000) 
z1ObeseLvl1 <- numeric(5000) 

BMIboy <- BMI[children$z1gender==1]

sd(BMIboy)
BMIboy <- mean(BMIboy) + scale(BMIboy) * 1.55
sd(BMIboy)

z1OverweightBMILvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= overweightCutoff[iteration-1, 1],1,0)

prop.table(table(z1OverweightBMILvl1[children$z1gender==1]))

z1ObeseLvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= obeseCutoff[iteration-1, 1],1,0)

prop.table(table(z1ObeseLvl1[children$z1gender==1]))


BMIgirl<- BMI[children$z1gender==0]

sd(BMIgirl)
BMIgirl <- mean(BMIgirl) + scale(BMIgirl)* 2
sd(BMIgirl)


z1OverweightBMILvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= overweightCutoff[iteration-1, 2],1,0)

prop.table(table(z1OverweightBMILvl1[children$z1gender==0]))

z1ObeseLvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= obeseCutoff[iteration-1, 2],1,0)

prop.table(table(z1ObeseLvl1[children$z1gender==0]))


prop.table(table(z1OverweightBMILvl1))
prop.table(table(z1ObeseLvl1))



#Fix the BMI distribution

NZHS2014 <- read.csv("scrap/NZHS2014.csv")

predict(lm(OverweightObese ~ I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(OverweightBoys ~ I(Age) + I(Age^2) , NZHS2014[1:7,]), data.frame(Age = 2:21))

predict(lm(OverweightGirls ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))

predict(lm(Obese  ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(ObeseBoys  ~ I(Age) + I(Age^2) , NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(ObeseGirls ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))


basefiledir <- paste(getwd() ,"/base/",sep="")
children <- read.csv(paste0(basefiledir, "synthBasefile_MhrswrkFixed_5000_New.csv") )

head(children)


children %>% group_by(z1gender) %>% summarise(mean(BMI11))

children %>% group_by(z1gender) %>% summarise(sd(BMI11))

children %>% group_by(r1stchildethn) %>% summarise(mean(BMI11))



overweightCutoff <<-
  matrix(c( 18.36,	18.09,
            17.85,	17.64,
            17.52,	17.35,
            17.39,	17.23,
            17.52,	17.33,
            17.88,	17.69,
            18.41,	18.28,
            19.07,	18.99,
            19.8,	  19.78,
            20.51,	20.66,
            21.2,	  21.59,
            21.89,	22.49,
            22.6,	  23.27,
            23.28,	23.89,
            23.89,	24.34,
            24.46,	24.7,
            25,	    25,
            25,	    25,
            25,	    25,
            25,	    25), ncol = 2, byrow = TRUE)

obeseCutoff <<-
  matrix(c( 19.99,	19.81,
            19.5,	  19.38,
            19.23,	19.16,
            19.27,	19.2,
            19.76,	19.61,
            20.59,	20.39,
            21.56,	21.44,
            22.71,	22.66,
            23.96,	23.97,
            25.07,	25.25,
            26.02,	26.47,
            26.87,	27.57,
            27.64,	28.42,
            28.32,	29.01,
            28.89,	29.4,
            29.43,	29.7,
            30,	    30,
            30,	    30,
            30,	    30,
            30,	    30), ncol = 2, byrow = TRUE)

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

overweightCutoff <<-
  matrix(
    c(c(18.4, 17.9, 17.6, 17.4, 17.6, 17.9, 18.4, 19.1, 19.8, 20.6, 21.2, 21.9, 22.6, 23.3, 23.9, 24.5, rep(25, 4)),
      c(18, 17.6, 17.3, 17.1, 17.3, 17.8, 18.3, 19.1, 19.9, 20.7, 21.7, 22.6, 23.3, 23.9, 24.4, 24.7, rep(25, 4))),
    ncol = 2)

obeseCutoff <<-
  matrix(c(20.1, 19.6, 19.3, 19.3, 19.8, 20.6, 21.6, 22.8, 24, 25.1, 26, 26.8, 27.6, 28.3, 28.9, 29.4, rep(30, 4)),
         c(20.1, 19.4, 19.1, 19.2, 19.7, 20.5, 21.6, 22.8, 24.1, 25.4, 26.7, 27.8, 28.6, 29.1, 29.4, 29.7, rep(30, 4)),
         ncol = 2)



#Fix the BMI distribution

NZHS2014 <- read.csv("scrap/NZHS2014.csv")

predict(lm(OverweightObese ~ I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(OverweightBoys ~ I(Age) + I(Age^2) , NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(OverweightGirls ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))

predict(lm(Obese  ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(ObeseBoys  ~ I(Age) + I(Age^2) , NZHS2014[1:7,]), data.frame(Age = 2:21))
predict(lm(ObeseGirls ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))



fun <- 
  function(init, ave, age){
    
    #BMI <- rnorm(5000, par[1], par[2])
    
    shape = ave^2/init
    scale = init/ave
    
    BMI <- rgamma(5000, shape = shape, scale = scale)
    
    z1OverweightBMILvl1 <- z1ObeseLvl1 <- rep(NA, 5000)
    
    BMIboy <- BMI[children$z1gender==1]
    z1OverweightBMILvl1[children$z1gender==1] <- 
      ifelse(BMIboy >= overweightCutoff[ age - 1, 1],1,0)
    
    z1ObeseLvl1[children$z1gender==1] <- 
      ifelse(BMIboy >= obeseCutoff[ age - 1, 1],1,0)
    
    BMIgirl<- BMI[children$z1gender==0]
    
    z1OverweightBMILvl1[children$z1gender==0] <- 
      ifelse(BMIgirl >= overweightCutoff[ age - 1, 2],1,0)
    
    z1ObeseLvl1[children$z1gender==0] <- 
      ifelse(BMIgirl >= obeseCutoff[ age - 1, 2],1,0)
    
    propTable <- function(x) prop.table(table(x))[2]
    
    OverweightNZHS = predict(lm(OverweightObese ~ I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
    ObeseNZHS = predict(lm(Obese  ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
    
    abs(prop.table(table(z1OverweightBMILvl1))[2]*100 - OverweightNZHS[age-1] ) +  
      abs(prop.table(table(z1ObeseLvl1))[2]*100  - ObeseNZHS[age-1] )
  }


uniroot(fun, interval = c(0.4,1), ave = 17.7, age = 2)

fun(0.3, 17.7, 2)

fun(c(55,.5), age = 2)

age2 = optim(0.5, fun, ave = 17.7, age = 2, method = "Brent", lower = 0, upper = 5)$par

fun(age2, 17.7, 2)



age3 =optim(c(1,1), fun, age = 3)$par
age4 =optim(c(1,1), fun, age = 4)$par
age5 =optim(c(1,1), fun, age = 5)$par
age6 =optim(c(1,1), fun, age = 6)$par
age7 =optim(c(1,1), fun, age = 7)$par
age8 =optim(c(0.9,1.1), fun, age = 8)$par
age9 =optim(c(1,1), fun, age = 9)$par
age10 =optim(c(1,1), fun, age = 10)$par
age11 =optim(c(1,1), fun, age = 11)$par
age12 =optim(c(1,1), fun, age = 12)$par
age13 =optim(c(1,1), fun, age = 13)$par
age14 =optim(c(1,1), fun, age = 14)$par
age15 =optim(c(1,1), fun, age = 15)$par
age16 =optim(c(1,1), fun, age = 16)$par
age17 =optim(c(1,1), fun, age = 17)$par
age18 =optim(c(1,1), fun, age = 18)$par
age19 =optim(c(1,1), fun, age = 19)$par
age20 =optim(c(1,1), fun, age = 20)$par
age21 =optim(c(1,1), fun, age = 21)$par



children <- read.csv("base//synthBasefile_MhrswrkFixed_5000_New.csv") 
BMI <- children[,c(names(children)[grep("^BMI", names(children))])]
apply(BMI, 2, sd)
apply(BMI, 2, mean)

children$z1gender1 <- ifelse(children$z1gender == 1, 1, 0)
children$r1stchildethn2 <- ifelse(children$r1stchildethn == 2, 1, 0)
children$r1stchildethn3 <- ifelse(children$r1stchildethn == 3, 1, 0)
children$r1stchildethn4 <- ifelse(children$r1stchildethn == 4, 1, 0)

inter <- age2[1] - (0.5 * mean(children$z1gender1) + 2.6 * mean(children$r1stchildethn2) + 
                      3.3 * mean(children$r1stchildethn3) - 2.2 * mean(children$r1stchildethn4))

eMean <-  inter +  (0.5 * (children$z1gender1) + 2.6 * (children$r1stchildethn2) + 
                      3.3 * (children$r1stchildethn3) - 2.2 * (children$r1stchildethn4))

BMI2 <- sapply(eMean, function(x) rnorm(1, x, age2[2] - sd(eMean)))

mean(BMI2)

z1OverweightBMILvl1 <- z1ObeseLvl1 <- rep(NA, 5000)

BMIboy <- BMI2[children$z1gender==1]
z1OverweightBMILvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= overweightCutoff[1, 1],1,0)

z1ObeseLvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= obeseCutoff[1, 1],1,0)

BMIgirl<- BMI2[children$z1gender==0]

z1OverweightBMILvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= overweightCutoff[1, 2],1,0)

z1ObeseLvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= obeseCutoff[1, 2],1,0)


prop.table(table(z1OverweightBMILvl1))
prop.table(table(z1ObeseLvl1))






#########################################################################################

BMI <- children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])]


fun <- 
  function(par, age){
    
    BMI[,paste0("BMI", age )]<- BMI[,paste0("BMI", age )] 
    
    BMI[,paste0("BMI", age)]<- BMI[,paste0("BMI", age )] 
    
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
    
    propTable <- function(x) prop.table(table(x))[2]
    
    OverweightBMI = z1OverweightBMILvl1 %>% select(-r1stchildethn, -z1gender) %>% summarise_all(funs(propTable)) %>% as.numeric()
    OverweightNZHS = predict(lm(OverweightObese ~ I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
    
    ObeseBMI = z1ObeseLvl1 %>% select(-r1stchildethn, -z1gender) %>% summarise_all(funs(propTable)) %>% as.numeric()
    ObeseNZHS = predict(lm(Obese  ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))
    
    abs(OverweightBMI[age-1]*100 - OverweightNZHS[age-1] ) +  abs(ObeseBMI[age-1]*100  - ObeseNZHS[age-1] )
  }


age2 = optim(c(1,2), fun, age = 2)$par
age3 =optim(c(1,1), fun, age = 3)$par
age4 =optim(c(1,1), fun, age = 4)$par
age5 =optim(c(1,1), fun, age = 5)$par
age6 =optim(c(1,1), fun, age = 6)$par
age7 =optim(c(1,1), fun, age = 7)$par
age8 =optim(c(0.9,1.1), fun, age = 8)$par
age9 =optim(c(1,1), fun, age = 9)$par
age10 =optim(c(1,1), fun, age = 10)$par
age11 =optim(c(1,1), fun, age = 11)$par
age12 =optim(c(1,1), fun, age = 12)$par
age13 =optim(c(1,1), fun, age = 13)$par
age14 =optim(c(1,1), fun, age = 14)$par
age15 =optim(c(1,1), fun, age = 15)$par
age16 =optim(c(1,1), fun, age = 16)$par
age17 =optim(c(1,1), fun, age = 17)$par
age18 =optim(c(1,1), fun, age = 18)$par
age19 =optim(c(1,1), fun, age = 19)$par
age20 =optim(c(1,1), fun, age = 20)$par
age21 =optim(c(1,1), fun, age = 21)$par

temp <- 
  cbind(age2, age3, age4, age5, age6, age7, age8, age9, age10,
        age11, age12, age13, age14, age15, age16, age17, age18, age19,
        age20, age21)

BMI <- children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])]

BMI[, grep("^BMI", names(BMI))] <-
  t(apply(BMI[, grep("^BMI", names(BMI))], 1, function(x) x * temp[1,]))

BMI[, grep("^BMI", names(BMI))] <-
  t(apply(BMI[, grep("^BMI", names(BMI))], 1, function(x) x + temp[2,]))


BMI %>% group_by(z1gender, r1stchildethn) %>% summarise_all(funs(mean)) %>% t()


z1OverweightBMILvl1 <- z1ObeseLvl1 <- BMI

for(i in 2:21){
  BMIcur <- z1OverweightBMILvl1[, i + 1]
  
  BMIboy <- sapply(BMIcur[children$z1gender==1], function(x) rnorm(1, x, sdBMI[i-1, 1] * 0.8))
  
  z1OverweightBMILvl1[children$z1gender==1, i + 1] <- 
    ifelse(BMIboy >= overweightCutoff[i-1, 1],1,0)
  
  
  z1ObeseLvl1[children$z1gender==1, i + 1] <- 
    ifelse(BMIboy >= obeseCutoff[i-1, 1],1,0)
  
  
  
  BMIgirl<- sapply(BMIcur[children$z1gender==0], function(x) rnorm(1, x, sdBMI[i-1, 2]* 0.8))
  
  z1OverweightBMILvl1[children$z1gender==0, i + 1] <- 
    ifelse(BMIgirl >= overweightCutoff[i-1, 2],1,0)
  
  z1ObeseLvl1[children$z1gender==0, i + 1] <- 
    ifelse(BMIgirl >= obeseCutoff[i-1, 2],1,0)
  
}

propTable <- function(x) prop.table(table(x))[2]

data.frame( BMI = z1OverweightBMILvl1 %>% select(-r1stchildethn, -z1gender) %>% summarise_all(funs(propTable)) %>% as.numeric(),
            NZHS = predict(lm(OverweightObese ~ I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))/100)

data.frame( BMI = z1ObeseLvl1 %>% select(-r1stchildethn, -z1gender) %>% summarise_all(funs(propTable)) %>% as.numeric(),
            NZHS = predict(lm(Obese  ~  I(Age) + I(Age^2), NZHS2014[1:7,]), data.frame(Age = 2:21))/100)

BMI %>% group_by(z1gender, r1stchildethn) %>% summarise_all(funs(mean)) %>% t()



# BMI[children$z1gender == 0 & children$r1stchildethn==1,-c(1,2)] =
#   BMI[children$z1gender == 0 & children$r1stchildethn==1,-c(1,2)]  + 0.65
# BMI[children$z1gender == 0 & children$r1stchildethn==2,-c(1,2)] =
#   BMI[children$z1gender == 0 & children$r1stchildethn==2,-c(1,2)]  + 1.31
# BMI[children$z1gender == 0 & children$r1stchildethn==3,-c(1,2)] =
#   BMI[children$z1gender == 0 & children$r1stchildethn==3,-c(1,2)] - 0.95
# BMI[children$z1gender == 0 & children$r1stchildethn==4,-c(1,2)] =
#   BMI[children$z1gender == 0 & children$r1stchildethn==4,-c(1,2)]  - 2.14
# 
# 
# BMI[children$z1gender == 1 & children$r1stchildethn==1,-c(1,2)] =
#   BMI[children$z1gender == 1 & children$r1stchildethn==1,-c(1,2)]
# BMI[children$z1gender == 1 & children$r1stchildethn==2,-c(1,2)] =
#   BMI[children$z1gender == 1 & children$r1stchildethn==2,-c(1,2)]  + 0.5
# BMI[children$z1gender == 1 & children$r1stchildethn==3,-c(1,2)] =
#   BMI[children$z1gender == 1 & children$r1stchildethn==3,-c(1,2)] - 0.32
# BMI[children$z1gender == 1 & children$r1stchildethn==4,-c(1,2)] =
#   BMI[children$z1gender == 1 & children$r1stchildethn==4,-c(1,2)]  - 3.03


BMI %>% group_by(z1gender, r1stchildethn) %>% summarise_all(funs(mean)) %>% t()

BMI %>% select(-r1stchildethn) %>% group_by(z1gender) %>% summarise_all(funs(mean)) %>% t()

BMI %>% select(-z1gender) %>% group_by(r1stchildethn) %>% summarise_all(funs(mean)) %>% t()


z1OverweightBMILvl1 %>% select(-r1stchildethn) %>% group_by(z1gender) %>% summarise_all(funs(propTable)) %>% t()

z1OverweightBMILvl1 %>% select(-z1gender) %>% group_by(r1stchildethn) %>% summarise_all(funs(propTable)) %>% t()


z1ObeseLvl1 %>% select(-r1stchildethn) %>% group_by(z1gender) %>% summarise_all(funs(propTable)) %>% t()

z1ObeseLvl1 %>% select(-z1gender) %>% group_by(r1stchildethn) %>% summarise_all(funs(propTable)) %>% t()



apply(z1OverweightBMILvl1[,-c(1,2)], 2, function(x) x[children$z1gender==1] )

z1OverweightBMILvl1 <- numeric(5000) 
z1ObeseLvl1 <- numeric(5000) 

BMIboy <- BMI[children$z1gender==1]

z1OverweightBMILvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= overweightCutoff[iteration-1, 1],1,0)


z1ObeseLvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= obeseCutoff[iteration-1, 1],1,0)



BMIgirl<- BMI[children$z1gender==0]

z1OverweightBMILvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= overweightCutoff[iteration-1, 2],1,0)

z1ObeseLvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= obeseCutoff[iteration-1, 2],1,0)


prop.table(table(z1OverweightBMILvl1))

prop.table(table(z1ObeseLvl1))




propTable <- function(x){
  
  
  
  
}


prop.table(table(x[-1])[1])

rbind(0:ncol(BMI), BMI) %>% select(-r1stchildethn, -z1gender) %>% summarise_all(funs(propTable)) %>% t()





BMI <- BMI %>% gather( Age, BMI, -r1stchildethn, -z1gender)




BMI$Age <- as.numeric(factor(BMI$Age,  levels = paste0("BMI", 2:21))) + 1

BMI[children$r1stchildethn==4,] = BMI[children$r1stchildethn==4,] *0.96
BMI[children$r1stchildethn==1,] = BMI[children$r1stchildethn==1,] *1.002

#write.csv(BMI, "BMI.csv")


BMI <- children[,c("r1stchildethn", "z1gender", paste0("BMI", 2))]
sd(BMI)

tempchildren <- BMI
tempchildren$z1gender <- factor(tempchildren$z1gender)
tempchildren$r1stchildethn <- factor(tempchildren$r1stchildethn)

tempchildren$z1gender1 <- ifelse(tempchildren$z1gender == 1, 1, 0)
tempchildren$r1stchildethn2 <- ifelse(tempchildren$r1stchildethn == 2, 1, 0)
tempchildren$r1stchildethn3 <- ifelse(tempchildren$r1stchildethn == 3, 1, 0)
tempchildren$r1stchildethn4 <- ifelse(tempchildren$r1stchildethn == 4, 1, 0)


m1 <- glm(BMI2~  r1stchildethn2 + r1stchildethn3 + r1stchildethn4, 
          tempchildren[tempchildren$z1gender == 1,], family = Gamma(link = "identity"))
summary(m1)

m1$sd <- sd(m1$residuals)

pBMI <- exp(predSimNorm(m1, tempchildren))

cor(BMI, pBMI)



# BMI[children$r1stchildethn==4] = BMI[children$r1stchildethn==4] *0.95
# BMI[children$r1stchildethn==1] = BMI[children$r1stchildethn==1] *1.002

BMI = pBMI

z1OverweightBMILvl1 <- numeric(5000) 
z1ObeseLvl1 <- numeric(5000) 

BMIboy <- BMI[children$z1gender==1]

z1OverweightBMILvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= overweightCutoff[iteration-1, 1],1,0)

prop.table(table(z1OverweightBMILvl1[children$z1gender==1]))

z1ObeseLvl1[children$z1gender==1] <- 
  ifelse(BMIboy >= obeseCutoff[iteration-1, 1],1,0)

prop.table(table(z1ObeseLvl1[children$z1gender==1]))


BMIgirl<- BMI[children$z1gender==0]

z1OverweightBMILvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= overweightCutoff[iteration-1, 2],1,0)

prop.table(table(z1OverweightBMILvl1[children$z1gender==0]))

z1ObeseLvl1[children$z1gender==0] <- 
  ifelse(BMIgirl >= obeseCutoff[iteration-1, 2],1,0)

prop.table(table(z1ObeseLvl1[children$z1gender==0]))

prop.table(table(z1OverweightBMILvl1))

prop.table(table(z1ObeseLvl1))

tapply(z1OverweightBMILvl1, children$r1stchildethn, function(x) prop.table(table(x)))

tapply(z1ObeseLvl1, children$r1stchildethn, function(x) prop.table(table(x)))



# > tableBuilderNew(env.base, statistic = "freq", "z1OverweightLvl1", grpbyName = "r1stchildethn") %>% 
#   +   filter(Var == "Overweight") %>% 
#   +   select(-Lower, -Upper) %>%  spread(groupByData, Mean)
# Joining, by = c("Year", "A0")
# Joining, by = c("Year", "groupByData", "Run")
#           Var Year Asian Maori NZ Euro Pacific
# 1  Overweight    2  21.2  35.2    24.3    47.6
# 2  Overweight    3  19.6  37.6    24.1    47.9
# 3  Overweight    4  20.1  37.9    24.0    48.0
# 4  Overweight    5  21.0  40.1    24.0    50.3
# 5  Overweight    6  21.1  41.7    24.7    51.5
# 6  Overweight    7  21.7  41.4    24.8    50.1
# 7  Overweight    8  22.0  42.0    25.1    52.6
# 8  Overweight    9  23.9  42.9    25.1    54.0
# 9  Overweight   10  23.5  43.3    26.2    54.2
# 10 Overweight   11  23.3  45.4    27.1    54.6
# 11 Overweight   12  23.7  46.5    27.9    55.8
# 12 Overweight   13  27.0  46.0    29.6    54.2
# 13 Overweight   14  26.5  47.0    30.4    55.7
# 14 Overweight   15  28.4  46.5    32.4    57.1
# 15 Overweight   16  29.2  47.1    33.5    58.2
# 16 Overweight   17  29.6  49.1    35.4    60.2
# 17 Overweight   18  31.2  50.8    36.2    60.8
# 18 Overweight   19  30.4  52.2    38.5    60.7
# 19 Overweight   20  31.2  54.4    39.9    62.2
# 20 Overweight   21  33.3  55.5    41.8    64.5
# > 
#   > tableBuilderNew(env.base, statistic = "freq", "z1OverweightBMILvl1", grpbyName = "r1stchildethn") %>% 
#   +   filter(Var == "Overweight") %>% 
#   +   select(-Lower, -Upper) %>%  spread(groupByData, Mean)
# Joining, by = c("Year", "A0")
# Joining, by = c("Year", "groupByData", "Run")
#           Var Year Asian Maori NZ Euro Pacific
# 1  Overweight    2  13.7  22.9    19.4    25.6
# 2  Overweight    3  19.5  32.4    26.7    39.1
# 3  Overweight    4  18.2  33.3    26.1    42.5
# 4  Overweight    5  21.2  33.3    26.5    44.5
# 5  Overweight    6  21.5  34.4    24.8    45.8
# 6  Overweight    7  22.3  37.2    25.7    50.4
# 7  Overweight    8  25.0  39.4    26.4    52.8
# 8  Overweight    9  26.5  40.8    26.5    56.5
# 9  Overweight   10  26.3  41.7    26.3    60.2
# 10 Overweight   11  28.0  43.0    27.8    62.9
# 11 Overweight   12  26.8  44.7    28.3    63.2
# 12 Overweight   13  27.8  46.7    29.6    64.9
# 13 Overweight   14  29.5  49.3    31.0    69.3
# 14 Overweight   15  31.1  51.1    33.0    66.4
# 15 Overweight   16  33.0  51.6    35.1    66.3
# 16 Overweight   17  35.1  51.9    36.2    65.4
# 17 Overweight   18  36.4  50.9    38.0    63.9
# 18 Overweight   19  38.2  52.8    41.3    64.5
# 19 Overweight   20  42.6  53.4    43.2    64.4
# 20 Overweight   21  43.2  54.7    43.7    65.9











