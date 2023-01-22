nrecords = 1000 
ncols = 3 
x <- matrix( runif( nrecords*ncols), ncol=ncols) 
y <- 2 + x[,1] + 3*x[,2] - 4*x[,3] 
y = exp(y)/(1+exp(y)) 
ppp <- runif(nrecords) 
y <- ifelse(y > ppp, "good", "bad") 
testdata <- data.frame(y, x)

obs.prob <- function(x) {
  ## observed fraction of good, the 
  ## second level in this case 
  out <- table(x) 
  out[2]/length(x)
}

plotfr <- function(y, x, n=10) {
  tmp <- cut(x,n) 
  p <- tapply(y,tmp, obs.prob) 
  plot(p) 
  lines(p) 
  title(
    paste(deparse(substitute(y)), 
    "and",
    deparse(substitute(x))) 
  ) 
}


par(mfrow=c(2,2)) 
plotfr(testdata$y, testdata$X1) 
plotfr(testdata$y, testdata$X2) 
plotfr(testdata$y, testdata$X3)


test.glm = glm(y ~ X1 + X2 + X3, family = binomial, data=testdata) 
summary(test.glm)

y.pred <- predict(test.glm, newdata = data.frame(x))

y.preds = exp(y.pred)/(1+exp(y.pred)) 
ppp <- runif(nrecords) 
y.preds <- ifelse(y.preds > ppp, "good", "bad") 
y.preds

preddata <- data.frame(y.preds, x)

par(mfrow=c(2,2)) 
plotfr(preddata$y, preddata$X1) 
plotfr(preddata$y, preddata$X2) 
plotfr(preddata$y, preddata$X3)

simulate(test.glm)

#manual
yp1 <- predict(test.glm, newdata = data.frame(x))

co <- coef(test.glm)
y <- 2 + x[,1] + 3*x[,2] - 4*x[,3] 

yp2 <- co["(Intercept)"] + co["X1"]*x[,1] + co["X2"]*x[,2] + co["X3"]*x[,3]

#names(yp1) <- NULL
#names(yp2)
str(yp2)
all.equal(yp1,yp2,check.names=FALSE,check.attributes=FALSE)

#stripped model
strip.glm <- function (f) { 
  f.str <- list(coefficients=f$coefficients, 
  family=f$family, 
  terms=f$terms, 
  qr=list(pivot=f$qr$pivot), 
  rank=f$rank) 
  attr(f.str$terms, ".Environment") <- globalenv() 
  class(f.str) <- class(f) 
  f.str 
} 

s.glm <- strip.glm(test.glm)

#to make s.glm print properly:
s.glm$null.deviance <- test.glm$null.deviance
s.glm$deviance <- test.glm$deviance
s.glm$aic <- test.glm$aic


yp3 <- predict(s.glm, newdata = data.frame(x))

names(yp3) <- NULL
all.equal(yp1,yp3)
all.equal(yp3,predictor)
tt <- terms(test.glm)

ttt <- delete.response(tt)

model.frame(tt, data.frame(x))

methods(model.frame)

predict.lm


tt <-
delete.response(tt)

model.glm <- models$gpmorb
newdata <- simframe$values

newdata$age_minus1 <- 2

# get terms from model.glm in newdata
mf <- model.frame(delete.response(terms(model.glm)), newdata)
mf[,3]

#modified model with * term
#--------------------------
m.glm <- s.glm
m.glm$terms <- terms(as.formula("y~I(X1)+I(X2)+I(X3*X3)"))

mp1 <- predict(m.glm, newdata = data.frame(x))
co <- coef(m.glm)
#y <- 2 + x[,1] + 3*x[,2] - 4*x[,3] 

mp2 <- co["(Intercept)"] + co["X1"]*x[,1] + co["X2"]*x[,2] + co["X3"]*x[,3]*x[,3]

#names(yp1) <- NULL
#names(yp2)
str(yp2)
all.equal(mp1,mp2,check.names=FALSE,check.attributes=FALSE)
all.equal(mp1,yp1,check.names=FALSE,check.attributes=FALSE)

