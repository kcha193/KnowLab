#' model <- models$gptotvis
#' newdata <- simframe.master
#' predict2(model,newdata)
#' predict2(model, .GlobalEnv)
#' attach(simframe.master)
#' all.equal(predict2(model,newdata), predict(model,newdata))
predict2 <- function(model, newdata) {
	
	tt <- terms(model)
	Terms <- delete.response(tt)
	
	#get only Terms from newdata
	m <- model.frame(Terms, newdata)
	
	#create design matrix, ie: an intercept + Terms
	X <- model.matrix(Terms, m)
	
	# matrix multiple X by model coefficients
	drop(X %*% model$coefficients)
	
}

# all.equal(predict(model,newdata),predict3(model,newdata))
predict3 <- function(model, newdata) {
	
	tt <- terms(model)
	Terms <- delete.response(tt)
	
	vars <- attr(delete.response(terms(model)), "variables")
	
	#evalute vars, return as list
	vars.evaluated <- eval(vars, envir=newdata)
	names(vars.evaluated) <- as.character(vars)[-1]
	
	#convert to matrix 
	vars.evaluated.mx <- as.matrixFromList(vars.evaluated, byrow = F)
	
	#add intercept of 1 
	vars.evaluated.mx <- cbind(Intercept=1, vars.evaluated.mx)
	
	# matrix multiple by model coefficients
	result <- drop(vars.evaluated.mx %*% model$coefficients)
	structure(result, names=row.names(newdata))
}

attach(simframe.master)
detach(simframe.master)
all.equal(predict4(model), predict(model, simframe.master), check.attributes=F)
all.equal(predict4(model), predict(model, simframe.master), check.attributes=F)



model <- models$gptotvis

newdata2 <- simframe.master
attach(newdata2)
newdata2$z1smoke <- rep(1,1075)
print(sum(z1smoke))


all.equal(z1smoke, newdata2$z1smoke) 

all.equal(predict4test(model), predict(model, newdata2), check.attributes=F)


z1smoke <- 0

all.equal(predict4testA(model), predict(model, newdata2), check.attributes=F)
all.equal(predict4testB(model), predict(model, newdata2), check.attributes=F)
all.equal(predict4testC(model), predict(model, newdata2), check.attributes=F)
all.equal(predict4testD(model), predict(model, newdata2), check.attributes=F)
all.equal(predict4testE(model), predict(model, newdata2), check.attributes=F)


predict4testA <- function(model) {
	
	z1smoke <- rep(1,1075)
	
	innerpred <- function(model) {
		predict4(model)
	}
	
	innerpred(model)
}


predict4testB <- function(model) {

	z1smoke <- rep(1,1075)
	
	innerpred <- function(model) {
		psb(model)
	}
	
	innerpred(model)
}

predict4testC <- function(model) {
	
	z1smoke <- rep(1,1075)
	
	innerpred <- function(model) {
		psb2(model)
	}
	
	innerpred(model)
}


predict4testD <- function(model) {
	
	z1smoke <- rep(1,1075)
	
	innerpred <- function(model) {
		psb3(model)
	}
	
	innerpred(model)
}


predict4testE <- function(model) {
	
	z1smoke <- rep(1,1075)
	
	innerpred <- function(model) {
		psb4(model)
	}
	
	innerpred(model)
}



psb <- function(model.glm, newdata) {
	predict4(model.glm)	
}

psb2 <- function(model.glm, newdata) {
	predict4(model.glm, envir=parent.frame())	
}

psb3 <- function(model.glm, newdata=parent.frame()) {
	predict4(model.glm, envir=newdata)	
}

psb4 <- function(model.glm, newdata=parent.frame()) {
	psb3(model.glm, newdata)	
}








outerpred <- function(model) {
	cat("innerpred frame: ", sys.nframe(),"\n")
	print(sys.frames())
	print(sum(z1smoke))
	predict4(model)
}


#' @param envir
#'  an environment in which model variables are evaluated. May also be NULL, a list, 
#'  a data frame, a pairlist or an integer as specified to sys.call.
#' 
#'  If not specified then the parent frame is used, ie: the environment of the function
#'  calling this (i.e: the dynamic parent rather than the lexical parent in which this
#'  function is defined).
#'  
#'  The usual order of evaluation is used, i.e: in envir, then its parents and along
#'  the search path. For example, if a model variable is reassigned in the function
#'  calling this, then that reassigned value will be used before a global or attached
#'  value.
predict4 <- function(model, envir = parent.frame()) {

	# get vars from model
	vars <- attr(delete.response(terms(model)), "variables")
	
	#evalute vars, return as list
	vars.evaluated <- eval(vars, envir)
	#names(vars.evaluated) <- as.character(vars)[-1]
	
	#convert to matrix 
	vars.evaluated.mx <- as.matrixFromList(vars.evaluated, byrow = F)
	
	#add intercept of 1 
	vars.evaluated.mx <- cbind(Intercept=1, vars.evaluated.mx)
	
	# matrix multiple by model coefficients
	drop(vars.evaluated.mx %*% model$coefficients)
}
