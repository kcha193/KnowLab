
object <- models$gptotvis
object <- model.glm
na.action = na.pass
newdata = simframe.master

predict.lm(object, newdata)
 
#uses the following attributes of object:
#terms, rank, coefficients, stats:::qr.lm(object)$pivot

predict.lm <- function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
		interval = c("none", "confidence", "prediction"), level = 0.95, 
		type = c("response", "terms"), terms = NULL, na.action = na.pass, 
		pred.var = res.var/weights, weights = 1, ...) 
{
	tt <- terms(object)
	if (!inherits(object, "lm")) 
		warning("calling predict.lm(<fake-lm-object>) ...")
	if (missing(newdata) || is.null(newdata)) {
		mm <- X <- model.matrix(object)
		mmDone <- TRUE
		offset <- object$offset
	}
	else {
		Terms <- delete.response(tt)
		
		#get only Terms from newdata
		m <- model.frame(Terms, newdata, na.action = na.action, 
				xlev = object$xlevels)
		
		#check classes of Terms matches m (ie: newdata)
		if (!is.null(cl <- attr(Terms, "dataClasses"))) 
			.checkMFClasses(cl, m)
		
		#create design matrix, ie: an intercept + Terms
		X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
		offset <- rep(0, nrow(X))
		if (!is.null(off.num <- attr(tt, "offset"))) 
			for (i in off.num) offset <- offset + eval(attr(tt, 
								"variables")[[i + 1]], newdata)
		if (!is.null(object$call$offset)) 
			offset <- offset + eval(object$call$offset, newdata)
		mmDone <- FALSE
	}
	n <- length(object$residuals)
	p <- object$rank
	p1 <- seq_len(p)
	
	#index of terms
	piv <- if (p) 
		stats:::qr.lm(object)$pivot[p1]
	if (p < ncol(X) && !(missing(newdata) || is.null(newdata))) 
		warning("prediction from a rank-deficient fit may be misleading")
	
	# get coefficients
	beta <- object$coefficients
	
	#do matrix multiplication
	predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
	
	# select only piv
	#selected <- X[, piv, drop = FALSE]
	# do multiplication
	#predictor2 <- drop(selected %*% beta[piv])
	
	if (!is.null(offset)) 
		predictor <- predictor + offset
	interval <- match.arg(interval)
	if (interval == "prediction") {
		if (missing(newdata)) 
			warning("Predictions on current data refer to _future_ responses\n")
		if (missing(newdata) && missing(weights)) {
			w <- weights.default(object)
			if (!is.null(w)) {
				weights <- w
				warning("Assuming prediction variance inversely proportional to weights used for fitting\n")
			}
		}
		if (!missing(newdata) && missing(weights) && !is.null(object$weights) && 
				missing(pred.var)) 
			warning("Assuming constant prediction variance even though model fit is weighted\n")
		if (inherits(weights, "formula")) {
			if (length(weights) != 2L) 
				stop("'weights' as formula should be one-sided")
			d <- if (missing(newdata) || is.null(newdata)) 
						model.frame(object)
					else newdata
			weights <- eval(weights[[2L]], d, environment(weights))
		}
	}
	type <- match.arg(type)
	if (se.fit || interval != "none") {
		res.var <- if (is.null(scale)) {
					r <- object$residuals
					w <- object$weights
					rss <- sum(if (is.null(w)) r^2 else r^2 * w)
					df <- object$df.residual
					rss/df
				}
				else scale^2
		if (type != "terms") {
			if (p > 0) {
				XRinv <- if (missing(newdata) && is.null(w)) 
							qr.Q(qr.lm(object))[, p1, drop = FALSE]
						else X[, piv] %*% qr.solve(qr.R(qr.lm(object))[p1, 
											p1])
				ip <- drop(XRinv^2 %*% rep(res.var, p))
			}
			else ip <- rep(0, n)
		}
	}
	if (type == "terms") {
		if (!mmDone) {
			mm <- model.matrix(object)
			mmDone <- TRUE
		}
		aa <- attr(mm, "assign")
		ll <- attr(tt, "term.labels")
		hasintercept <- attr(tt, "intercept") > 0L
		if (hasintercept) 
			ll <- c("(Intercept)", ll)
		aaa <- factor(aa, labels = ll)
		asgn <- split(order(aa), aaa)
		if (hasintercept) {
			asgn$"(Intercept)" <- NULL
			if (!mmDone) {
				mm <- model.matrix(object)
				mmDone <- TRUE
			}
			avx <- colMeans(mm)
			termsconst <- sum(avx[piv] * beta[piv])
		}
		nterms <- length(asgn)
		if (nterms > 0) {
			predictor <- matrix(ncol = nterms, nrow = NROW(X))
			dimnames(predictor) <- list(rownames(X), names(asgn))
			if (se.fit || interval != "none") {
				ip <- matrix(ncol = nterms, nrow = NROW(X))
				dimnames(ip) <- list(rownames(X), names(asgn))
				Rinv <- qr.solve(qr.R(qr.lm(object))[p1, p1])
			}
			if (hasintercept) 
				X <- sweep(X, 2L, avx, check.margin = FALSE)
			unpiv <- rep.int(0L, NCOL(X))
			unpiv[piv] <- p1
			for (i in seq.int(1L, nterms, length.out = nterms)) {
				iipiv <- asgn[[i]]
				ii <- unpiv[iipiv]
				iipiv[ii == 0L] <- 0L
				predictor[, i] <- if (any(iipiv > 0L)) 
							X[, iipiv, drop = FALSE] %*% beta[iipiv]
						else 0
				if (se.fit || interval != "none") 
					ip[, i] <- if (any(iipiv > 0L)) 
								as.matrix(X[, iipiv, drop = FALSE] %*% Rinv[ii, 
														, drop = FALSE])^2 %*% rep.int(res.var, 
												p)
							else 0
			}
			if (!is.null(terms)) {
				predictor <- predictor[, terms, drop = FALSE]
				if (se.fit) 
					ip <- ip[, terms, drop = FALSE]
			}
		}
		else {
			predictor <- ip <- matrix(0, n, 0L)
		}
		attr(predictor, "constant") <- if (hasintercept) 
					termsconst
				else 0
	}
	if (interval != "none") {
		tfrac <- qt((1 - level)/2, df)
		hwid <- tfrac * switch(interval, confidence = sqrt(ip), 
				prediction = sqrt(ip + pred.var))
		if (type != "terms") {
			predictor <- cbind(predictor, predictor + hwid %o% 
							c(1, -1))
			colnames(predictor) <- c("fit", "lwr", "upr")
		}
		else {
			if (!is.null(terms)) 
				hwid <- hwid[, terms, drop = FALSE]
			lwr <- predictor + hwid
			upr <- predictor - hwid
		}
	}
	if (se.fit || interval != "none") {
		se <- sqrt(ip)
		if (type == "terms" && !is.null(terms)) 
			se <- se[, terms, drop = FALSE]
	}
	if (missing(newdata) && !is.null(na.act <- object$na.action)) {
		predictor <- napredict(na.act, predictor)
		if (se.fit) 
			se <- napredict(na.act, se)
	}
	if (type == "terms" && interval != "none") {
		if (missing(newdata) && !is.null(na.act)) {
			lwr <- napredict(na.act, lwr)
			upr <- napredict(na.act, upr)
		}
		list(fit = predictor, se.fit = se, lwr = lwr, upr = upr, 
				df = df, residual.scale = sqrt(res.var))
	}
	else if (se.fit) 
		list(fit = predictor, se.fit = se, df = df, residual.scale = sqrt(res.var))
	else predictor
}