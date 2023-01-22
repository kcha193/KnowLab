formula <- Terms; data = newdata

model.frame <- function (formula, data = NULL, subset = NULL, na.action = na.fail, 
		drop.unused.levels = FALSE, xlev = NULL, ...) 
{
	possible_newdata <- !missing(data) && is.data.frame(data) && 
			identical(deparse(substitute(data)), "newdata") && (nr <- nrow(data)) > 
			0
	if (!missing(formula) && nargs() == 1 && is.list(formula) && 
			!is.null(m <- formula$model)) 
		return(m)
	if (!missing(formula) && nargs() == 1 && is.list(formula) && 
			all(c("terms", "call") %in% names(formula))) {
		fcall <- formula$call
		m <- match(c("formula", "data", "subset", "weights", 
						"na.action"), names(fcall), 0)
		fcall <- fcall[c(1, m)]
		fcall[[1L]] <- as.name("model.frame")
		env <- environment(formula$terms)
		if (is.null(env)) 
			env <- parent.frame()
		return(eval(fcall, env, parent.frame()))
	}
	if (missing(formula)) {
		if (!missing(data) && inherits(data, "data.frame") && 
				length(attr(data, "terms"))) 
			return(data)
		formula <- as.formula(data)
	}
	else if (missing(data) && inherits(formula, "data.frame")) {
		if (length(attr(formula, "terms"))) 
			return(formula)
		data <- formula
		formula <- as.formula(data)
	}
	formula <- as.formula(formula)
	if (missing(na.action)) {
		if (!is.null(naa <- attr(data, "na.action")) & mode(naa) != 
				"numeric") 
			na.action <- naa
		else if (!is.null(naa <- getOption("na.action"))) 
			na.action <- naa
	}
	if (missing(data)) 
		data <- environment(formula)
	else if (!is.data.frame(data) && !is.environment(data) && 
			!is.null(attr(data, "class"))) 
		data <- as.data.frame(data)
	else if (is.array(data)) 
		stop("'data' must be a data.frame, not a matrix or an array")
	if (!inherits(formula, "terms")) 
		formula <- terms(formula, data = data)
	
	env <- environment(formula)
	rownames <- .row_names_info(data, 0L)
	vars <- attr(formula, "variables")
	predvars <- attr(formula, "predvars")
	
	if (is.null(predvars)) 
		predvars <- vars
	varnames <- sapply(vars, function(x) paste(deparse(x, width.cutoff = 500), 
						collapse = " "))[-1L]
	variables <- eval(predvars, data, env)
	resp <- attr(formula, "response")
	if (is.null(rownames) && resp > 0L) {
		lhs <- variables[[resp]]
		rownames <- if (is.matrix(lhs)) 
					rownames(lhs)
				else names(lhs)
	}
	if (possible_newdata && length(variables)) {
		nr2 <- max(sapply(variables, NROW))
		if (nr2 != nr) 
			warning(gettextf("'newdata' had %d rows but variable(s) found have %d rows", 
							nr, nr2), call. = FALSE)
	}
	if (is.null(attr(formula, "predvars"))) {
		for (i in seq_along(varnames)) predvars[[i + 1]] <- makepredictcall(variables[[i]], 
					vars[[i + 1]])
		attr(formula, "predvars") <- predvars
	}
	extras <- substitute(list(...))
	extranames <- names(extras[-1L])
	extras <- eval(extras, data, env)
	subset <- eval(substitute(subset), data, env)
	data <- .Internal(model.frame(formula, rownames, variables, 
					varnames, extras, extranames, subset, na.action))
	if (length(xlev)) {
		for (nm in names(xlev)) if (!is.null(xl <- xlev[[nm]])) {
				xi <- data[[nm]]
				if (is.character(xi)) {
					xi <- as.factor(xi)
					warning(gettextf("character variable '%s' changed to a factor", 
									nm), domain = NA)
				}
				if (!is.factor(xi) || is.null(nxl <- levels(xi))) 
					warning(gettextf("variable '%s' is not a factor", 
									nm), domain = NA)
				else {
					xi <- xi[, drop = TRUE]
					nxl <- levels(xi)
					if (any(m <- is.na(match(nxl, xl)))) 
						stop(gettextf("factor '%s' has new level(s) %s", 
										nm, paste(nxl[m], collapse = ", ")), domain = NA)
					data[[nm]] <- factor(xi, levels = xl, exclude = NULL)
				}
			}
	}
	else if (drop.unused.levels) {
		for (nm in names(data)) {
			x <- data[[nm]]
			if (is.factor(x) && length(unique(x[!is.na(x)])) < 
					length(levels(x))) 
				data[[nm]] <- data[[nm]][, drop = TRUE]
		}
	}
	attr(formula, "dataClasses") <- sapply(data, .MFclass)
	attr(data, "terms") <- formula
	data
}
