captureAll <- function (expr) 
{
	rval <- NULL
	file <- textConnection("rval", "w", local = TRUE)
	sink(file, type = "output")
	assign("warning", function(..., call. = TRUE, immediate. = FALSE, 
					domain = NULL) {
				args <- list(...)
				if (length(args) == 1 && inherits(args[[1]], "condition")) {
					base::warning(..., call. = call., immediate. = immediate., 
							domain = domain)
				}
				else {
					oldwarn <- getOption("warn")
					if (immediate. && oldwarn < 1) {
						options(warn = 1)
						on.exit(options(warn = oldwarn))
					}
					.Internal(warning(as.logical(call.), as.logical(immediate.), 
									.makeMessage(..., domain = domain)))
				}
			}, envir = TempEnv())
	on.exit({
				sink(type = "output")
				close(file)
				if (exists("warning", envir = TempEnv())) rm("warning", 
							envir = TempEnv())
			})
	"evalVis" <- function(Expr) {
		owarns <- getOption("warning.expression")
		options(warning.expression = expression())
		on.exit({
					nwarns <- getOption("warning.expression")
					if (!is.null(nwarns) && length(as.character(nwarns)) == 
							0) options(warning.expression = owarns)
				})
		res <- try(withCallingHandlers(.Internal(eval.with.vis(Expr, 
										.GlobalEnv, baseenv())), warning = function(e) {
							msg <- conditionMessage(e)
							call <- conditionCall(e)
							wl <- getOption("warning.length")
							if (is.null(wl)) 
								wl <- 1000
							if (nchar(msg) > wl) 
								msg <- paste(substr(msg, 1, wl), .gettext("[... truncated]"))
							Warn <- getOption("warn")
							if (!is.null(call) && identical(call[[1]], quote(eval.with.vis))) 
								e$call <- NULL
							if (Warn < 0) {
								return()
							}
							else if (Warn == 0) {
								if (exists("warns", envir = TempEnv())) {
									lwarn <- get("warns", envir = TempEnv())
								}
								else lwarn <- list()
								if (length(lwarn) >= 50) 
									return()
								assign("warns", append(lwarn, list(e)), envir = TempEnv())
								return()
							}
							else if (Warn > 1) {
								msg <- .gettextf("(converted from warning) %s", 
										msg)
								stop(simpleError(msg, call = call))
							}
							else {
								if (!is.null(call)) {
									dcall <- deparse(call)[1]
									prefix <- paste(.gettext("Warning in"), dcall, 
											": ")
									sm <- strsplit(msg, "\n")[[1]]
									if (nchar(dcall, type = "w") + nchar(sm[1], 
											type = "w") > 58) 
										prefix <- paste(prefix, "\n  ", sep = "")
								}
								else prefix <- .gettext("Warning : ")
								msg <- paste(prefix, msg, "\n", sep = "")
								cat(msg)
							}
						}, interrupt = function(i) cat(.gettext("<INTERRUPTED!>\n")), 
						error = function(e) {
							call <- conditionCall(e)
							msg <- conditionMessage(e)
							if (!is.null(call) && identical(call[[1]], quote(eval.with.vis))) 
								call <- NULL
							if (!is.null(call)) {
								dcall <- deparse(call)[1]
								prefix <- paste(.gettext("Error in "), dcall, 
										": ")
								sm <- strsplit(msg, "\n")[[1]]
								if (nchar(dcall, type = "w") + nchar(sm[1], 
										type = "w") > 61) 
									prefix <- paste(prefix, "\n  ", sep = "")
							}
							else prefix <- .gettext("Error: ")
							msg <- paste(prefix, msg, "\n", sep = "")
							.Internal(seterrmessage(msg[1]))
							if (identical(getOption("show.error.messages"), 
									TRUE)) {
								cat(msg)
							}
						}, message = function(e) {
							signalCondition(e)
							cat(conditionMessage(e))
						}), silent = TRUE)
		if (exists("warns", envir = TempEnv())) {
			warns <- get("warns", envir = TempEnv())
			last.warning <- lapply(warns, "[[", "call")
			names(last.warning) <- sapply(warns, "[[", "message")
			attr(res, "last.warning") <- last.warning
			rm("warns", envir = TempEnv())
		}
		return(res)
	}
	WarningMessage <- function(last.warning) {
		assign("last.warning", last.warning, envir = baseenv())
		n.warn <- length(last.warning)
		if (n.warn < 11) {
			print.warnings(warnings(" ", sep = ""))
		}
		else if (n.warn >= 50) {
			cat(.gettext("There were 50 or more warnings (use warnings() to see the first 50)\n"))
		}
		else {
			cat(.gettextf("There were %d warnings (use warnings() to see them)\n", 
							n.warn))
		}
		return(invisible(n.warn))
	}
	for (i in 1:length(expr)) {
		tmp <- evalVis(expr[[i]])
		if (inherits(tmp, "try-error")) {
			last.warning <- attr(tmp, "last.warning")
			if (!is.null(last.warning)) {
				cat(.gettext("In addition: "))
				WarningMessage(last.warning)
			}
			break
		}
		else {
			if (tmp$visible) 
				print(tmp$value)
			last.warning <- attr(tmp, "last.warning")
			if (!is.null(last.warning)) 
				WarningMessage(last.warning)
		}
	}
	cat("\n")
	return(rval)
}