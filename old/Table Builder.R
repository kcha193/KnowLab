
#' Returns a dataset for use in a table by the Table Builder GUI.
#' Currently cannot do freqs for final outcomes (can only do freqs for those vars with binbreaks)
#' Also currently can only use a true categorical variable as a groupby variable - this coould 
#' be changed.
#' 
#' If the user defined a logiset/subgroup expression inthe scenario weightings screen and then
#' goes to tableBuilder() and sets a grpbyName, then the results they get will be on the entire 
#' dataset, not just on their subgroup they defined earlier.  The user can't define a logiset
#' expression in tableBuilder - the logisetexpr parameter is there so it can be used to show the 
#' user, in the scenario weightings screen, the distributions of the variable of interest for 
#' their subgroup only so they can better choose the proportions for their subgroup scenario.   
#' 
#'  @param envName 
#'  the environment to use - Base, Scenario 1, Scenario 2 etc.
#'  @param statistic
#'  the summary measure to use in producing the dataset - frequencies, means, quintiles
#'  @param variableName
#'  the variable to use in producing the dataset
#' 	@param grpbyName
#'  a subgroup by which to examine the variable
#' @param CI
#' logical indicating whether 95% confidence intervals should be generated
#' @param logisetexpr
#' a character expression which defines the logiset variable
#' @param not.in.logiset
#' logical.  If TRUE, then the results will be calculated on those not in the logiset rather 
#' than those in the logiset.
#' 
tableBuilder <- function(env, statistic, variableName, grpbyName="", CI=TRUE, 
                          logisetexpr=NULL, not.in.logiset=FALSE) {
	
	if (!is.null(logisetexpr)) {
		if (logisetexpr == "") {  
			logisetexpr <- NULL
		}
	}
	NUM_ITERATIONS <- nrow(env$modules[[1]]$run_results_collated$freqs[[1]])

	#do we need to have the simframe as an input or do we need to do an environment thing?
	catvars <- c(getOutcomeVars(env$simframe, "categorical"), "SESBTH", "r1stchildethn", "r1stmeduc", "r1stfeduc", "z1single0", "fage", "z1single0Lvl1", "bthorder", "NPRESCH", "z1genderLvl1")
	contvars <- c(getOutcomeVars(env$simframe, "continuous"), "bwkg", "pregalc", "ga", "INTERACT", "PUNISH", "MAGE", "pregsmk", "BREAST")
	presimvars <- c(names(env$presim.stats), "z1single0Lvl1", "z1genderLvl1", "NPRESCH", "INTERACT", "PUNISH")
	
	run_results <- env$modules[[1]]$run_results	

	run_tables <- lapply(run_results, function(single_run) {
				##single_run <- run_results[[1]]
				mx <- single_run$outcomes[[variableName]]
				if (is.null(mx)) {
					#variable is a time-invariant "presim" variable
					mx <- env$simframe[[variableName]]
					if (is.null(mx)) {
						#variable is a time-invariant "Lvl" variable and is only present in the simframe with its Lvl suffix
						which.vars <- str_locate_all(names(env$simframe), variableName)
						lvl.vars <- which(lapply(which.vars, length)>0)
						if (variableName%in%c("r1stfeduc", "r1stmeduc", "SESBTH")) {
							mx <- binary.levels.combine(env$simframe[[lvl.vars[1]]], env$simframe[[lvl.vars[2]]], env$simframe[[lvl.vars[3]]])
						} else if (variableName%in%c("r1stchildethn")) {
							mx <- binary.levels.combine(env$simframe[[lvl.vars[1]]], env$simframe[[lvl.vars[2]]], env$simframe[[lvl.vars[3]]], env$simframe[[lvl.vars[4]]])
						} else {
							stop("Check use of binary.levels.combine in tableBuilder")
						}
						#above code OK cos all the time-invariant Lvl vars only have three categories
						#but would be better to change code to be more generic
					}
					mx <- matrix(rep(mx, NUM_ITERATIONS), ncol=NUM_ITERATIONS, byrow=FALSE)
				}
				mx_wtotals <- structure(cbind(mx, "All Years"=rowMeans(mx, na.rm=TRUE)), varname=attr(mx,"varname"))
				
				if ( (variableName%in%contvars) & (tolower(statistic)=="frequencies") ) {
					if (is.null(binbreaks[[variableName]])) {
						stop(gettextf("Cannot produce frequencies for %s because it has no binbreaks",variableName))
					}
					# frequency for a continuous variable 
					mx <- apply(mx, COL, function(x) {
								#x <- mx[,1]
								bin(x, binbreaks[[variableName]])
							})
					mx_wtotals <- apply(mx_wtotals, COL, function(x) {bin(x, binbreaks[[variableName]])})
				}
				
				attr(mx, "varname") <- variableName
				attr(mx_wtotals, "varname") <- variableName
				
				grpbymx <- single_run$outcomes[[grpbyName]]
				
				if (is.null(grpbymx)) {
					if (grpbyName=="") {
						#if no grouping create a matrix of NAs
						grpbymx <- matrix(nrow=nrow(mx), ncol=NUM_ITERATIONS)
					} else {
						#if not outcome, use base variable from children
						grpbymx <- env$simframe[[grpbyName]]
						if (is.null(grpbymx)) {
							#variable is a time-invariant "Lvl" variable and is only present in the simframe with its Lvl suffix
							which.vars <- str_locate_all(names(env$simframe), grpbyName)
							lvl.vars <- which(lapply(which.vars, length)>0)
							if (grpbyName%in%c("r1stfeduc", "r1stmeduc", "SESBTH")) {
								grpbymx <- binary.levels.combine(env$simframe[[lvl.vars[1]]], env$simframe[[lvl.vars[2]]], env$simframe[[lvl.vars[3]]])
							} else if (grpbyName%in%c("r1stchildethn")) {
								grpbymx <- binary.levels.combine(env$simframe[[lvl.vars[1]]], env$simframe[[lvl.vars[2]]], env$simframe[[lvl.vars[3]]], env$simframe[[lvl.vars[4]]])
							}
						}
					}
				}
				if (length(grpbymx)==nrow(mx)) {
					#grpby is a vector or a 1 column matrix
					grpbymx_wtotals <- matrix(rep(grpbymx, ncol(mx)+1), ncol=ncol(mx)+1)
					attr(grpbymx_wtotals, "varname") <- grpbyName
				} else {
					#grpbymx is a matrix with 18 columns
					grpbymx_wtotals <- structure(cbind(grpbymx, "All Years"=round(rowMeans(grpbymx, na.rm=TRUE))), varname=attr(grpbymx,"varname"))
					if (is.null(attr(grpbymx_wtotals, "varname"))) {
						attr(grpbymx_wtotals, "varname") <- grpbyName
					}
				}
				
				#define logiset variable if logiset expression was specified
				outcomes <- single_run$outcomes
				if (!is.null(logisetexpr)) {
					prepended.exprs <- prepend.paths(logisetexpr, env)
					logiset.expr <- unlist(prepended.exprs["sg.expr"])
					names(logiset.expr) <- ""
					simframe <- env$simframe
					eval(parse(text=logiset.expr)) #creates sg.var which is actually a logiset.var
					logiset <- sg.var
					logiset <- as.matrix(logiset)
					
					if (ncol(logiset)>1) {
						logiset_wtotals <- structure(cbind(logiset, "All Years"=round(rowMeans(logiset, na.rm=TRUE))), varname=attr(logiset,"varname"))
					} else {
						logiset_wtotals <- logiset
					}
					
					if (not.in.logiset) {
						logiset <- !logiset
						logiset_wtotals <- !logiset_wtotals
					}
				} else if (is.null(logisetexpr)) {
					logiset <- NULL
					logiset_wtotals <- NULL
				}
				logiset <<- logiset
				logiset_wtotals <<- logiset_wtotals
				
				#select statistic generating function based on statistic parameter
				if (tolower(statistic)=="frequencies") {
						if (all(is.na(grpbymx))) {
							grpby <- NULL
						} else {
							grpby <- grpbymx
						}
						#table_mx_cols_MELC(mx, grpby=grpby, grpby.tag=grpbyName, logiset=logiset, dict=dict)
						
						table_mx_cols(mx, grpby=grpby, grpby.tag=grpbyName, logiset=logiset)
					
				} else if (tolower(statistic)=="means") {
					if (grpbyName=="") {
						grpby <- NULL
					} else {
						grpby <- grpbymx_wtotals
					}
					mean_mx_cols_BCASO(mx_wtotals, grpby=grpby, grpby.tag=grpbyName, logiset=logiset_wtotals, dict=env$dict)
					
				} else if (tolower(statistic)=="quintiles") {
					quantile_mx_cols_BCASO(mx_wtotals, grpby=grpbymx_wtotals, grpby.tag=grpbyName, new.names=c("Min", "10th", "25th", "50th", "75th","90th","Max"), 
					                       probs=c(0,.1,.25,.5,.75,.9,1), logiset=logiset_wtotals, na.rm=TRUE, dict=env$dict)
				}
			})
	
	#select collator based on statistic
	if (tolower(statistic)=="frequencies") {
		if (variableName %in% catvars) {
			
			two.cat.vars <- c("z1singleLvl1", "z1chparLvl1", "welfareLvl1", "z1accomLvl1", "z1homeownLvl1", "z1overcrowdLvl1", "z1single0", "z1single0Lvl1", "z1condLvl1")
			if (variableName %in% two.cat.vars) {
				#if a two category variable use the "remove_zero_cat" collator
				if (grpbyName=="z1condLvl1") {
					num.runs <- length(run_tables)
					for (k in 1:num.runs) {
						run_tables[[k]][[1]] <- run_tables[[k]][[4]]
						run_tables[[k]][[2]] <- run_tables[[k]][[4]]
						run_tables[[k]][[3]] <- run_tables[[k]][[4]]
					}
				}
				if (grpbyName=="NPRESCH") {
					num.runs <- length(run_tables)
					for (k in 1:num.runs) {
						run_tables[[k]][[1]] <- run_tables[[k]][[6]]
						run_tables[[k]][[2]] <- run_tables[[k]][[6]]
						run_tables[[k]][[3]] <- run_tables[[k]][[6]]
						run_tables[[k]][[4]] <- run_tables[[k]][[6]]
						run_tables[[k]][[5]] <- run_tables[[k]][[6]]
					}
				}
				result <- collator_freqs_remove_zero_cat3(run_tables, dict=env$dict, CI=CI, binbreaks=binbreaks[[variableName]])
				
				if ((CI==TRUE)&(length(run_results)>1)) {
					if (!(grpbyName%in%c("z1condLvl1", "NPRESCH")) & !(variableName%in%c("z1condLvl1", "NPRESCH"))) {
						#if variableName or grpbyName is z1cond or NPRESCH then there is no data at year 1 so we do not need to add the normal theory CI 
						#add normal theory CI for year 1
						result <- normal.theory.CIs(result, outcomes=env$modules[[1]]$outcomes[[1]], simframe=env$simframe, grpbyName=grpbyName, logiset=logiset)
					}
				}
			} else {
				#if has three or more categories
				if (grpbyName=="z1condLvl1") { #replace with code I wrote elsewhere (table_mx_cols_MELC?) that does this
					num.runs <- length(run_tables)
					for (k in 1:num.runs) {
						run_tables[[k]][[1]] <- run_tables[[k]][[4]]
						run_tables[[k]][[2]] <- run_tables[[k]][[4]]
						run_tables[[k]][[3]] <- run_tables[[k]][[4]]
					}
				}
				if (grpbyName=="NPRESCH") { #replace with code I wrote elsewhere (table_mx_cols_MELC?) that does this
					num.runs <- length(run_tables) 
					for (k in 1:num.runs) {
						run_tables[[k]][[1]] <- run_tables[[k]][[6]]
						run_tables[[k]][[2]] <- run_tables[[k]][[6]]
						run_tables[[k]][[3]] <- run_tables[[k]][[6]]
						run_tables[[k]][[4]] <- run_tables[[k]][[6]]
						run_tables[[k]][[5]] <- run_tables[[k]][[6]]
					}
				}
				result <- collator_freqs2(run_tables, dict=env$dict, CI=CI, binbreaks=binbreaks[[variableName]])
				if ((CI==TRUE)&(length(run_results)>1)) {
					if (!(grpbyName%in%c("z1condLvl1", "NPRESCH")) & !(variableName%in%c("z1condLvl1", "NPRESCH"))) {
						#if variableName is NPRESCH then there is no data at year 1 so we do not need to add the normal theory CI 
						#add normal theory CI for year 1
						result <- normal.theory.CIs(result, outcomes=env$modules[[1]]$outcomes[[1]], simframe=env$simframe, grpbyName=grpbyName, logiset=logiset)
					}
				}
			}
			
		} else if (variableName %in% contvars) {
			if (grpbyName=="z1condLvl1") {
				num.runs <- length(run_tables)
				for (k in 1:num.runs) {
					run_tables[[k]][[1]] <- run_tables[[k]][[4]]
					run_tables[[k]][[2]] <- run_tables[[k]][[4]]
					run_tables[[k]][[3]] <- run_tables[[k]][[4]]
				}
			}
			if ((grpbyName %in% c("NPRESCH", "INTERACT", "PUNISH")) | (variableName %in% c("NPRESCH", "INTERACT", "PUNISH"))) {
				num.runs <- length(run_tables)
				for (k in 1:num.runs) {
					run_tables[[k]][[1]] <- run_tables[[k]][[6]]
					run_tables[[k]][[2]] <- run_tables[[k]][[6]]
					run_tables[[k]][[3]] <- run_tables[[k]][[6]]
					run_tables[[k]][[4]] <- run_tables[[k]][[6]]
					run_tables[[k]][[5]] <- run_tables[[k]][[6]]
				}
			}
			result <- collator_freqs2(run_tables, dict=env$dict, CI=CI, binbreaks=binbreaks[[variableName]])
			if ((CI==TRUE)&(length(run_results)>1)) {
				#add normal theory CI for year 1
				if (grpbyName=="" & is.null(logisetexpr)) {
					#this step necessary because if a subgroup scenario has been run but the user is wanting a table for the entire population, this will not be achieved unless the grping.logical paramater is set to FALSE
					#If the grping.logical parameter is not set to FALSE, then the default is TRUE and the results from tableBuilder will be a table on user specified subgroup in the scenario even though there is no grpbyName or logiset
					#Plus, above they will not actually get this table because when it looks at the column names of result to determine whether a category of the variableName variable is present it will think none of the categories are present and return 0 for the entire row
					if (!(grpbyName%in%c("z1condLvl1", "NPRESCH")) & !(variableName%in%c("z1condLvl1", "NPRESCH"))) {
						result <- normal.theory.CIs(result, outcomes=env$modules[[1]]$outcomes[[1]], simframe=env$simframe, cat.adjustments=env$cat.adjustments, grpbyName=grpbyName, logiset=logiset, grping.logical=FALSE)
					}
					#gives a table for the entire population
				} else {
					if (!(grpbyName%in%c("z1condLvl1", "NPRESCH")) & !(variableName%in%c("z1condLvl1", "NPRESCH"))) {
						result <- normal.theory.CIs(result, outcomes=env$modules[[1]]$outcomes[[1]], simframe=env$simframe, cat.adjustments=env$cat.adjustments, grpbyName=grpbyName, logiset=logiset, grping.logical=TRUE)
					#gives a table for the user-specified subgroup
					}
				}
			}
		} else {
			stop(gettextf("Unknown variable %s. Cannot not find in catvars or contvars.", variableName))
		}
		
	} else if (tolower(statistic)=="means") {
		result <- collator_means(run_tables, dict=env$dict, CI=CI, NA.as.zero=FALSE)
		attr(result, "meta")[["varname"]] <- variableName
		if ((CI==TRUE)&(length(run_results)>1)) {
			#add normal theory CI for year 1
			#result <- normal.theory.CI.means(result, simframe=env$simframe, outcomes=env$modules$years1_13$outcomes, grpby=grpbyName, logiset=logiset)
			result <- normal.theory.CIs(result, statistic="mean", outcomes=env$modules[[1]]$outcomes[[1]], simframe=env$simframe, grpbyName=grpbyName, logiset=logiset)
			
		}
		
	} else if (tolower(statistic)=="quintiles") {
		result <- collator_list_mx(run_tables, CI=CI, NA.as.zero=FALSE)#dict = dict.MELC,
		
	}
	
	if (variableName %in% presimvars) {
		if (grpbyName=="" | grpbyName %in% presimvars) {
			#take first non NA year
			NA.year <- apply(result, 1, function(x) { all(is.na(x)) })
			first.non.na.yr <- which(NA.year==FALSE)[1]
			result <- result[first.non.na.yr,]
			if ((CI==TRUE)&(length(run_tables)>1)) {
				#take every third name
				nms <- names(result)[(1:(length(result)/3))*3-2]
				#remove "Mean" from it
				nms.noMean <- rep(NA, length(nms))
				for (i in 1:length(nms)) {
					nms.noMean[i] <- str_sub(nms[i], 1, regexpr("Mean", nms)[[i]]-2)
				}
				result <- matrix(result, byrow=TRUE, ncol=3)
				rownames(result) <- nms.noMean
				colnames(result) <- c("Mean", "Lower", "Upper")
			}
		}
	}
	if ((grpbyName%in%c("z1condLvl1", "NPRESCH")) | (variableName%in%c("z1condLvl1", "NPRESCH"))) {
		if (grpbyName=="z1condLvl1" | variableName=="z1condLvl1") {
			result[1:4,] <- NA
		}
	}
	return(result)
}
