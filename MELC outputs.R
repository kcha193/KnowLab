# Displays MELC simulation outputs
# 
# Author: oman002
###############################################################################


#' melcModelCoefs(models)
melcModelCoefs <- function(models) {
	models_not_burt_not_cond <- remove.elements(models, c("burt8", "burt9_10", "burt11_13", "cond6", "cond7_8", "cond9_10"))
	
	model_coefs_all <- model_coefs_names_unique(models)
	model_coefs_all_not_burt_not_cond <- model_coefs_names_unique(models_not_burt_not_cond)
	model_coefs_burt <- model_coefs_names_unique(list(models$burt8, models$burt9_10, models$burt11_13))
	model_coefs_cond <- model_coefs_names_unique(list(models$cond6, models$cond7_8, models$cond9_10))
	model_coefs_burt_and_cond <- unique(c(burt,cond))
	
	unique_to_burt_and_cond <- setdiff(model_coefs_burt_and_cond, model_coefs_all_not_burt_not_cond )
	
	cat("Model coefficients for all models except burt and cond\n")
	cat(as.csv.string(model_coefs_all_not_burt_not_cond))
	
	cat("Model coefficients unique to burt and cond\n")
	cat(as.csv.string(unique_to_burt_and_cond))
}

#' Get melc run results collated for all modules
run_results_collated_all_modules <- function(env=env.base) 	
	env$modules[[1]]$run_results_collated



pco <- function() printOutputs(file="results/delme.csv", as.CSV = T)

#' Print the results from the simulation
#' 
#' @param env
#'  sim environment to print
#' @param file 
#'  file to print results to, or default to print to the console
#' @param as.CSV
#'  if TRUE, print results as comma separated values
#' 
#' @examples
#' printOutputs()
#' printOutputs(file="results\\delme.csv")
#' printOutputs(as.CSV = T)
printOutputs <- function(env=env.base, file="", as.CSV = F) {
	
	cat("", file=file) # clear file, if any
	
	cat <- function(...) {
		#output to file with append
		base::cat(..., file=file, append=T)
	}
	
	print_out <- function(x) {
		if (as.CSV) {
			cat(as.csv.string.list(x),sep="")
		} else {
			print(stripMeta.list(x))
		}
	}
	
	results <- run_results_collated_all_modules(env)
	
	results$confreqs <- lapply(results$confreqs, function (mx) {
				#mx <- confreqs$gpprev
				mx <- remove.zero.cols(remove.NA.cols(remove.cols.named(remove.rows.named(mx, "All Years"), "NA (%)")))
			})
	
	cat("Simulation name:,", env$name, "\n", sep="")
	cat("Number of children: ", env$numberOfUnits(), "\n")
	cat("Number of simulation runs:,", env$num_runs_simulated, "\n")
	cat("Random seed:,",as.csv.string(.Random.seed[1:5]))
	
	cat("\nCategorical variable frequencies\n\n")
	print_out(results$freqs)
	
	encat("\nCategorical variable frequencies by ethnicity\n\n")
	print_out(results$freqs_by_ethnicity)
	
	cat("\nMeans\n\n")
	print_out(results$means)
	
	cat("\nMeans by ethnicity\n\n")
	print_out(results$means_by_ethnicity)
	
	cat("\nSummaries\n\n")
	print_out(results$summaries)
	
	cat("\nContinuous variable frequencies\n\n")
	t.confreqs <- lapply(results$confreqs,t)
	print_out(t.confreqs)
	
}


printOutputsRoysPaper <- function(env=env.base, file="", as.CSV = F) {
	
	cat("", file=file) # clear file, if any
	
	cat <- function(...) {
		#output to file with append
		base::cat(..., file=file, append=T)
	}
	
	print_out <- function(x) {
		if (as.CSV) {
			cat(as.csv.string.list(x),sep="")
		} else {
			print(stripMeta.list(x))
		}
	}
	
	results <- melc_run_results_collated_all_modules(env)
	
	results$confreqs <- lapply(results$confreqs, function (mx) {
				#mx <- confreqs$gpprev
				mx <- remove.zero.cols(remove.NA.cols(remove.cols.named(remove.rows.named(mx, "All Years"), "NA (%)")))
			})
	
	cat("Simulation name:,", env$name, "\n", sep="")
	cat("Number of children: ", env$numberOfUnits(), "\n")
	cat("Number of simulation runs:,", env$num_runs_simulated, "\n")
	cat("Random seed:,",as.csv.string(.Random.seed[1:5]))
	
	cat("\nMeans\n\n")
	print_out(results$means)
	
	cat("\nMean by SES - 3 categories")
	print_out(results$means_by_SESBTH)
	
	cat("\nMean by SES - 2 categories")
	print_out(results$means_by_SESBTH_2cat)
	
	cat("\nMean by MAGE - 2 categories")
	print_out(results$means_by_MAGE_2cat)
	
	cat("\nMean by MAGE - 4 categories")
	print_out(results$means_by_MAGE_4cat)
	
	cat("\nMean by r1stmeduc - 2 categories")
	print_out(results$means_by_r1stmeduc_2cat)
	
	cat("\nMean by r1stmeduc - 3 categories")
	print_out(results$means_by_r1stmeduc)
	
	cat("\nMeans by ethnicity\n\n")
	print_out(results$means_by_ethnicity)
	
	#cat("\nSummaries\n\n")
	#print_out(results$summaries)
	
	cat("\nCategorical variable frequencies\n\n")
	print_out(results$freqs)
	
	cat("\nCategorical variable frequencies by ethnicity\n\n")
	print_out(results$freqs_by_ethnicity)
	
	cat("\nContinuous variable frequencies\n\n")
	t.confreqs <- lapply(results$confreqs,t)
	print_out(t.confreqs)
	
}

printCsvOutputsForValidation <- function(env=env.base, file="") {
	
	cat("", file=file) # clear file, if any
	
	cat <- function(...) {
		#output to file with append
		base::cat(..., file=file, append=T)
	}

	freqs <- c_list(env$modules, function(x) x$results$freqs$all)
	means <- c_list(env$modules, function(x) x$results$means$all) 
	quantiles <- c_list(env$modules, function(x) x$results$quantiles)

	cat("Simulation name:,", env$name, "\n", sep="")
	cat("Number of children: ", env$numberOfUnits(), "\n")
	cat("Number of simulation runs:,", env$runs_simulated, "\n")
	cat("Random seed:,",as.csv.string(.Random.seed[1:5]))
	
	cat("\nCategorical models frequencies\n\n")
	cat(as.csv.string.list(freqs),sep="")
	
	cat("\nCategorical models frequencies by ethnicity\n\n")
	cat(as.csv.string.list(freqs.grouped.by.ethnicity),sep="")
	
	cat("\nOutcome means\n\n")
	cat(as.csv.string.list(means),sep="")
	
	cat("\nMeans females by ethnicity\n\n")
	cat(as.csv.string.list(means.females.by.ethnicity),sep="")
	
	cat("\nOutcome summaries\n\n")
	cat(as.csv.string.list(summaries),sep="")
	
	
	cat("\nCategorical variable means (actual CHDS):\n\n")
	cat(as.csv.string.list(cat_var_means))
	
	cat("\nContinuous variable means (actual CHDS):\n\n")
	cat(as.csv.string.list(con_var_means))
	
	cat("\nContinuous variable quantiles (actual CHDS):\n\n")
	cat(as.csv.string.list(con_var_quantiles))
	
}


#' Print the kids & househouldsize outputs from the simulation in csv format
#' 
#' @examples
#' printCsvOutputsKidsHouseholdsize()
printCsvOutputsKidsHouseholdsize <- function(env=env.base) {
	
	years1_13 <- env$modules$years1_13$run_results_collated
	means <- list(kids=years1_13$means$kids, householdsize=years1_13$means$householdsize)
	confreqs <- list(kids=t(years1_13$confreqs$kids), householdsize=t(years1_13$confreqs$householdsize))
	
	cat(as.csv.string.list(means))
	cat(as.csv.string.list(confreqs))
	
}

#' Print the simulation outputs required for Roy's paper in csv format
#' 
#' @examples
#' printCsvOutputsRoysPaper()
#' printCsvOutputsRoysPaper(env.scenario)
printCsvOutputsRoysPaper <- function(env=env.base) {
	
	years1_13 <- env$modules$years1_13$run_results_collated
	
	resultvars <- c("gptotvis", "hadmtot", "houtptot")
	names(resultvars) <- resultvars
	
	r <- lapply(resultvars, function(var) {
				#var <- "gptotvis"
				cn <- colnames(years1_13$means[[var]]) 
				cn[1] <- "All families"
				colnames(years1_13$means[[var]]) <- cn
				cbind(
						dimnames_prepend_header(years1_13$means_by_SESBTH_2cat[[var]]),
						dimnames_prepend_header(years1_13$means_by_z1single0[[var]]),
						dimnames_prepend_header(years1_13$means_by_MAGE_2cat[[var]]),
						dimnames_prepend_header(years1_13$means_by_r1stmeduc_2cat[[var]]),
						years1_13$means[[var]]
				)
			})
	
	cat("Simulation name:,", env$name, "\n", sep="")
	cat("Number of children: ", env$numberOfUnits(), "\n")
	cat("Number of simulation runs:,", env$num_runs_simulated, "\n")
	cat("Random seed:,",as.csv.string(.Random.seed[1:5]))
	
	cat("Socio vars:\n")
	sociovars <- c("SESBTH", "z1single0", "z1single1", "r1stmeduc", "MAGE")
	cat(as.csv.string.list(env$presim.stats[sociovars], row.names=F))
	
	cat("\nOutcome means:\n\n")
	cat(as.csv.string.list(r),sep="")
	
	printCsvOutputsRoysPaperActualCHDS()
	
}

#' Print actual CHDS outcomes for use in Roy's paper
#' 
#' @example
#' printCsvOutputsRoysPaperActualCHDS()
printCsvOutputsRoysPaperActualCHDS <- function() {
	
	#actual chds hospital outcomes
	resultvars <- c(paste("gptotvis", 1:5, sep=""),
			paste("hadmtot", 1:5, sep=""),
			paste("houtptot", 1:5, sep=""))
	actualchds <- children[resultvars]
	
	actual.means <- t(t(colMeans(actualchds, na.rm=T)))
	colnames(actual.means) <- "All families"
	
	grping.vars <- c("SESBTH.2cat", "z1single0", "MAGE.2cat", "r1stmeduc.2cat", "MAGE.4cat")
	names(grping.vars) <- grping.vars
	
	actual.means.grped <-	lapply(grping.vars, function(gvar) {
				#gvar <- "SESBTH.2cat"
				result <- mean_mx_cols(actualchds, na.rm=T, grpby=children[[gvar]])
				#colnames(result) <- paste(gvar, names(gvar))
				result
				
			})
	
	actual.means.grped.mx <- do.call(cbind, actual.means.grped)
	
	cat("\nActual means:\n\n")
	cat(as.csv.string(actual.means))
	
	cat("\nActual means grouped:\n\n")
	cat(as.csv.string(actual.means.grped.mx))
	
}

#' printCsvOutputsActualCHDSForValidation(file="results/actual_CHDS.csv")
printCsvOutputsActualCHDSForValidation <- function(melc_basefile=read_csv(dirs$root, "base/validationDat20120312.csv"), file="") {
	
	cat("", file=file) # clear file, if any
	
	cat <- function(...) {
		#output to file with append
		base::cat(..., file=file, append=T)
	}
	
	expand_varnames <- function(varname_roots, years) {
		result <- lapply(varname_roots, function(varname_root) paste(varname_root, years, sep=""))
		structure(result, names=varname_roots)
	}
	
	#calculate frequencies (as proportions) for dichotomous variables
	calculate_freqs <- function(varnames_list) {
		lapply(varnames_list, function(varnames) {
					#varnames <- c("z1single1","z1single2")
					#varnames <- cat_varnames_list$z1chpar
					#varnames <- con_varnames_list$hadmtot
					tables <- table_mx_cols(melc_basefile[varnames], useNA="no")
					tables_as_percentages_and_without_zero_category <- sapply(tables, function(x) {
								#x <- tables[[1]]
								x_as_percentage <- prop.table(x)*100
								zero_category <- match("0", rownames(x_as_percentage))
								x_as_percentage[-zero_category]
							})
					
					as_matrix <- t(t(tables_as_percentages_and_without_zero_category))
					colnames(as_matrix) <- "Actual (%)"
					as_matrix
				})
	}
	
	#calculate frequencies (as proportions) for count variables
	calculate_cont_freqs <- function(varnames_list) {
		lapply(varnames_list, function(varnames) {
					freq.list <- table.grpby.mx.cols(melc_basefile[varnames], useNA="no")
					freq.matrix <- flatten.listmx(freq.list)
					freq.matrix[is.na(freq.matrix)] <- 0
					prop.matrix <- prop.table(freq.matrix, ROW)
					t.prop.matrix <- t(prop.matrix)*100
					colnames(t.prop.matrix) <- paste("year", strip.alpha(colnames(t.prop.matrix))) 
					t.prop.matrix
				})
	}
	
	calculate_means <- function(varnames_list) {
		lapply(varnames_list, function(varnames) {
					#varnames <- c("kids1",kids2")
					r <- t(t(colMeans(melc_basefile[varnames], na.rm=T)))
					colnames(r) <- "Mean"
					r
				})
	}
	
	calculate_quantiles <- function(varnames_list) {
		lapply(varnames_list, function(varnames) {
					quantile_mx_cols(melc_basefile[varnames], na.rm=T, probs=c(0,10,25,50,75,90,100)/100)
				}) 
	}
	
	children_in_simulation <- melc_basefile$a0 %in% children$A0
	melc_basefile <- melc_basefile[children_in_simulation, ]
	
	cat_varnames_list <- expand_varnames(c("z1single", "z1chpar", "welfare", "z1accom", "z1homeown", "z1overcrowd"), 1:13)
	
	con_varnames_list <- c( expand_varnames(c("kids", "householdsize", "chres", "mhrswrk", "fhrswrk", "msmoke", "fsmoke"), 1:13),
			expand_varnames(c("gpmorb","gpprev"),1:7),
			expand_varnames(c("gpresp", "gptotvis", "hadmtot", "houtptot"), 1:10),
			expand_varnames(c("BURT"), 8:13), 
			expand_varnames(c("cond"), 6:10))
	
	cat_var_freqs <- calculate_freqs(cat_varnames_list)
	con_var_freqs <- calculate_cont_freqs(con_varnames_list)
	con_var_means <- calculate_means(con_varnames_list)
	con_var_quantiles <- calculate_quantiles(con_varnames_list)
	
	cat("\nCategorical variable freqs (actual CHDS):\n\n")
	cat(as.csv.string.list(cat_var_freqs))
	
	cat("\nContinuous variable means (actual CHDS):\n\n")
	cat(as.csv.string.list(con_var_means))
	
	cat("\nContinuous variable quantiles (actual CHDS):\n\n")
	cat(as.csv.string.list(con_var_quantiles))
	
	cat("\nContinuous variable distributions (actual CHDS):\n\n")
	cat(as.csv.string.list(con_var_freqs))
	
}

#' Convert 1 column frequency tables of dichotomous variables into a matrix with 2 columns (the 
#' no and the yes column) with the correct column names.
#' A base simulation environment must be present for the function to work.
#' @param varname
#' A character string of the variable name which must include the "Lvl1" suffix
#' @return 
#' A matrix with 2 columns.  The first column in the "no" column and the second column is the "yes"
#' column. 
#' @examples 
#' userFormatDichotFreqs("z1singleLvl1")
userFormatDichotFreqs <- function(varname) {
	tab <- env.base$modules$years1_13$run_results_collated$freqs[[varname]]
	tab2 <- cbind(100-tab, tab)
	tab.names <- data.frame(names(env.base$dict$codings[[varname]]))
	colnames(tab2) <- apply(tab.names, 1, function(x) {paste(x, "(%)")}) 
	return(round(tab2,2))
}









