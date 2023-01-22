# Initialises all data and starts MELC base simulation
# 
# Author: oman002
###############################################################################

if (Sys.getenv("USELIB") != "") .USELIB <- Sys.getenv("USELIB") 

clearWorkspace <- function() {
	rmall <- function (exceptions = NULL) {
		vars <- ls(".GlobalEnv", all.names=TRUE)
		if (!is.null(exceptions)) {
			vars <- vars[!vars %in% exceptions]
		}
		rm(pos = ".GlobalEnv", list = vars)
	}
	
	#TODO: put RFace and Ascape.R functions in package
	.running_in_Ascape <- exists(".assignMatrix")
	if (!.running_in_Ascape) rmall(exceptions=c(".DEBUG",".NUMRUNS",".USELIB", "onload.expressions","DATA_DIR"))
	
	if ("package:simario" %in% search()) detach("package:simario", unload = T)
	while("simframe" %in% search()) detach("simframe")
}

clearWorkspace()

#' Create env.base and do a base simulation
#' 
#' @examples
#'  doSim.Base(simframe.master)

doSim.Base <- function(simframe=simframe.master, parallel) {
	env.base <<- SimenvMELC$new("Base", simframe)
	
	if (existsFunction("ascapeKeepObject")) ascapeKeepObject("env.base")
	
	if (!exists(".NUMRUNS")) .NUMRUNS <<- 1
	
	#
	if(parallel)
		print(env.base$simulateP(total_runs=.NUMRUNS))
	else 
		print(env.base$simulate(total_runs=.NUMRUNS))
}


		

loadSimario <- function() {
	.is_dev_environment <- length(find.package("devtools", quiet = T)) > 0
	if (.is_dev_environment & !exists(".USELIB")) {
		cat("loadSimario: loading pre-installed development version using load_all\n")
		
		library(stringr)
		library(stringi)
		library(devtools)
		library(testthat)
		library(snowfall)
		library(ggplot2)   
		library(gridExtra)
		
		#load_all("H:\\workspace\\simario\\src", reset = T)
		if(installed.packages()["devtools","Version"] >= 0.8) {
			simarioFun <<- load_all("../../simario/src", reset = TRUE)
		} else {
			load_all("simario", reset = T)
		} 
	
	
		#workaround for devtools issue https://github.com/hadley/devtools/issues/38
		Simenv$.super <- .GlobalEnv
		Simmodule$.super <- .GlobalEnv
	} else {
		cat("loadSimario: loading installed library\n")
		library(simario)
		cat("simario v", sessionInfo()$otherPkgs$simario$Version, "loaded\n")
	}
}

#' Initialise MELC models, basefile, simframe, and simulation control vars.
#' Perform base simulation.
#' 
#' @examples
#' basefiledir <- "D:/workspace.sim/MELC/CHDS/base/"
#' modelfiledir <- "D:/workspace.sim/MELC/CHDS/models/"
#' propensityfiledir <-  "D:/workspace.sim/MELC/CHDS/propensityFiles/"
#' catToCont.modelfiledir <- "D:/workspace.sim/MELC/CHDS/models_CatToCont/"
#' initMELC(dirs$base, dirs$models, dirs$propensityFiles)
#' initMELC(basefiledir, modelfiledir, propensityfiledir, catToCont.modelfiledir)
initMELC <- function() {
	
	
}

startMELC <- function(parallel) {
	
	#if no base simulation yet, then simulate 
	if (!exists("env.base")) {
		cat("Starting base simulation\n")
		
	#	 set same seed to begin with
		set.seed(1) 
		doSim.Base(simframe.master, parallel)
	}
	
	#create new scenario environment
	createNewScenarioEnvironment()	
}

createNewScenarioEnvironment <- function() {
	invisible(setScenarioSimenv(SimenvMELC$new()))
}


#setwd(file.path("C:/Users/kcha193/COMPASS/simario/", "src/demo/"))

setwd("C:/Users/kcha193/workspace/MELC/data")

loadSimario()

#setwd(file.path(Sys.getenv("R_USER"), "MELC/CHDS/"))


source("MELC charts.r")
source("MELC outputs.R")
source("Table Builder.R")
source("MELC Ascape.R")
source("MELC scenarios.R")
source("SimenvMELC.R")
#source("SimmoduleMELC1_13.R")
source("SimmoduleMELC1_21.R")
source("StoreBaseRuns.R")
source("MELC UI.r")
source("simulateRun.R")


NUM_ITERATIONS <- 21

initMELC(dirs$base, dirs$models, dirs$PropensityModels, dirs$catToContModels, num.iterations=NUM_ITERATIONS)

#load("base/FullBaseRun.rData") # loads env.base object
#load("base/FullBaseRunNew.rData") # loads env.base object

sfInit(parallel=TRUE, cpus = 4, slaveOutfile = "test.txt" )
	
binbreaks<-binbreaks
models <- models
PropensityModels <- PropensityModels
catToContModels <- catToContModels
check.subgroup.expr <- check.subgroup.expr


sfExport('binbreaks', 'models', 'PropensityModels', 
		'catToContModels','predSimBinomsSelect_notChangeScores', 
		'predSimBinomsSelect','predSimNormsSelect3Models',
		'children', 'NUM_ITERATIONS', 'check.subgroup.expr',
		'dict')
sfExport(list=ls(simarioFun$env), namespace= "simario")
sfLibrary(Hmisc)
sfLibrary(snowfall)
	
.NUMRUNS = 10

startMELC(parallel = TRUE)


saveRDS(env.base, "./base/FullBaseRun.rds")

#save(env.base, file="./base/FullBaseRun.rData") # save env.base object

sfStop()


