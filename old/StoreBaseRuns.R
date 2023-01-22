#' Storing Base Runs
#' 
#'At the end of every run, this function is call (inside simulate()) and 1 z dimension (corresponding to
#' the current run) of every array in the cont.outcomes.all.runs list is filled with the matrix of 
#' outcomes.  
#' cont.outcomes.all.runs must alreday exist as a list if empty arrays.  
#' Dimensions: elements of list=variables. 
#' Array dimensions: c(x, y, z) =  c(num children, num iterations (years), num variables)
#' 
#'Store base runs in order to do scenario testing on continuous time-variant variables
#' 
#' @export
#' @examples
#' \dontrun{

#' }
store_cont_outcomes <- function(., run.num) {
	.$cont.outcomes.all.runs$kids[,,run.num] <- .$modules$years1_13$outcomes$kids
	.$cont.outcomes.all.runs$chres[,,run.num] <- .$modules$years1_13$outcomes$chres
	.$cont.outcomes.all.runs$mhrswrk[,,run.num] <- .$modules$years1_13$outcomes$mhrswrk
	.$cont.outcomes.all.runs$fhrswrk[,,run.num] <- .$modules$years1_13$outcomes$fhrswrk
	.$cont.outcomes.all.runs$msmoke[,,run.num] <- .$modules$years1_13$outcomes$msmoke
	.$cont.outcomes.all.runs$fsmoke[,,run.num] <- .$modules$years1_13$outcomes$fsmoke
}





