# TODO: Add comment
# 
# Author: jtho139
###############################################################################
#typical distributions

#kids
mat <- env.base$modules$years1_13$run_results_collated$confreqs$kids
tmat <- t(mat)
five.plus <- apply(tmat[5:nrow(tmat),], 2, sum)
kidsCat <- rbind(tmat[1:4,], five.plus)
row.names(kidsCat)[5] <- "5+ (%)"

#chres
#"0", "1", "2+"
#binbreaks$chres <- c(-1, 0, 1, max(children$chres, na.rm=TRUE))
mat <- env.base$modules$years1_13$run_results_collated$confreqs$chres
tmat <- t(mat)
matcat <- character(length(binbreaks$chres)-1)
for (i in 1:length(matcat)) {
	matcat[(tmat[,1]>binbreaks$chres[i])&(tmat[,1]<=binbreaks$chres[i+1])]
}

two.plus <- apply(tmat[3:nrow(tmat),], 2, sum)
kidsCat <- rbind(tmat[1:4,], five.plus)
row.names(kidsCat)[5] <- "5+ (%)"





