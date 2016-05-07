mhrs <- numeric(1017)
mhrs[env.scenario$modules$years1_13$outcomes$mhrswrk<20] <- 1

z1ac <- env.scenario$modules$years1_13$outcomes$z1accomLvl1

tab <- table(mhrs, z1ac)
tab[,2]/apply(tab,1,sum)

dim(z1ac)

table(z1ac[,5])/sum(table(z1ac[,5]))

simulate_material_circumstances <- function() {
	z1accomLvl1 <<- predSimBinomsSelect(z1accom_previousLvl1, models$z1accomPrev0, models$z1accomPrev1)
	z1homeownLvl1 <<- adjustCatVar(predSimBinomsSelect(z1homeown_previousLvl1, models$z1homeownPrev0, models$z1homeownPrev1), "z1homeown")
	z1overcrowdLvl1 <<- adjustCatVar(predSimBinomsSelect(z1overcrowd_previousLvl1, models$z1overcrowdPrev0, models$z1overcrowdPrev1), "z1overcrowd")
}

table(simframe.master$z1accomLvl1)

x = 

adjustCatVar2(z1accomLvl1, "z1accomLvl1")

ses1 <- env.scenario$simframe$SESBTHLvl1
table(ses1)
maori <- env.scenario$simframe$r1stchildethnLvl3==1
table(maori)
table(ses1[maori])
ses2 <- env.scenario$simframe$SESBTHLvl2
table(ses2)
table(ses2[maori])
ses3 <-env.scenario$simframe$SESBTHLvl3
table(ses3[maori])



mhrs <- numeric(1017)
mhrs[env.scenario$modules$years1_13$outcomes$mhrswrk<20] <- 1

z1wel <- env.scenario$modules$years1_13$outcomes$welfareLvl1[,2]
table(z1wel)
fhrs <- env.scenario$modules$years1_13$outcomes$fhrswrk[,2]
summary(fhrs)
fhrs.cat <- bin(fhrs, binbreaks$fhrswrk)
table(fhrs.cat)
round(table(fhrs.cat)/sum(table(fhrs.cat)), 3)
table(fhrs.cat[z1wel==1])
round(table(fhrs.cat[z1wel==1])/sum(table(fhrs.cat[z1wel==1])), 3)

z1wel <- env.scenario$modules$years1_13$outcomes$welfareLvl1[,1]
table(z1wel)
fhrs <- env.scenario$modules$years1_13$outcomes$fhrswrk[,1]
summary(fhrs)
fhrs.cat <- bin(fhrs, binbreaks$fhrswrk)
table(fhrs.cat)
round(table(fhrs.cat)/sum(table(fhrs.cat)), 3)
table(fhrs.cat[z1wel==1])
round(table(fhrs.cat[z1wel==1])/sum(table(fhrs.cat[z1wel==1])), 3)

pac <- env.scenario$simframe$r1stchildethnLvl2
table(pac)
fhrs <- env.scenario$modules$years1_13$outcomes$fhrswrk[,2]
summary(fhrs)
fhrs.cat <- bin(fhrs, binbreaks$fhrswrk)
table(fhrs.cat)
round(table(fhrs.cat)/sum(table(fhrs.cat)), 3)
table(fhrs.cat[pac==1])
round(table(fhrs.cat[pac==1])/sum(table(fhrs.cat[pac==1])), 3)

maori <- env.scenario$simframe$r1stchildethnLvl3
table(maori)
mhrs <- env.scenario$modules$years1_13$outcomes$mhrswrk[,2]
summary(mhrs)
mhrs.cat <- bin(mhrs, binbreaks$mhrswrk)
table(mhrs.cat)
round(table(mhrs.cat)/sum(table(mhrs.cat)), 3)
table(mhrs.cat[maori==1])
round(table(mhrs.cat[maori==1])/sum(table(mhrs.cat[maori==1])), 3)

fhrs=env.scenario$modules$years1_13$outcomes$fhrswrk[,2]
table(bin(fhrs, binbreaks$fhrswrk))/sum(table(bin(fhrs, binbreaks$fhrswrk)))
x = env.scenario$modules$years1_13$outcomes$chres[,3]
table(bin(x, binbreaks$chres))/sum(table(bin(x, binbreaks$chres)))

fhrs=env.scenario$modules$years1_13$outcomes$fhrswrk[,1]
table(bin(fhrs, binbreaks$fhrswrk))/sum(table(bin(fhrs, binbreaks$fhrswrk)))

maori <- env.scenario$simframe$r1stchildethnLvl3
table(maori)
fhrs <- env.scenario$modules$years1_13$outcomes$fhrswrk[,1]
summary(fhrs)
fhrs.cat <- bin(fhrs, binbreaks$fhrswrk)
table(fhrs.cat)
round(table(fhrs.cat)/sum(table(fhrs.cat)), 3)
table(fhrs.cat[maori==1])
round(table(fhrs.cat[maori==1])/sum(table(fhrs.cat[maori==1])), 3)

mhrs = env.scenario$modules$years1_13$outcomes$mhrswrk[,1]
z1.mhrs <- numeric(length(mhrs))
z1.mhrs[mhrs<20] <- 1
table(z1.mhrs)
summary(mhrs)
z1ac <- env.scenario$modules$years1_13$outcomes$z1accomLvl1[,1]
table(z1ac)
table(z1ac[z1.mhrs==1])/sum(table(z1ac[z1.mhrs==1]))

mhrs = env.scenario$modules$years1_13$outcomes$mhrswrk[,4]
z1.mhrs <- numeric(length(mhrs))
z1.mhrs[mhrs<20] <- 1
table(z1.mhrs)
summary(mhrs)
z1ac <- env.scenario$modules$years1_13$outcomes$z1accomLvl1[,4]
table(z1ac)
table(z1ac[z1.mhrs==1])/sum(table(z1ac[z1.mhrs==1]))

mhrs = env.scenario$modules$years1_13$outcomes$mhrswrk[,1]
z1.mhrs <- numeric(length(mhrs))
z1.mhrs[mhrs<20] <- 1
table(z1.mhrs)
summary(mhrs)
fhrs <- env.scenario$modules$years1_13$outcomes$fhrswrk[,1]
summary(fhrs)
fhrs.cat <- bin(fhrs, binbreaks$fhrswrk)
table(fhrs.cat[z1.mhrs==1])/sum(table(fhrs.cat[z1.mhrs==1]))

mhrs = env.scenario$modules$years1_13$outcomes$mhrswrk[,3]
z1.mhrs <- numeric(length(mhrs))
z1.mhrs[mhrs<20] <- 1
table(z1.mhrs)
summary(mhrs)
fhrs <- env.scenario$modules$years1_13$outcomes$fhrswrk[,3]
summary(fhrs)
fhrs.cat <- bin(fhrs, binbreaks$fhrswrk)
table(fhrs.cat[z1.mhrs==1])/sum(table(fhrs.cat[z1.mhrs==1]))

mhrs = env.scenario$modules$years1_13$outcomes$mhrswrk[,1]
z1.mhrs <- numeric(length(mhrs))
z1.mhrs[mhrs<20] <- 1
table(z1.mhrs)
summary(mhrs)
ses1 <- env.scenario$simframe$SESBTHLvl1
table(ses1[z1.mhrs==1])/sum(table(ses1[z1.mhrs==1]))
ses2 <- env.scenario$simframe$SESBTHLvl2
ses3 <-env.scenario$simframe$SESBTHLvl3
table(ses2[z1.mhrs==1])/sum(table(ses2[z1.mhrs==1]))
table(ses3[z1.mhrs==1])/sum(table(ses3[z1.mhrs==1]))

z1wel <- env.scenario$modules$years1_13$outcomes$welfareLvl1[,4]
table(z1wel)
z1ac <- env.scenario$modules$years1_13$outcomes$z1accomLvl1[,4]
table(z1ac)
table(z1ac[z1wel==1])/sum(table(z1ac[z1wel==1]))

a=13
z1mhrs1=rep(NA, 1017)
 z1mhrs1[env.scenario$modules$years1_13$outcomes$mhrswrk[,a]==0]<-0
 z1mhrs1[env.scenario$modules$years1_13$outcomes$mhrswrk[,a]>0]<-1
 t=table(z1mhrs1)
 round(t/sum(t), 3)
 
 a=13
 mhrs = env.scenario$modules$years1_13$outcomes$mhrswrk[,a]
 hist(mhrs, freq=F)
 
 mhrs = env.scenario$modules$years1_13$outcomes$mhrswrk[,1]
 table(bin(mhrs, binbreaks$mhrswrk))/sum(table(bin(mhrs, binbreaks$mhrswrk)))
 
 chr = env.scenario$modules$years1_13$outcomes$chres[,3]
 table(bin(chr, binbreaks$chres))/sum(table(bin(chr, binbreaks$chres)))


 dg=env.base$modules$years1_13$outcomes$dadgroup
 for (i in 1:13) {
 	print(round(table(dg[,i])/sum(table(dg[,i])), 2))
 }
 
 mg=env.base$modules$years1_13$outcomes$mumgroup
 sing=env.base$modules$years1_13$outcomes$z1single
 
 row.tots <- table(sub.df$age[!is.na(sub.df$dadgroup)])
 tab <- table(sub.df$age, sub.df$dadgroup)
 tab2 <- tab
 for (i in 1:13) {
	 tab2[i,] <- tab[i,]/row.tots[i]
 }
 round(tab2, 2)
 

 for (i in 1:13) {
	 print(i)
 	print(round(table(env.base$modules$years1_13$outcomes$dadgroup[,i])/sum( table(env.base$modules$years1_13$outcomes$dadgroup[,i])), 2))
 }
 
 #validating dadgroup
#get a table with the 0 (no dad) group too (the function currently used removes the 0 group)
dg.tab <- env.base$modules$years1_13$run_results_collated$freqs$dadgroup
dg.tab0 <- apply(dg.tab, 1, function(x) {100 - sum(x)})
dg.tab2 <- cbind(dg.tab0, dg.tab)

#save to an excel file
write.csv(dg.tab2, "h:/COMPASS/MelC/fsmoke2stage/simulatedDadgroupByAge.csv", row.names=F)

fsmoke.cat <- bin(env.base$modules$years1_13$outcomes$fsmoke, binbreaks$fsmoke)
tab <- array(dim=c(13,4,4))
pct.tab <- tab
for (d in 1:4) {
	for (a in 1:13) {
		tab[a,,d] <- table(fsmoke.cat[,a][env.base$modules$years1_13$outcomes$dadgroup[,a]==(d-1)])
		pct.tab[a,,d] <- round((tab[a,,d]/sum(tab[a,,d]))*100, 2) 
	}
}

table(fsmoke.cat[,4][env.base$modules$years1_13$outcomes$dadgroup[,4]==2])

hist(env.base$modules$years1_13$outcomes$fsmoke[,1][env.base$modules$years1_13$outcomes$dadgroup[,1]==1])

hist(env.base$modules$years1_13$outcomes$fsmoke[env.base$modules$years1_13$outcomes$dadgroup==0], freq=F, main="Fsmoke Distribution for Dadgroup 0", xlab="Number of cigarettes smoked per day")
hist(env.base$modules$years1_13$outcomes$fsmoke[env.base$modules$years1_13$outcomes$dadgroup==1], freq=F, main="Fsmoke Distribution for Dadgroup 1", xlab="Number of cigarettes smoked per day")
grid()
hist(env.base$modules$years1_13$outcomes$fsmoke[env.base$modules$years1_13$outcomes$dadgroup==2], freq=F, main="Fsmoke Distribution for Dadgroup 2", xlab="Number of cigarettes smoked per day")
grid()
hist(env.base$modules$years1_13$outcomes$fsmoke[env.base$modules$years1_13$outcomes$dadgroup==3], freq=F, main="Fsmoke Distribution for Dadgroup 3", xlab="Number of cigarettes smoked per day")
grid()

#msmoke histograms by mumgroup
hist(env.base$modules$years1_13$outcomes$msmoke[env.base$modules$years1_13$outcomes$mumgroup==1], freq=F, main="Msmoke Distribution for Mumgroup 1", xlab="Number of cigarettes smoked per day")
grid()
hist(env.base$modules$years1_13$outcomes$msmoke[env.base$modules$years1_13$outcomes$mumgroup==2], freq=F, main="Msmoke Distribution for Mumgroup 2", xlab="Number of cigarettes smoked per day")
grid()
hist(env.base$modules$years1_13$outcomes$msmoke[env.base$modules$years1_13$outcomes$mumgroup==3], freq=F, main="Msmoke Distribution for Mumgroup 3", xlab="Number of cigarettes smoked per day")
grid()


msmoke.cat <- bin(env.base$modules$years1_13$outcomes$msmoke, binbreaks$msmoke)
tab <- array(dim=c(13,4,4))
pct.tab <- tab
for (d in 1:4) {
	for (a in 1:13) {
		tab[a,,d] <- table(msmoke.cat[,a][env.base$modules$years1_13$outcomes$mumgroup[,a]==(d-1)])
		pct.tab[a,,d] <- round((tab[a,,d]/sum(tab[a,,d]))*100, 2) 
	}
}

colnames(mx.flattened)<-c(rep("1 0", 3), rep("1 1", 3), rep("2 0", 3), rep("2 1", 3), rep("3 0", 3), rep("3 1", 3))

x.c <- "1 0 Mean"
rs <- str_locate_all(x.c, " ")
n <- length(rs[[1]])
end <- rs[[1]][n] - 1
str_sub(x.c, 1, end)

x.c.v <- c("1 0 Mean",  "1 0 Lower", "1 0 Upper", "1 1 Mean",  "1 1 Lower", "1 1 Upper",
	"2 0 Mean",  "2 0 Lower", "2 0 Upper", "2 1 Mean",  "2 1 Lower", "2 1 Upper",
	"3 0 Mean",  "3 0 Lower", "3 0 Upper", "3 1 Mean",  "3 1 Lower", "3 1 Upper")
rs <- str_locate_all(x.c.v, " ")
n <- lapply(rs, function(x) {length(x)})
n <- n[[1]]
end <- lapply(rs, function(x) {x[n] - 1})
end <- end[[1]]
result <- tapply(x.c.v, 1:length(x.c.v), function(x) {str_sub(x, 1, end)})

#simplifying the above
x.c.v <- c("1 0 Mean",  "1 0 Lower", "1 0 Upper", "1 1 Mean",  "1 1 Lower", "1 1 Upper",
		"2 0 Mean",  "2 0 Lower", "2 0 Upper", "2 1 Mean",  "2 1 Lower", "2 1 Upper",
		"3 0 Mean",  "3 0 Lower", "3 0 Upper", "3 1 Mean",  "3 1 Lower", "3 1 Upper")
rs <- str_locate_all(x.c.v, " ")
n <- length(rs[[1]])
end <- rs[[1]][n] - 1
result <- tapply(x.c.v, 1:length(x.c.v), function(x) {str_sub(x, 1, end)})

#code used to label the Mean, Lower, and Upper in collated means
# reorder resultCI so that lower and upper is next to the mean of each grouping
numGroups <- dim(xa)[COL]
reordering <- as.vector(sapply(c(1:numGroups), function (x) { seq(from=x, length.out=3, by=numGroups)}))
resultCI <- resultCI[, reordering]

colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
names(dimnames(resultCI)) <- names(dimnames(result))

table(fage_years)
summary(fage_years[fage_years!=99])

table(mage_years)
summary(mage_years[mage_years!=99])

is(children)
table(children$pregsmk)
pregsmk.cat <- character(length(children$pregsmk))
pregsmk.cat[children$pregsmk==0]<-"0"
pregsmk.cat[(children$pregsmk>=1)&(children$pregsmk<6)]<-"1-5"
pregsmk.cat[(children$pregsmk>=6)&(children$pregsmk<11)]<-"6-10"
pregsmk.cat[(children$pregsmk>=11)&(children$pregsmk<16)]<-"11-15"
pregsmk.cat[(children$pregsmk>=16)&(children$pregsmk<21)]<-"16-20"
pregsmk.cat[children$pregsmk>=21]<-">20"
table(pregsmk.cat)
table(pregsmk.cat)/sum(table(pregsmk.cat))

ga.cat <- character(length(children$ga))
ga.cat[children$ga<35]<-"<35"
ga.cat[children$ga==35]<-"35"
ga.cat[children$ga==36]<-"36"
ga.cat[(children$ga>36)&(children$ga<=42)]<-"37-42"
ga.cat[children$ga>42]<-"43+"
table(ga.cat)
table(ga.cat)/sum(table(ga.cat))

pa<-env.scenario$simframe$pregalc
table(bin(pa, binbreaks$pregalc))

x<-env.scenario$simframe$bwkg
table(bin(x, binbreaks$bwkg))

x<-env.scenario$simframe$ga
table(bin(x, binbreaks$ga))

x<-env.scenario$simframe$BREAST
table(bin(x, binbreaks$BREAST))
table(bin(x, binbreaks$BREAST))/sum(table(bin(x, binbreaks$BREAST)))

x<-env.base$simframe$pregsmk
table(bin(x, binbreaks$pregsmk))/sum(table(bin(x, binbreaks$pregsmk)))

x<-env.base$simframe$pregalc
table(bin(x, binbreaks$pregalc))/sum(table(bin(x, binbreaks$pregalc)))

summary(env.base$simframe$PUNISH)

round(apply(outcomes$hadmtot, 2, mean), 2)

x<-env.base$modules$years1_13$outcomes$hadmtot
hist(x[,10])

test<-tableBuilder("Base", "frequencies", "welfareLvl1", "SESBTH")
y<-env.base$modules$years1_13$outcomes$welfareLvl1
gp<-children$SESBTH
tab<-table(y[,2], gp)
col.sums<-apply(tab, 2, sum)
tab[2,]/col.sums*100

y <- env.base$modules$years1_13$run_results$run1$outcomes$fhrswrk
gp <- children$SESBTH
mean(y[,1])
mean(y[,2][gp==3])

 test <- tableBuilder2("Base", "frequencies", "welfareLvl1", "SESBTH", CI=TRUE)
 test <- tableBuilder2("Base", "frequencies", "welfareLvl1", "", CI=TRUE)
 test <- tableBuilder2("Base", "frequencies", "welfareLvl1", "z1overcrowdLvl1", CI=TRUE)
 test <- tableBuilder2("Base", "means", "welfareLvl1", "SESBTH", CI=TRUE)
 test <- tableBuilder2("Base", "means", "welfareLvl1", "", CI=TRUE)
 test <- tableBuilder2("Base", "means", "welfareLvl1", "z1overcrowdLvl1", CI=TRUE)
 test <- tableBuilder2("Base", "quintiles", "welfareLvl1", "SESBTH", CI=TRUE)
 test <- tableBuilder2("Base", "quintiles", "welfareLvl1", "", CI=TRUE)
 test <- tableBuilder2("Base", "quintiles", "welfareLvl1", "z1overcrowdLvl1", CI=TRUE)

 test <- tableBuilder2("Base", "frequencies", "fhrswrk", "SESBTH", CI=FALSE)
 test <- tableBuilder2("Base", "frequencies", "fhrswrk", "", CI=FALSE)
 test <- tableBuilder2("Base", "frequencies", "fhrswrk", "z1overcrowdLvl1", CI=FALSE)
 test <- tableBuilder2("Base", "means", "fhrswrk", "SESBTH", CI=FALSE)
 test <- tableBuilder2("Base", "means", "fhrswrk", "", CI=FALSE)
 test <- tableBuilder2("Base", "means", "fhrswrk", "z1overcrowdLvl1", CI=FALSE)
 test <- tableBuilder2("Base", "quintiles", "fhrswrk", "SESBTH", CI=FALSE)
 test <- tableBuilder2("Base", "quintiles", "fhrswrk", "", CI=FALSE)
 test <- tableBuilder2("Base", "quintiles", "fhrswrk", "z1overcrowdLvl1", CI=FALSE)

 #currently cannot do freqs for final outcomes (can only do freqs for those vars with binbreaks)
 # test <- tableBuilder2("Base", "frequencies", "gptotvis", "SESBTH", CI=FALSE)
 #test <- tableBuilder2("Base", "frequencies", "gptotvis", "", CI=FALSE)
 #test <- tableBuilder2("Base", "frequencies", "gptotvis", "z1overcrowdLvl1", CI=FALSE)
 test <- tableBuilder2("Base", "means", "gptotvis", "SESBTH", CI=FALSE)
 test <- tableBuilder2("Base", "means", "gptotvis", "", CI=FALSE)
 test <- tableBuilder2("Base", "means", "gptotvis", "z1overcrowdLvl1", CI=FALSE)
 test <- tableBuilder2("Base", "quintiles", "gptotvis", "SESBTH", CI=FALSE)
 test <- tableBuilder2("Base", "quintiles", "gptotvis", "", CI=FALSE)
 test <- tableBuilder2("Base", "quintiles", "gptotvis", "z1overcrowdLvl1", CI=FALSE)
 
 test <- tableBuilder2("Base", "means", "gptotvis", "SESBTH", CI=TRUE)
 test <- tableBuilder2("Base", "means", "gptotvis", "", CI=TRUE)
 test <- tableBuilder2("Base", "means", "gptotvis", "z1overcrowdLvl1", CI=TRUE)
 test <- tableBuilder2("Base", "quintiles", "gptotvis", "SESBTH", CI=TRUE)
 test <- tableBuilder2("Base", "quintiles", "gptotvis", "", CI=TRUE)
 test <- tableBuilder2("Base", "quintiles", "gptotvis", "z1overcrowdLvl1", CI=TRUE)
 
 mhrs<-env.scenario$modules$years1_13$outcomes$mhrswrk
 sing<-env.scenario$modules$years1_13$outcomes$z1singleLvl1
 mhrsLt20Y1<-mhrs[,1]<20
 table(mhrsLt20Y1)
 singY1<-sing[,1]
 table(singY1, mhrsLt20Y1)
 
 welf<-env.base$modules$years1_13$outcomes$welfareLvl1
 mhrs<-env.base$modules$years1_13$outcomes$mhrswrk
 a=13
mhrs.dichot<-mhrs[,a]<20
table(mhrs.dichot)
table(welf[,a], mhrs.dichot)

mhrs<-env.base$modules$years1_13$outcomes$mhrswrk
fhrs<-env.base$modules$years1_13$outcomes$fhrswrk
a=3
mhrs.dichot<-mhrs[,a]<20
mean(fhrs[mhrs.dichot,a], na.rm=T)

mhrs<-env.base$modules$years1_13$outcomes$mhrswrk
a=1
mhrs.dichot<-mhrs[,a]<20
welf<-env.base$modules$years1_13$outcomes$welfareLvl1[,a]
ses<-children$SESBTH
welf.sub<-welf[mhrs.dichot]
ses.sub<-ses[mhrs.dichot]
tapply(welf.sub, ses.sub, mean)

eth<-children$r1stchildethn

table(ses, eth)

welf<-run_results$run1$outcomes$welfareLvl1[,1]
table(welf, sg.var)y

quantile(fhrs[,a][mhrs.dichot], probs<-c(0,.1,.25,.5,.75,.9,1))

a=1
sing<-env.base$modules$years1_13$outcomes$z1singleLvl1[,a]
fhrs<-env.base$modules$years1_13$outcomes$fhrswrk[,a]
tapply(fhrs, sing, mean)

a=10
fhrs.run1<-env.base$modules$years1_13$run_results$run1$outcomes$fhrswrk[,a]
sing.run1<-env.base$modules$years1_13$run_results$run1$outcomes$z1singleLvl1[,a]
tapply(fhrs.run1, sing.run1, mean)
sing.id<-which(sing.run1==1)
length(which(fhrs.run1[sing.id]==0))/length(fhrs.run1[sing.id])

a=10
fhrs.run2<-env.base$modules$years1_13$run_results$run2$outcomes$fhrswrk[,a]
sing.run2<-env.base$modules$years1_13$run_results$run2$outcomes$z1singleLvl1[,a]
tapply(fhrs.run2, sing.run2, mean)
sing.id<-which(sing.run2==1)
length(which(fhrs.run2[sing.id]==0))/length(fhrs.run2[sing.id])


mhrs <-env.scenario$modules$years1_13$outcomes$mhrswrk
single <- env.scenario$modules$years1_13$outcomes$z1singleLvl1
sg <- mhrs<21
table(sg[,1], sg[,1])
tapply(mhrs[,1], sg[,1], mean)
tapply(single[,1], sg[,1], mean)
tapply(single[,2], sg[,2], mean)

mhrs <-env.base$modules$years1_13$outcomes$mhrswrk
single <- env.base$modules$years1_13$outcomes$z1singleLvl1
sg <- mhrs<21
table(sg[,1], sg[,1])
tapply(mhrs[,1], sg[,1], mean)
tapply(single[,1], sg[,1], mean)
tapply(single[,2], sg[,2], mean)

 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="bwkg", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="bwkg", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="bwkg", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="bwkg", grpbyName="", CI=TRUE, logisetexpr="mhrswrk<21", dict=dict.MELC, not.in.subgroup=T)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="bwkg", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="bwkg", grpbyName="SESBTH", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="bwkg", grpbyName="welfareLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="bwkg", grpbyName="welfareLvl1", CI=FALSE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)

 #variableName=pregalc
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="pregalc", grpbyName="SESBTH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr="mhrswrk<21", dict=dict.MELC, not.in.subgroup=T)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="pregalc", grpbyName="SESBTH", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="pregalc", grpbyName="welfareLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="pregalc", grpbyName="welfareLvl1", CI=FALSE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)

 test <- tableBuilder(envName="Base", statistic="means", variableName="pregalc", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr="mhrswrk<21", dict=dict.MELC, not.in.subgroup=T)
 test <- tableBuilder(envName="Base", statistic="means", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="pregalc", grpbyName="SESBTH", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="pregalc", grpbyName="welfareLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="pregalc", grpbyName="welfareLvl1", CI=FALSE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)

 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="pregalc", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr="mhrswrk<21", dict=dict.MELC, not.in.subgroup=T)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="pregalc", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="pregalc", grpbyName="SESBTH", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="pregalc", grpbyName="welfareLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="pregalc", grpbyName="welfareLvl1", CI=FALSE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 
 #variableName=SESBTH
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="SESBTH", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr="mhrswrk<21", dict=dict.MELC, not.in.subgroup=T)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="SESBTH", grpbyName="r1stmeduc", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="SESBTH", grpbyName="welfareLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="SESBTH", grpbyName="welfareLvl1", CI=FALSE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 
 test <- tableBuilder(envName="Base", statistic="means", variableName="SESBTH", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr="mhrswrk<21", dict=dict.MELC, not.in.subgroup=T)
 test <- tableBuilder(envName="Base", statistic="means", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="SESBTH", grpbyName="r1stmeduc", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="SESBTH", grpbyName="welfareLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="SESBTH", grpbyName="welfareLvl1", CI=FALSE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="SESBTH", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr="mhrswrk<21", dict=dict.MELC, not.in.subgroup=T)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="SESBTH", grpbyName="", CI=TRUE, logisetexpr="r1stchildethnLvl3==1", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="SESBTH", grpbyName="r1stmeduc", CI=TRUE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="SESBTH", grpbyName="welfareLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="quintiles", variableName="SESBTH", grpbyName="welfareLvl1", CI=FALSE, logisetexpr="r1stchildethnLvl3==1 & mhrswrk<21", dict=dict.MELC)
 
 x <- env.scenario$simframe$pregsmk
 x2 <- rep(NA, length(x))
 x2[x==0] <- '0'
 x2[x>=1 & x<=5] <- '1-5'
 x2[x>=6 & x<=10] <- '6-10'
 x2[x>=11 & x<=15] <- '11-15'
 x2[x>=16 & x<=20] <- '16-20'
 x2[x>20] <- '21+'
 table(x2)/sum(table(x2))
 welf <- env.scenario$simframe$welfareLvl1
 table(x2[welf==1])/sum(table(x2[welf==1]))
 
 
 #FINAL SCENARIO TESTING
 env.scenario <<- SimenvMELC$new()
 env.scenario$cat.adjustments$z1single[1,] <- c(0.3, 0.7)
 env.scenario$simulate(1)
 
 env.scenario <<- SimenvMELC$new()
 env.scenario$cat.adjustments$z1single[1,] <- c(0, 1)
 env.scenario$simulate(1)
 
 test <- table_mx_cols_MELC(mx, grpby=grpbymx, grpby.tag=grpbyName, logiset=logiset, dict=dict)
 
 #Z1COND TESTS
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1condLvl1", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1condLvl1", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1condLvl1", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1condLvl1", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 ##test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1condLvl1", grpbyName="fsmoke", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
#tableBuilder doesn't allow subgrouping by a continuous variable
 
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="mhrswrk", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1singleLvl1", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="welfareLvl1", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="r1stchildethn", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="r1stchildethn", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="r1stchildethn", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="r1stmeduc", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="ga", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1genderLvl1", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1genderLvl1", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="kids", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
#tried all with CI=FALSE
 
#NPRESCH TESTS
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="NPRESCH", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="NPRESCH", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="NPRESCH", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="NPRESCH", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1singleLvl1", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="welfareLvl1", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1genderLvl1", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="r1stmeduc", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="r1stchildethn", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="mhrswrk", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="ga", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="kids", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 
 
 #
test <- tableBuilder(envName="Base", statistic="means", variableName="z1condLvl1", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="z1condLvl1", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
##test <- tableBuilder(envName="Base", statistic="means", variableName="z1condLvl1", grpbyName="fsmoke", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
#tableBuilder doesn't allow subgrouping by a continuous variable

test <- tableBuilder(envName="Base", statistic="means", variableName="mhrswrk", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="z1singleLvl1", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="welfareLvl1", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
#doesn't make sense to do the mean of child ethnicity - though these three do work
#test <- tableBuilder(envName="Base", statistic="means", variableName="r1stchildethn", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
#test <- tableBuilder(envName="Base", statistic="means", variableName="r1stchildethn", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
#test <- tableBuilder(envName="Base", statistic="means", variableName="r1stchildethn", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="r1stmeduc", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="ga", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="z1genderLvl1", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="z1genderLvl1", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="kids", grpbyName="z1condLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)


#
test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)

test <- tableBuilder(envName="Base", statistic="means", variableName="z1singleLvl1", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="welfareLvl1", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="z1genderLvl1", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="r1stmeduc", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="r1stchildethn", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="mhrswrk", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="ga", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="kids", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)

test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="z1singleLvl1", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="SESBTH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="r1stchildethn", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)

test <- tableBuilder(envName="Base", statistic="means", variableName="z1singleLvl1", grpbyName="NPRESCH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="welfareLvl1", grpbyName="NPRESCH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="z1genderLvl1", grpbyName="NPRESCH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="r1stmeduc", grpbyName="NPRESCH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="r1stchildethn", grpbyName="NPRESCH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="mhrswrk", grpbyName="NPRESCH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="ga", grpbyName="NPRESCH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="kids", grpbyName="NPRESCH", CI=FALSE, logisetexpr=NULL, dict=dict.MELC)


env.scenario <<- SimenvMELC$new()
subgroupExpression <- "welfareLvl1==1 & fsmoke>10"
subgroupExpression <- "welfareLvl1==1"
setGlobalSubgroupFilterExpression(subgroupExpression)
env.scenario$cat.adjustments$z1single[1,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1single[5,] <- c(0.2,0.8)
env.scenario$cat.adjustments$kids[1,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$kids[5,] <- c(rep(1/5, 5))
env.scenario$simulate(1)
 env.scenario$simulate(2)
 
 summary(env.scenario$simframe$kids)
 table(env.scenario$simframe$kids)
 
 #matrices
 v <- env.scenario$modules$years1_13$outcomes$kids
 welf <- env.scenario$modules$years1_13$outcomes$welfareLvl1
 fsmk <- env.scenario$modules$years1_13$outcomes$fsmoke
 #yr1
welf1 <- welf[,1]
smk1 <- fsmk[,1]
k1 <- v[,1]
ksub <- k1[welf1==1 & smk1>10]
table(ksub)

#yr5
welf5 <- welf[,5]
smk5 <- fsmk[,5]
k5 <- v[,5]
ksub5 <- k5[welf5==1 & smk5>10]
table(ksub5)
length(ksub5)
subgroupExpression <- "welfareLvl1==1 & fsmoke>10"

.NUMRUNS <- 1
doSim.Base()
env.scenario <<- SimenvMELC$new()
#subgroupExpression <- "welfareLvl1==1 & fsmoke>10"
#subgroupExpression <- "welfareLvl1==1"
#setGlobalSubgroupFilterExpression(subgroupExpression)
env.scenario$cat.adjustments$z1single[1,] <- c(0,1)
env.scenario$cat.adjustments$z1single[5,] <- c(0,1)
env.scenario$cat.adjustments$kids[1,] <- c(.5, .5, 0, 0, 0)
env.scenario$cat.adjustments$kids[5,] <- c(.5, .5, 0, 0, 0)
env.scenario$simulate(1)
#env.scenario$simulate(2)

#systematic test of all variables
.NUMRUNS <- 2
#doSim.Base()
env.scenario <<- SimenvMELC$new()
subgroupExpression <- "z1homeownLvl1==1 & msmoke>10"
#subgroupExpression <- "welfareLvl1==1"
#subgroupExpression <- "r1stchildethnLvl2==1"
setGlobalSubgroupFilterExpression(subgroupExpression)
env.scenario$cat.adjustments$z1single[1,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1single[5,] <- c(0.2,0.8)
env.scenario$cat.adjustments$kids[1,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$kids[5,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$z1accom[2,] <- c(0.9,0.1)
env.scenario$cat.adjustments$z1accom[6,] <- c(0.1,0.9)
env.scenario$cat.adjustments$fhrswrk[1,] <- c(rep(1/7, 7))
env.scenario$cat.adjustments$fhrswrk[7,] <- c(rep(1/7, 7))
#env.scenario$simulate(1)
env.scenario$simulate(2)

.NUMRUNS <- 2
#doSim.Base()
env.scenario <<- SimenvMELC$new()
subgroupExpression <- "z1singleLvl1==1 & msmoke>10"
#subgroupExpression <- "welfareLvl1==1"
setGlobalSubgroupFilterExpression(subgroupExpression)
env.scenario$cat.adjustments$welfare[1,] <- c(0.2,0.8)
env.scenario$cat.adjustments$welfare[5,] <- c(0.2,0.8)
env.scenario$cat.adjustments$welfare[6,] <- c(0.2,0.8)
env.scenario$cat.adjustments$welfare[8,] <- c(0.2,0.8)
env.scenario$cat.adjustments$welfare[10,] <- c(0.2,0.8)
#env.scenario$simulate(1)
env.scenario$simulate(2)

.NUMRUNS <- 2
#doSim.Base()
env.scenario <<- SimenvMELC$new()
#subgroupExpression <- "ga<40 & r1stmeducLvl2==1"
subgroupExpression <- "welfareLvl1==1"
setGlobalSubgroupFilterExpression(subgroupExpression)
env.scenario$cat.adjustments$z1homeown[1,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1homeown[2,] <- c(0.2,0.8)
#env.scenario$cat.adjustments$welfare[6,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1homeown[8,] <- c(0.2,0.8)
#env.scenario$cat.adjustments$welfare[10,] <- c(0.2,0.8)
#env.scenario$simulate(1)
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$z1accom[1,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1accom[5,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1accom[6,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1accom[8,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1accom[10,] <- c(0.2,0.8)
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$z1homeown[1,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1homeown[5,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1homeown[6,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1homeown[8,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1homeown[10,] <- c(0.2,0.8)
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$z1overcrowd[1,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1overcrowd[5,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1overcrowd[6,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1overcrowd[8,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1overcrowd[10,] <- c(0.2,0.8)
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$SESBTH[1,] <- c(rep(1/3, 3))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$pregsmk[1,] <- c(rep(1/6, 6))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$pregalc[1,] <- c(rep(1/9, 9))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$bwkg[1,] <- c(rep(1/5, 5))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$ga[1,] <- c(rep(1/5, 5))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$r1stmeduc[1,] <- c(rep(1/3, 3))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$r1stfeduc[1,] <- c(rep(1/3, 3))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$fage[1,] <- c(rep(1/6, 6))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$BREAST[1,] <- c(rep(1/13, 13))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$MAGE[1,] <- c(rep(1/6, 6))
env.scenario$simulate(2)

.NUMRUNS <- 1
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$fhrswrk[1,] <- c(rep(1/7, 7))
env.scenario$cat.adjustments$fhrswrk[5,] <- c(rep(1/7, 7))
env.scenario$cat.adjustments$fhrswrk[6,] <- c(rep(1/7, 7))
env.scenario$cat.adjustments$fhrswrk[8,] <- c(rep(1/7, 7))
env.scenario$cat.adjustments$fhrswrk[10,] <- c(rep(1/7, 7))
env.scenario$simulate(1)

.NUMRUNS <- 1
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$fhrswrk[1,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[2,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[3,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[4,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[5,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[6,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[7,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[8,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[9,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[10,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[11,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[12,] <- c(0.05, rep(.95/6, 6))
env.scenario$cat.adjustments$fhrswrk[13,] <- c(0.05, rep(.95/6, 6))
env.scenario$simulate(1)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$mhrswrk[1,] <- c(rep(1/6, 6))
env.scenario$cat.adjustments$mhrswrk[5,] <- c(rep(1/6, 6))
env.scenario$cat.adjustments$mhrswrk[6,] <- c(rep(1/6, 6))
env.scenario$cat.adjustments$mhrswrk[8,] <- c(rep(1/6, 6))
env.scenario$cat.adjustments$mhrswrk[10,] <- c(rep(1/6, 6))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$fsmoke[1,] <- c(rep(1/4, 4))
env.scenario$cat.adjustments$fsmoke[5,] <- c(rep(1/4, 4))
env.scenario$cat.adjustments$fsmoke[6,] <- c(rep(1/4, 4))
env.scenario$cat.adjustments$fsmoke[8,] <- c(rep(1/4, 4))
env.scenario$cat.adjustments$fsmoke[10,] <- c(rep(1/4, 4))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$msmoke[1,] <- c(rep(1/4, 4))
env.scenario$cat.adjustments$msmoke[5,] <- c(rep(1/4, 4))
env.scenario$cat.adjustments$msmoke[6,] <- c(rep(1/4, 4))
env.scenario$cat.adjustments$msmoke[8,] <- c(rep(1/4, 4))
env.scenario$cat.adjustments$msmoke[10,] <- c(rep(1/4, 4))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$kids[1,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$kids[5,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$kids[6,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$kids[8,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$kids[10,] <- c(rep(1/5, 5))
env.scenario$simulate(2)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$householdsize[1,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$householdsize[5,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$householdsize[6,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$householdsize[8,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$householdsize[10,] <- c(rep(1/5, 5))
env.scenario$simulate(2)

.NUMRUNS <- 1
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$householdsize[1,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$householdsize[5,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$householdsize[6,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$householdsize[8,] <- c(rep(1/5, 5))
env.scenario$cat.adjustments$householdsize[10,] <- c(rep(1/5, 5))
env.scenario$simulate(1)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$chres[1,] <- c(rep(1/3, 3))
env.scenario$cat.adjustments$chres[5,] <- c(rep(1/3, 3))
env.scenario$cat.adjustments$chres[6,] <- c(rep(1/3, 3))
env.scenario$cat.adjustments$chres[8,] <- c(rep(1/3, 3))
env.scenario$cat.adjustments$chres[10,] <- c(rep(1/3, 3))
env.scenario$simulate(2)

.NUMRUNS <- 1
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$z1cond[5,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1cond[6,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1cond[8,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1cond[10,] <- c(0.2,0.8)
env.scenario$simulate(1)


.NUMRUNS <- 1
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$INTERACT[1,] <- c(rep(1/9, 9))
env.scenario$simulate(1)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$NPRESCH[1,] <- c(.01, .1, .01, .88)
env.scenario$simulate(2)

.NUMRUNS <- 1
env.scenario <<- SimenvMELC$new()
env.scenario$cat.adjustments$PUNISH[1,] <- c(rep(1/6, 6))
env.scenario$simulate(1)

.NUMRUNS <- 2
#doSim.Base()
env.scenario <<- SimenvMELC$new()
subgroupExpression <- "ga<40 & r1stmeducLvl2==1"
#subgroupExpression <- "welfareLvl1==1"
setGlobalSubgroupFilterExpression(subgroupExpression)
env.scenario$cat.adjustments$z1accom[1,] <- c(0.2,0.8)
#env.scenario$cat.adjustments$welfare[5,] <- c(0.2,0.8)
#env.scenario$cat.adjustments$welfare[6,] <- c(0.2,0.8)
env.scenario$cat.adjustments$z1accom[8,] <- c(0.2,0.8)
#env.scenario$cat.adjustments$welfare[10,] <- c(0.2,0.8)
#env.scenario$cat.adjustments$PUNISH[1,] <- c(rep(1/6, 6))
env.scenario$cat.adjustments$NPRESCH[1,] <- c(.01, .1, .01, .88)
env.scenario$cat.adjustments$INTERACT[1,] <- c(.01, .01, .01, rep(.97/6, 6))
#env.scenario$simulate(1)
env.scenario$simulate(2)

test <- tableBuilder(envName="Base", statistic="means", variableName="ga", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="INTERACT", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="INTERACT", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="INTERACT", grpbyName="z1single0Lvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="INTERACT", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 
 test <- tableBuilder(envName="Base", statistic="means", variableName="PUNISH", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="PUNISH", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="PUNISH", grpbyName="z1single0Lvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="PUNISH", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 
 test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="z1single0Lvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="means", variableName="NPRESCH", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 

 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="INTERACT", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="INTERACT", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="INTERACT", grpbyName="z1single0Lvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="INTERACT", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="mhrswrk", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="PUNISH", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="PUNISH", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="PUNISH", grpbyName="z1single0Lvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="PUNISH", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="NPRESCH", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="NPRESCH", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="NPRESCH", grpbyName="z1single0Lvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="NPRESCH", grpbyName="z1singleLvl1", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)

 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1condLvl1", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)


test <- tableBuilder(envName="Base", statistic="means", variableName="z1single0Lvl1", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)

test <- tableBuilder(envName="Base", statistic="means", variableName="z1singleLvl1", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1singleLvl1", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)

test <- tableBuilder(envName="Base", statistic="frequencies", variableName="householdsize", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="frequencies", variableName="householdsize", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="BREAST", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="frequencies", variableName="BREAST", grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)

test <- tableBuilder(envName="Base", statistic="frequencies", variableName="BREAST", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="BREAST", grpbyName="NPRESCH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
test <- tableBuilder(envName="Base", statistic="means", variableName="BREAST", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)

test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1overcrowdLvl1", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)

.NUMRUNS <- 2
env.scenario <<- SimenvMELC$new()
subgroupExpression <- "r1stchildethnLvl2==1 | r1stchildethnLvl3==1"
setGlobalSubgroupFilterExpression(subgroupExpression)
env.scenario$cat.adjustments$welfare[1,] <- c(.815, .185)
env.scenario$cat.adjustments$welfare[2,] <- c(.9, .1)
env.scenario$cat.adjustments$welfare[3,] <- c(.888, .112)
env.scenario$cat.adjustments$welfare[4,] <- c(.864, .136)
env.scenario$cat.adjustments$welfare[5,] <- c(.846, .154)
env.scenario$cat.adjustments$welfare[6,] <- c(.876, .124)
env.scenario$cat.adjustments$welfare[7,] <- c(.88, .12)
env.scenario$cat.adjustments$welfare[8,] <- c(.89, .11)
env.scenario$cat.adjustments$welfare[9,] <- c(.891, .109)
env.scenario$cat.adjustments$welfare[10,] <- c(.894, .106)
env.scenario$cat.adjustments$welfare[11,] <- c(.895, .105)
env.scenario$cat.adjustments$welfare[12,] <- c(.879, .121)
env.scenario$cat.adjustments$welfare[13,] <- c(.879, .121)
#env.scenario$simulate(1)
env.scenario$simulate(2)

mat <- matrix(c(.815, .9, .888, .864, .846, .876, .88, .89, .891, .894, .895, .879, .879,
				.185, .1, .112, .136, .154, .124, .12, .11, .109, .106, .105, .121, .121), 
		byrow=FALSE, ncol=2)
for (i in 1:NUM_ITERATIONS) {
	env.scenario$cat.adjustments$welfare[i,] <- mat[i,]
}


env.scenario$cat.adjustments$welfare[5,] <- c(.85, .15)
env.scenario$cat.adjustments$welfare[6,] <- c(.87, .13)
env.scenario$cat.adjustments$welfare[7,] <- c(.89, .11)
env.scenario$cat.adjustments$welfare[8,] <- c(.91, .09)
env.scenario$cat.adjustments$welfare[9,] <- c(.93, .07)
env.scenario$cat.adjustments$welfare[10,] <- c(.95, .05)
env.scenario$cat.adjustments$welfare[11,] <- c(.95, .05)
env.scenario$cat.adjustments$welfare[12,] <- c(.95, .05)
env.scenario$cat.adjustments$welfare[13,] <- c(.95, .05)

env.scenario$cat.adjustments$fsmoke[1,] <- c(.925, .029, .032, .014)

test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1condLvl1", grpbyName="r1stchildethn", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)

x <- is.model.var("MAGE", models)
 n <- names(x)
 write.csv(n, "H:/COMPASS/colloquium2014/MAGEModels.csv", row.names=F)
 
 
 test <- tableBuilder(envName="Base", statistic="frequencies", variableName="z1overcrowdLvl1", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 
 .NUMRUNS <- 2
 env.scenario <<- SimenvMELC$new()
 subgroupExpression <- "SESBTHLvl3==1"
 setGlobalSubgroupFilterExpression(subgroupExpression)
 env.scenario$cat.adjustments$z1overcrowd[1,] <- c(.868, .132)
 env.scenario$cat.adjustments$z1overcrowd[2,] <- c(.848, .152)
 env.scenario$cat.adjustments$z1overcrowd[3,] <- c(.838, .162)
 env.scenario$cat.adjustments$z1overcrowd[4,] <- c(.833, .167)
 env.scenario$cat.adjustments$z1overcrowd[5,] <- c(.842, .158)
 env.scenario$cat.adjustments$z1overcrowd[6,] <- c(.811, .189)
 env.scenario$cat.adjustments$z1overcrowd[7,] <- c(.812, .188)
 env.scenario$cat.adjustments$z1overcrowd[8,] <- c(.819, .181)
 env.scenario$cat.adjustments$z1overcrowd[9,] <- c(.831, .181)
 env.scenario$cat.adjustments$z1overcrowd[10,] <- c(.850, .169)
 env.scenario$cat.adjustments$z1overcrowd[11,] <- c(.878, .150)
 env.scenario$cat.adjustments$z1overcrowd[12,] <- c(.896, .122)
 env.scenario$cat.adjustments$z1overcrowd[13,] <- c(.888, .104)
 #env.scenario$simulate(1)
 env.scenario$simulate(2)
 
 .NUMRUNS <- 1
 env.scenario <<- SimenvMELC$new()
 setGlobalSubgroupFilterExpression(subgroupExpression)
 env.scenario$cat.adjustments$z1overcrowd[1,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[2,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[3,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[4,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[5,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[6,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[7,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[8,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[9,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[10,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[11,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[12,] <- c(.9, .1)
 env.scenario$cat.adjustments$z1overcrowd[13,] <- c(.9, .1)
 env.scenario$simulate(1)

 
 .NUMRUNS <- 1
 env.scenario <<- SimenvMELC$new()
 env.scenario$cat.adjustments$SESBTH[1,] <- c(.35, .4, .25)
 env.scenario$simulate(1)

 
 test <- tableBuilder(envName="Scenario", statistic="frequencies", variableName="z1overcrowdLvl1", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 
 test <- tableBuilder(envName="Base", statistic="means", variableName="gptotvis", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 test <- tableBuilder(envName="Scenario", statistic="means", variableName="gptotvis", grpbyName="SESBTH", CI=TRUE, logisetexpr=NULL, dict=dict.MELC)
 
 tableBuilder(envName="Base", statistic="means", variableName="fhrswrk", grpbyName="r1stfeduc", CI=FALSE)
 
 basefiledir=dirs$base
 modelfiledir=dirs$models
 propensityfiledir=dirs$PropensityModels
 catToCont.modelfiledir=dirs$catToContModels
 num.iterations=NUM_ITERATIONS
 
 mx <- outcomes[catvars][[7]]
 test <- table_mx_cols_MELC(mx, grpby=sg.var, grpby.tag=sg.expr, dict=dict.MELC)
catvars[7] 
 
collated_results_freqs <- collated_results$freqs[[1]]
num.runs=1

check.row.names <- function(run) {
	#for each iteration, check row.names exist, if not, set rownames to be the same as those that do exist
	rownames.list <- lapply(run, rownames)
	not.present <- lapply(rownames.list, function(x) { which(is.null(x)) })
	not.present.id <- as.numeric(names(unlist(not.present)))
}

test <- mean_mx_cols_BCASO(outcomes[convars][[15]], grpby=sg.var, grpby.tag=sg.expr, dict=dict.MELC)

test <- collator_freqs_remove_zero_cat3(all_run_results_zipped$freqs_by_subgroup[[10]], dict=dict.MELC, CI=TRUE)

test <- collator_freqs_remove_zero_cat3(all_run_results_zipped$freqs_by_subgroup[[9]], dict=dict.MELC, CI=TRUE)

test <- table_mx_cols_MELC(outcomes[catvars][[9]], , grpby=sg.var, grpby.tag=sg.expr, dict=dict.MELC)

test <-  table_mx_cols_MELC(outcomes[catvars][[9]], grpby=sg.var, grpby.tag=sg.expr, dict=dict.MELC)

test <- mean_mx_cols_BCASO(outcomes[convars][[15]], grpby=sg.var, grpby.tag=sg.expr, dict=dict.MELC)

test <- collator_freqs(all_run_results_zipped$confreqs[[2]], dict=dict.MELC, CI=FALSE)

test <- collator_freqs(all_run_results_zipped$confreqs[[11]], dict=dict.MELC, CI=TRUE)

result.by.col<- lapply(1:ncol(mx), function(i) {
			#i=1
			aggregate(mx[,i], by=list(grpby[,i]), FUN=quantile, probs=probs, na.rm=TRUE)
		})