




simulateRun <- function(nRun, simenv) {
  
    simulate_family_household <- function() {
      if (iteration <=13) {
        if (iteration>=2 & iteration<=5) {
          z1singleLvl1 <- predSimBinomsSelect_notChangeScores(z1single_previousLvl1, models$z1singlePrev0A2_5, models$z1singlePrev1A2_5)	
        } else if (iteration>=6 & iteration<=13) {
          z1singleLvl1 <- predSimBinomsSelect_notChangeScores(z1single_previousLvl1, models$z1singlePrev0A6_13, models$z1singlePrev1A6_13)
        }
        
        #generate propensities for scenario testing
        z1singlemodel <- PropensityModels[["z1single"]]
        z1singlePropensities <- predLogistic(z1singlemodel[[1]])
        #add some random variation to the propensity scores so that each run the units changed can differ
        #give each unit it's own standard deviation according to a binomial distribution
        s <- sqrt(z1singlePropensities*(1 - z1singlePropensities))
        z1singlePropensities <- rnorm(length(z1singlePropensities), z1singlePropensities, s)
        
        #CALIBRATION
        mult.factor <- c(NA, 1.208, 1.230, 1.148, 1.096, 1.134, 1.055, 1.043, 1.091, 1.068, 1.065, 1.071, 1.062)
        prop1 <- mean(z1singleLvl1)*mult.factor[iteration]
        calib.cat.adjust <- c(1-prop1, prop1)
        z1singleLvl1 <- adjustCatVarCalib(z1singleLvl1, "z1single", propens=z1singlePropensities, desiredProps=calib.cat.adjust)
        
        #scenarios
        z1singleLvl1 <<- adjustCatVar(z1singleLvl1, "z1single", propens=z1singlePropensities)
        z1singleLvl0 <<- as.integer(!z1singleLvl1)
        if (iteration<=13) {
          checkNAs(z1singleLvl1)
        }
        
        z1single_ethLvl1 <- z1singleLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
        z1single_ethLvl2 <- z1singleLvl1*r1stchildethnLvl2
        z1single_ethLvl3 <- z1singleLvl1*r1stchildethnLvl3
        
        z1single_previous_ethLvl1 <- z1single_previousLvl1*(r1stchildethnLvl2 + r1stchildethnLvl4)
        z1single_previous_ethLvl2 <- z1single_previousLvl1*r1stchildethnLvl2
        z1single_previous_ethLvl3 <- z1single_previousLvl1*r1stchildethnLvl3
        
        z1single_ethLvl1 <<- z1single_ethLvl1
        z1single_ethLvl2 <<- z1single_ethLvl2
        z1single_ethLvl3 <<- z1single_ethLvl3
        z1single_previous_ethLvl1 <<- z1single_previous_ethLvl1
        z1single_previous_ethLvl2 <<- z1single_previous_ethLvl2
        z1single_previous_ethLvl3 <<- z1single_previous_ethLvl3
        
        NO_PARENT <- 99L
        
        randunif <- runif(NUMCHILDREN)
        randunif2 <- runif(NUMCHILDREN)
        
        onemorep <-  z1singleLvl0 & z1single_previousLvl1
        onelessp <-  z1singleLvl1 & !z1single_previousLvl1
        samep <-  z1singleLvl1 == z1single_previousLvl1
        
        assert(onemorep | onelessp | samep)
        
        sptype <- rep(NA_integer_, NUMCHILDREN)
        sptype[onemorep] <- 1L
        sptype[onelessp] <- 2L
        sptype[samep] <- 3L
        
        onelessp_mumleft <- onelessp & randunif <= 0.0738
        onelessp_dadleft <- onelessp & randunif > 0.0738
        
        onemorep_newdad <-	onemorep & fage_years_previous == NO_PARENT
        onemorep_newmum <-	onemorep & mage_years_previous == NO_PARENT
        #added for scenarios, in a scenario if we set a child to be in a single parent family, we
        #would ideally also set either fage_years or mage_years to be 99.  This is a work-around.
        prop.new.dad <- .95
        tot.num.new.dads <- max(round(sum(onemorep)*prop.new.dad), sum(onemorep_newdad))
        
        if (tot.num.new.dads>(sum(onemorep) - sum(onemorep_newmum))) {
          tot.num.new.dads <- sum(onemorep) - sum(onemorep_newmum)
        }
        
        
        #those left to assign new mum or dad
        to.assign <- onemorep & !onemorep_newdad & !onemorep_newmum
        num.new.dads.to.assign <- tot.num.new.dads - sum(onemorep_newdad)
        
        if (length(which(to.assign))>1){
          rand.nums <- sample(which(to.assign))
          onemorep_newdad[which(to.assign)][rank(rand.nums)<=num.new.dads.to.assign] <- TRUE
          onemorep_newmum[which(to.assign)][rank(rand.nums)>num.new.dads.to.assign] <- TRUE
        } else if ((length(which(to.assign))==1)&(num.new.dads.to.assign==1)) {
          onemorep_newdad[which(to.assign)] <- TRUE
        } else if ((length(which(to.assign))==1)&(num.new.dads.to.assign==0)) {
          onemorep_newmum[which(to.assign)] <- TRUE
        } else if (sum(to.assign)==0) {
          none.to.assign <- TRUE	
        } else {
          stop("Check assignment of new mums and dads in simulate_family_household()")
        }
        
        assert(onemorep_newdad | onemorep_newmum | onelessp_mumleft | onelessp_dadleft | samep)
        
        single_father_family <- (samep & mage_years_previous == NO_PARENT) | onelessp_mumleft
        single_mother_family <- ((samep & z1single_previousLvl1==1) & !single_father_family) | onelessp_dadleft
        
        new_mums <- rbinom(sum(single_mother_family), 1, .0128)
        new_mums <- new_mums==1
        sing.mums <- which(single_mother_family==TRUE)
        single_mother_family_new_mum <- rep(FALSE, length(single_mother_family))
        single_mother_family_new_mum[sing.mums] <- new_mums
        single_mother_family_same_mum <- single_mother_family & !single_mother_family_new_mum
        
        two_parent_family <- (samep & !z1singleLvl1) | onemorep
        
        assert(single_father_family | single_mother_family_new_mum | single_mother_family_same_mum| two_parent_family ) 
        
        num_two_parent_family <- sum(two_parent_family)
        
        two_parent_family_no_change <- two_parent_family & samep
        two_parent_family_mum_changed <- two_parent_family & onemorep_newmum
        two_parent_family_dad_changed <- two_parent_family & onemorep_newdad
        two_parent_family_both_changed <- two_parent_family & !samep & !onemorep_newmum & !onemorep_newdad
        
        typeofchangeLvl1 <- rep(0L, NUMCHILDREN)
        typeofchangeLvl2 <- rep(0L, NUMCHILDREN)
        typeofchangeLvl3 <- rep(0L, NUMCHILDREN)
        typeofchangeLvl4 <- rep(0L, NUMCHILDREN)
        typeofchangeLvl5 <- rep(0L, NUMCHILDREN)
        
        typeofchangeLvl1[onemorep_newmum] <- 1L
        typeofchangeLvl2[onemorep_newdad] <- 1L
        typeofchangeLvl3[onelessp_mumleft] <- 1L
        typeofchangeLvl4[onelessp_dadleft] <- 1L
        typeofchangeLvl5[samep] <- 1L
        
        typeofchangeLvl1 <<- typeofchangeLvl1
        typeofchangeLvl2 <<- typeofchangeLvl2
        typeofchangeLvl3 <<- typeofchangeLvl3
        typeofchangeLvl4 <<- typeofchangeLvl4
        typeofchangeLvl5 <<- typeofchangeLvl5
        
        eth_typeofchangeLvl1 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl1
        eth_typeofchangeLvl2 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl2
        eth_typeofchangeLvl3 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl3
        eth_typeofchangeLvl4 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl4
        eth_typeofchangeLvl5 <<- (r1stchildethnLvl1 + r1stchildethnLvl4)*typeofchangeLvl5
        eth_typeofchangeLvl6 <<- r1stchildethnLvl2*typeofchangeLvl1
        eth_typeofchangeLvl7 <<- r1stchildethnLvl2*typeofchangeLvl2
        eth_typeofchangeLvl8 <<- r1stchildethnLvl2*typeofchangeLvl3
        eth_typeofchangeLvl9 <<- r1stchildethnLvl2*typeofchangeLvl4
        eth_typeofchangeLvl10 <<- r1stchildethnLvl2*typeofchangeLvl5
        eth_typeofchangeLvl11 <<- r1stchildethnLvl3*typeofchangeLvl1
        eth_typeofchangeLvl12 <<- r1stchildethnLvl3*typeofchangeLvl2
        eth_typeofchangeLvl13 <<- r1stchildethnLvl3*typeofchangeLvl3
        eth_typeofchangeLvl14 <<- r1stchildethnLvl3*typeofchangeLvl4
        eth_typeofchangeLvl15 <<- r1stchildethnLvl3*typeofchangeLvl5
        
        #typnode, specifies which typology tree branch was selected
        #for debugging purposes
        typnode <- rep(NA_integer_, NUMCHILDREN)
        typnode[onelessp_mumleft] <- 1L
        typnode[onelessp_dadleft] <- 2L
        typnode[onemorep_newdad] <- 3L
        typnode[onemorep_newmum] <- 4L
        typnode[single_father_family] <- 5L 
        typnode[single_mother_family_new_mum] <- 6L
        typnode[single_mother_family_same_mum] <- 7L
        #typnode[single_mother_family_mum_left_dad_return] <- 8L
        typnode[two_parent_family_both_changed] <- 9L 
        typnode[two_parent_family_mum_changed] <- 10L
        typnode[two_parent_family_dad_changed] <- 11L
        typnode[two_parent_family_no_change] <- 12L
        
        checkNAs(typnode)
        
        changewasmum <- rep(0L, NUMCHILDREN)
        changewasdad <- rep(0L, NUMCHILDREN)
        
        changewasmum[onelessp_mumleft | 
                       onemorep_newmum | 
                       single_mother_family_new_mum | 
                       two_parent_family_both_changed |
                       two_parent_family_mum_changed] <- 1L
        
        changewasdad[onelessp_dadleft |
                       onemorep_newdad |
                       two_parent_family_both_changed |
                       two_parent_family_dad_changed] <- 1L
        
        
        #fage_years
        fage_years <- rep(NA_integer_, NUMCHILDREN)
        
        fage_inc <- fage_years_previous!=NO_PARENT
        fage_years[fage_inc] <- fage_years_previous[fage_inc] + 1
        
        fage_years[onelessp_dadleft | single_mother_family_new_mum | single_mother_family_same_mum] <- NO_PARENT
        
        #when doing scenario testing it is possible to change children that were single to two-parent,
        #but they will still have 99s for fage_years_previous, so we need to impute a fathers age for this year
        #(it cannot just be incremented by 1) 
        #the model only depends on mage_years
        impute_fathers_age <- ((two_parent_family_no_change | single_father_family) & fage_years_previous==NO_PARENT) | onemorep_newdad
        
        fage_years[impute_fathers_age] <- as.integer(round(predSimNorm(models$fage_years, set=impute_fathers_age)))
        #fage years will be about 78 if mage_years 99 was used - write in some code to correct it if fage_years is this old
        if (any(fage_years[impute_fathers_age]>78)) {
          fage_years[(impute_fathers_age==TRUE)&(fage_years>78)] <- sample(fage_years[(!is.na(fage_years))&(fage_years<78)], length(which((impute_fathers_age==TRUE)&(fage_years>78))))
        }
        
        #double-check for NAs - there should be none
        if (sum(is.na(fage_years))>0) {
          warning("NAs in fage_years.  Mean fage_years imputed\n")
        }
        na.id <- which(is.na(fage_years))
        fage_years[na.id] <- round(mean(fage_years[fage_years!=99], na.rm=T))
        
        checkNAs(fage_years)
        
        #mage_years
        mage_years <- rep(NA_integer_, NUMCHILDREN)
        
        mage_years[onelessp_mumleft | single_father_family] <- NO_PARENT
        
        mage_inc <- (onelessp_dadleft | onemorep_newdad | two_parent_family | single_mother_family_same_mum) & mage_years_previous != NO_PARENT
        mage_prev_no_parent <- (onelessp_dadleft | onemorep_newdad | two_parent_family | single_mother_family_same_mum) & mage_years_previous == NO_PARENT
        mage_years[mage_inc] <- mage_years_previous[mage_inc] + 1
        mage_years[mage_prev_no_parent] <- NO_PARENT
        
        mage_years[onemorep_newmum] <- MAGE[onemorep_newmum] + iteration - 4
        
        mage_years_previous_smfnm <- mage_years_previous[single_mother_family_new_mum]
        
        if (length(mage_years_previous_smfnm) > 0) {
          differenceInMumAge <- rep(NA_integer_, length(mage_years_previous_smfnm))
          differenceInMumAge[mage_years_previous_smfnm >=55] <- -39
          differenceInMumAge[mage_years_previous_smfnm <=19] <- 41
          midgroup <- mage_years_previous_smfnm  > 19 & mage_years_previous_smfnm < 55
          differenceInMumAge[midgroup] <- sample(c(-1:3), size=sum(midgroup), replace=T)
          
          mage_years[single_mother_family_new_mum] <- mage_years_previous[single_mother_family_new_mum] + differenceInMumAge
        }
        
        checkNAs(mage_years)
        
        no99mage_years <- mage_years
        no99mage_years[mage_years==99] <- NA
        
        # mum group and dad group
        sameDadAsb4 <- !changewasdad & fage_years != NO_PARENT
        sameMumAsb4 <- !changewasmum & mage_years != NO_PARENT
        
        #if ever allow a scenario father's age then put an 'if' statement here that only
        #does this if a scenario is not being run on father's age
        fage_years_grouped_same_as_fage <- bin(fage_years - iteration, binbreaks$fage_years)
        at_birth_fage_years_grped <- bin(children$fage_years - 1, binbreaks$fage_years)
        #table(fage_years_grouped_same_as_fage, at_birth_fage_years_grped)
        current_fage_consistent_with_at_birth_fage <- fage_years_grouped_same_as_fage == at_birth_fage_years_grped
        
        birthfather <- sameDadAsb4 & current_fage_consistent_with_at_birth_fage
        
        if (any(is.na(simenv$cat.adjustments$MAGE))) {
          dif.in.mage <- (mage_years - iteration) - (children$mage_years - 1)
          birthmother <- sameMumAsb4 & (abs(dif.in.mage)<2)
        } else {
          #doing a scenario on mother's age
          #set a certain percentage to be the birthmothers
          #set 3.5% to be not birthmothers
          birthmother <- sameMumAsb4
          birthmother[birthmother==TRUE] <- rbinom(sum(sameMumAsb4), 1, 0.965) 
        }
        
        
        mumgroup <- rep(NA_integer_, NUMCHILDREN)
        mumgroup[birthmother==1] <- 1L
        mumgroup[!birthmother & !sameMumAsb4] <- 2L
        mumgroup[!birthmother & sameMumAsb4] <- 3L
        mumgroup[mage_years == NO_PARENT] <- 0L
        checkNAs(mumgroup)
        
        dadgroup <- rep(NA_integer_, NUMCHILDREN)
        dadgroup[birthfather] <- 1L
        dadgroup[!birthfather & !sameDadAsb4] <- 2L
        dadgroup[!birthfather & sameDadAsb4] <- 3L
        dadgroup[(fage_years == NO_PARENT)|single_mother_family] <- 0L
        #temp fix
        if (sum(is.na(dadgroup))>0) {
          na.id <- which(is.na(dadgroup))
          dadgroup[na.id] <- 1
        }
        checkNAs(dadgroup)
        
        #kids
        if (iteration<=5) {
          change_children_num <- predSimNorm(models$chkids2_5)
        } else if (iteration>5 & iteration<=13) {
          change_children_num <- predSimNorm(models$chkids6_13)
        }# else if (sum(is.na(z1singleLvl1))==0) {
        #z1single was simulated for year 14 to 18
        #bthorder1.change <- rep(0, NUMCHILDREN)
        #bthorder2.change <- rbinom(NUMCHILDREN, 1, .01) 
        #bthorder2.change <- bthorder2.change*(-1)
        #bthorder3.change <- rbinom(NUMCHILDREN, 1, .02)
        #bthorder3.change <- bthorder3.change*(-1)
        
        #	change_children_num <- rep(NA, NUMCHILDREN)
        #	change_children_num[bthorder==1] <- bthorder1.change[bthorder==1]
        #	change_children_num[bthorder==2] <- bthorder2.change[bthorder==2]
        #	change_children_num[bthorder>=3] <- bthorder3.change[bthorder>=3]
        
        #consistency tweak
        change_children_num[change_children_num < 1 - kids_previous] <- (1 - kids_previous)[change_children_num < 1 - kids_previous]
        
        kids <- kids_previous + change_children_num
        
        kids <- round(kids)
        
        #max cut-off tweak
        kids[kids>10] <- 10
        
        #generate propensities for scenario testing
        kidsmodels <- PropensityModels[["kids"]]
        kidsPropensities <- predictOrdinal(kidsmodels, NUMCHILDREN, stochastic=TRUE)
        
        kids <- adjustContVar(kids, "kids", propens=kidsPropensities[,-ncol(kidsPropensities)])
        
        kids[kids<1] <- 1
        kids[kids>10] <- 10
        checkNAs(kids)
        
        change_kids <- kids - kids_previous
        
        #household_size
        householdsize <- round(householdsize_previous - as.integer(onelessp) + as.integer(onemorep) + change_kids)
        householdsize[householdsize < 2] <- 2
        
        #CALIBRATION
        mult.factor <- c(NA, rep(1, 2), .989, .967, .965, .939, .934, .952, .946, .916, .983, .960)
        householdsize <- householdsize*mult.factor[iteration]
        #no rounding or else we just get exactly the same values as not aligning
        #note: this means that confreqs will have decimals in it.					
        
        #scenarios
        householdsize <- adjustContVar(householdsize, "householdsize")
        
        #max cut-off tweak
        householdsize[householdsize < 2] <- 2
        householdsize[householdsize > 14] <- 14
        checkNAs(householdsize)
        
        #modify enclosing environment
        
        onemorep <<- as.integer(onemorep)
        onelessp <<-  as.integer(onelessp)
        samep <<-  as.integer(samep)
        
        sptype <<- sptype
        sptypeLvl1 <<- rep(0, NUMCHILDREN)
        sptypeLvl1[sptype==1] <<- 1 
        sptypeLvl2 <<- rep(0, NUMCHILDREN)
        sptypeLvl2[sptype==2] <<- 1 
        sptypeLvl3 <<- rep(0, NUMCHILDREN)
        sptypeLvl3[sptype==3] <<- 1 
        typnode <<- typnode
        
        fage_years <<- fage_years
        mage_years <<- mage_years
        
        #no99fage_years <<- no99fage_years
        no99mage_years <<- no99mage_years
        
        kids <<- kids
        householdsize <<- householdsize
        
        mumgroup <<- mumgroup
        dadgroup <<- dadgroup
      } else {
        #iteration>=14
        z1singleLvl1 <<- NAs
        kids <<- NAs
        householdsize <<- NAs
        mumgroup <<- NAs
        dadgroup <<- NAs
      }
    }
    
    simulate_psychosocial_factors <- function() {
      
      ####################
      #####  z1chpar  ####
      ####################
      if (iteration<=5) {
        z1chparLvl1 <- predSimBinomsSelect_notChangeScores(z1chpar_previousLvl1, models$z1chparPrev0A2_5, models$z1chparPrev1A2_5)
      } else if (iteration>5 & iteration<=13) {
        z1chparLvl1 <- predSimBinomsSelect_notChangeScores(z1chpar_previousLvl1, models$z1chparPrev0A6_13, models$z1chparPrev1A6_13)
      } else {
        z1chparLvl1 <<- NAs
      }
      ##switching 0s and 1s for children that previously had a change in parents 
      ##(z1chpar different kind of change score to e.g. z1single or welfare) 
      #z1chparLvl1[z1chpar_previousLvl1==1] <- (-1)*(z1chparLvl1[z1chpar_previousLvl1==1] - 1)
      
      if (iteration<=13) {
        #generate propensities for scenario testing
        z1chparmodel <- PropensityModels[["z1chpar"]]
        z1chparPropensities <- predLogistic(z1chparmodel[[1]])
        #add some random variation to the propensity scores so that each run the units changed can differ
        #give each unit it's own standard deviation according to a binomial distribution
        s <- sqrt(z1chparPropensities*(1 - z1chparPropensities))
        z1chparPropensities <- rnorm(length(z1chparPropensities), z1chparPropensities, s)
        
        z1chparLvl1 <<- adjustCatVar(z1chparLvl1, "z1chpar", propens=z1chparPropensities)
        
        z1chparLvl0 <<- as.integer(!z1chparLvl1)
        checkNAs(z1chparLvl1)
      }
      
      
      z1chpar_ethLvl1 <- z1chparLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
      z1chpar_ethLvl2 <- z1chparLvl1*r1stchildethnLvl2
      z1chpar_ethLvl3 <- z1chparLvl1*r1stchildethnLvl3
      
      z1chpar_previous_ethLvl1 <- z1chpar_previousLvl1*(r1stchildethnLvl2 + r1stchildethnLvl4)
      z1chpar_previous_ethLvl2 <- z1chpar_previousLvl1*r1stchildethnLvl2
      z1chpar_previous_ethLvl3 <- z1chpar_previousLvl1*r1stchildethnLvl3
      
      z1chpar_ethLvl1 <<- z1chpar_ethLvl1
      z1chpar_ethLvl2 <<- z1chpar_ethLvl2
      z1chpar_ethLvl3 <<- z1chpar_ethLvl3
      z1chpar_previous_ethLvl1 <<- z1chpar_previous_ethLvl1
      z1chpar_previous_ethLvl2 <<- z1chpar_previous_ethLvl2
      z1chpar_previous_ethLvl3 <<- z1chpar_previous_ethLvl3
      
      ####################
      #####  chres  ######
      ####################
      #create binary variable for whether parents changed previously or not
      if (iteration <= 13) {
        z1chres.prev <- rep(NA, length(chres_previous))
        z1chres.prev[chres_previous>0] <- 1
        z1chres.prev[chres_previous==0] <- 0
        if (iteration<=5) {
          z1chres <- predSimBinomsSelect_notChangeScores(z1chres.prev, models$z1chresPrev0.a2_5, models$z1chresPrev1.a2_5)
        } else if (iteration>5 & iteration<=13) {
          z1chres <- predSimBinomsSelect_notChangeScores(z1chres.prev, models$z1chresPrev0.a6_13, models$z1chresPrev1.a6_13)
        } 
        #for children that do have a change in residence, simulate the number of changes
        if (iteration<=5) {
          chres.pre <- predSimNBinom(models$chres.a2_5) + 1
        } else if (iteration>5 & iteration<=13) {
          chres.pre <- predSimNBinom(models$chres.a6_13) + 1	
        } 
        #the '+ 1' is the backtransformation (chres - 1 is NB distributed)
        chres <- rep(NA, length(chres.pre))
        chres[z1chres==0] <- 0
        chres[z1chres==1] <- chres.pre[z1chres==1]
        #tweak for if the simulated value of chres is greater than 13
        chres[chres>13] <- sample(c(2:10, 13), sum(chres>13), prob=c(.656, .198, .072, .023, .017, .008, .004, .019, .001, .001), replace=TRUE)
        checkNAs(chres)
        
        #generate propensities for scenario testing
        chresmodels <- PropensityModels[["chres"]]
        chresPropensities <- predictOrdinal(chresmodels, NUMCHILDREN, stochastic=TRUE)
        
        chres <- adjustContVar(chres, "chres", propens=chresPropensities[,-ncol(chresPropensities)])
      } else {
        #iteration >13
        chres <- NAs
      }
      
      chres <<- chres		
    }
    
    #' modifies: welfareLvl1, welfareLvl0, mhrswrk, fhrswrk
    #' simulate_employment()
    simulate_employment <- function() {
      if (iteration<=13) {
        #mhrswrk
        #create binary variable for whether the mother worked previously or not
        z1mhrs.prev <- rep(NA, length(mhrswrk_previous))
        z1mhrs.prev[mhrswrk_previous>0] <- 1
        z1mhrs.prev[mhrswrk_previous==0] <- 0
        
        #simulate for the current iteration whether the mother works or not
        #first create separate vectors of length number of children for each mumgroup model
        if (iteration<=5) {
          z1mhrs1 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a2_5.mg1, models$z1mhrswrk.prev1.a2_5)	
          z1mhrs2 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a2_5.mg2, models$z1mhrswrk.prev1.a2_5)	
          z1mhrs3 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a2_5.mg3, models$z1mhrswrk.prev1.a2_5)
        } else if (iteration>5) {
          z1mhrs1 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a6_13.mg1 , models$z1mhrswrk.prev1.a6_13)	
          z1mhrs2 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a6_13.mg2, models$z1mhrswrk.prev1.a6_13)	
          z1mhrs3 <- predSimBinomsSelect_notChangeScores(z1mhrs.prev, models$z1mhrswrk.prev0.a6_13.mg3, models$z1mhrswrk.prev1.a6_13)
        }
        #then assign to the variable denoting whether the mother works or not, the values from the vector corresponding to her mumgroup
        z1mhrs <- rep(NA, length(mhrswrk_previous))
        z1mhrs[mumgroup==0] <- z1mhrs1[mumgroup==0] #0 #
        z1mhrs[mumgroup==1] <- z1mhrs1[mumgroup==1]
        z1mhrs[mumgroup==2] <- z1mhrs2[mumgroup==2]
        z1mhrs[mumgroup==3] <- z1mhrs3[mumgroup==3]
        
        #for mothers that do work, simulate the number of hours they work
        #create separate vectors of length number of children for each mumgroup model
        
        if (iteration<=5) {
          mhrswrk.pre1 <- predSimNBinom(models$mhrswrk.a2_5.mg1) + 1
          #the other mumgroups had small numbers and so a standard NB model with a single dispersion parameter was used
          mhrswrk.pre2 <- predSimNBinom(models$mhrswrk.a2_5.mg2) + 1
          mhrswrk.pre3 <- predSimNBinom(models$mhrswrk.a2_5.mg3) + 1
          #the '+ 1' is the backtransformation (mhrswrk - 1 is NB distributed)
        } else if (iteration>5) {
          #mumgroup1 uses a negative binomial with the dispersion parameter modeled as a function of age
          alpha <- .4203 - .0145*iteration 
          mhrswrk.pre1 <- predSimNBinom(models$mhrswrk.a6_13.mg1, alpha=alpha) + 1
          #the other mumgroups had small numbers and so a standard NB model with a single dispersion parameter was used
          mhrswrk.pre2 <- predSimNBinom(models$mhrswrk.a6_13.mg2) + 1
          mhrswrk.pre3 <- predSimNBinom(models$mhrswrk.a6_13.mg3) + 1
          #the '+ 1' is the backtransformation (mhrswrk - 1 is NB distributed)
        }
        
        #assign to mhrswrk, the values from the vector corresponding to her mumgroup
        mhrswrk[mumgroup==1] <- mhrswrk.pre1[mumgroup==1]
        mhrswrk[mumgroup==2] <- mhrswrk.pre2[mumgroup==2]
        mhrswrk[mumgroup==3] <- mhrswrk.pre3[mumgroup==3]
        mhrswrk[z1mhrs==0] <- 0
        mhrswrk <- round(mhrswrk)
        mhrswrk[mhrswrk < 0] <- 0
        mhrswrk[mhrswrk>73] <- 73
        
        #CALIBRATION
        mult.factor <- c(NA, 1.22, 1.29, 1.36, 1.37, 1.30, 1.27, 1.24, 1.25, 1.15, 1.15, 1.10, 1.12)
        mhrswrk <- mhrswrk*mult.factor[iteration]
        #no rounding or else we just get exactly the same values as not aligning
        
        #generate propensities for scenario testing
        mhrswrkmodels <- PropensityModels[["mhrswrk"]]
        mhrswrkPropensities <- predictOrdinal(mhrswrkmodels, NUMCHILDREN, stochastic=TRUE)
        
        #scenarios
        mhrswrk <- adjustContVar(mhrswrk, "mhrswrk", propens=mhrswrkPropensities[,-ncol(mhrswrkPropensities)])
        checkNAs(mhrswrk)
        
        #fhrswrk
        #create binary variable for whether the father worked previously or not
        z1fhrs.prev <- rep(NA, length(fhrswrk_previous))
        z1fhrs.prev[fhrswrk_previous>0] <- 1
        z1fhrs.prev[fhrswrk_previous==0] <- 0
        
        #simulate for the current iteration whether the father works or not
        #first create separate vectors of length number of children for each dadgroup model
        if (iteration<=5) {
          z1fhrs1 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a2_5.dg1 , models$z1fhrswrk.prev1.a2_5.dg1)	
          z1fhrs2 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a2_5.dg2 , models$z1fhrswrk.prev1.a2_5.dg2)	
          z1fhrs3 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a2_5.dg3 , models$z1fhrswrk.prev1.a2_5.dg3)
        } else if (iteration>5) {
          z1fhrs1 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a6_13.dg1 , models$z1fhrswrk.prev1.a6_13.dg1)	
          z1fhrs2 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a6_13.dg2 , models$z1fhrswrk.prev1.a6_13.dg2)	
          z1fhrs3 <- predSimBinomsSelect_notChangeScores(z1fhrs.prev, models$z1fhrswrk.prev0.a6_13.dg3 , models$z1fhrswrk.prev1.a6_13.dg3)
        }
        #then assign to the variable denoting whether the father works or not, the values from the vector corresponding to his dadgroup
        z1fhrs <- rep(NA, length(fhrswrk_previous))
        z1fhrs[dadgroup==0] <- z1fhrs1[dadgroup==0] #0 #
        z1fhrs[dadgroup==1] <- z1fhrs1[dadgroup==1]
        z1fhrs[dadgroup==2] <- z1fhrs2[dadgroup==2]
        z1fhrs[dadgroup==3] <- z1fhrs3[dadgroup==3]
        
        #for fathers that do work, simulate the number of hours they work
        #3 normal models are used
        if (iteration<=5) {
          fhrswrk.pre <- predSimNorm(models$fhrswrk.a2_5)
        } else if (iteration>5) {
          fhrswrk.pre <- predSimNormsSelect3Models(dadgroup, models$fhrswrk.a6_13.dg1, models$fhrswrk.a6_13.dg2, models$fhrswrk.a6_13.dg3)
        }
        fhrswrk <- fhrswrk.pre
        fhrswrk[z1fhrs==0] <- 0
        fhrswrk <- round(fhrswrk)
        fhrswrk[fhrswrk < 0] <- 0
        fhrswrk[fhrswrk > 100] <- 100
        
        #generate propensities for scenario testing
        fhrswrkmodels <- PropensityModels[["fhrswrk"]]
        fhrswrkPropensities <- predictOrdinal(fhrswrkmodels, NUMCHILDREN, stochastic=TRUE)
        
        fhrswrk <- adjustContVar(fhrswrk, "fhrswrk", propens=mhrswrkPropensities[,-ncol(fhrswrkPropensities)])
        checkNAs(fhrswrk)
      } else {
        #iteration>=14
        mhrswrk <- NAs
        fhrswrk <- NAs
      }
      
      mhrswrk <<- mhrswrk
      fhrswrk <<- fhrswrk
      
      #welfare
      if (iteration <=13)  {
        if (iteration<=5) {
          welfareLvl1 <- predSimBinomsSelect(welfare_previousLvl1, models$welfarePrev0.a2_5, models$welfarePrev1.a2_5)
        } else if (iteration>5) {
          welfareLvl1 <- predSimBinomsSelect(welfare_previousLvl1, models$welfarePrev0.a6_13, models$welfarePrev1.a6_13)
        }
        
        #generate propensities for scenario testing
        welfaremodel <- PropensityModels[["welfare"]]
        welfarePropensities <- predLogistic(welfaremodel[[1]])
        #add some random variation to the propensity scores so that each run the units changed can differ
        #give each unit it's own standard deviation according to a binomial distribution
        s <- sqrt(welfarePropensities*(1 - welfarePropensities))
        welfarePropensities <- rnorm(length(welfarePropensities), welfarePropensities, s)
        
        #CALIBRATION
        mult.factor <- c(NA, 2.05, 1.22, 1.04, 1.03, 1.29, 1.26, 1.23, 1.23, 1.17, 1.15, 1.12, 1.09)
        prop1 <- mean(welfareLvl1)*mult.factor[iteration]
        calib.cat.adjust <- c(1-prop1, prop1)
        welfareLvl1 <- adjustCatVarCalib(welfareLvl1, "welfare", propens=welfarePropensities, desiredProps=calib.cat.adjust)
        
        #Scenarios
        welfareLvl1 <<- adjustCatVar(welfareLvl1, "welfare", propens=welfarePropensities)
        checkNAs(welfareLvl1)
        welfareLvl0 <<- as.integer(!welfareLvl1)
      } else {
        #iteration>=14
        welfareLvl1 <<- NAs
      }
      
      welfare_ethLvl1 <<- welfareLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
      welfare_ethLvl2 <<- welfareLvl1*r1stchildethnLvl2
      welfare_ethLvl3 <<- welfareLvl1*r1stchildethnLvl3
      
      welfare_previous_ethLvl1 <<- welfare_previousLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
      welfare_previous_ethLvl2 <<- welfare_previousLvl1*r1stchildethnLvl2
      welfare_previous_ethLvl3 <<- welfare_previousLvl1*r1stchildethnLvl3
      
    }
    
    simulate_material_circumstances <- function() {
      if (iteration<=13) {
        if (iteration<=5) {
          z1accomLvl1 <- predSimBinomsSelect_notChangeScores(z1accom_previousLvl1, models$z1accomPrev0.a2_5, models$z1accomPrev1.a2_5)
          z1homeownLvl1 <- predSimBinomsSelect_notChangeScores(z1homeown_previousLvl1, models$z1homeownPrev0.a2_5, models$z1homeownPrev1.a2_5)
          z1overcrowdLvl1 <- predSimBinomsSelect_notChangeScores(z1overcrowd_previousLvl1, models$z1overcrowdPrev0.a2_5, models$z1overcrowdPrev1.a2_5)
        } else if (iteration>5) {
          z1accomLvl1 <- predSimBinomsSelect_notChangeScores(z1accom_previousLvl1, models$z1accomPrev0.a6_13, models$z1accomPrev1.a6_13)
          z1homeownLvl1 <- predSimBinomsSelect_notChangeScores(z1homeown_previousLvl1, models$z1homeownPrev0.a6_13, models$z1homeownPrev1.a6_13)
          z1overcrowdLvl1 <- predSimBinomsSelect_notChangeScores(z1overcrowd_previousLvl1, models$z1overcrowdPrev0.a6_13, models$z1overcrowdPrev1.a6_13)
        }	
        
        #generate propensities for scenario testing and calibration
        z1accommodel <- PropensityModels[["z1accom"]]
        z1homeownmodel <- PropensityModels[["z1homeown"]]
        z1overcrowdmodel <- PropensityModels[["z1overcrowd"]]
        
        z1accomPropensities <- predLogistic(z1accommodel[[1]])
        z1homeownPropensities <- predLogistic(z1homeownmodel[[1]])
        z1overcrowdPropensities <- predLogistic(z1overcrowdmodel[[1]])
        
        #add some random variation to the propensity scores so that each run the units changed can differ
        #give each unit it's own standard deviation according to a binomial distribution
        sz1accom <- sqrt(z1accomPropensities*(1 - z1accomPropensities))
        sz1homeown <- sqrt(z1homeownPropensities*(1 - z1homeownPropensities))
        sz1overcrowd <- sqrt(z1overcrowdPropensities*(1 - z1overcrowdPropensities))
        
        z1accomPropensities <- rnorm(length(z1accomPropensities), z1accomPropensities, sz1accom)
        z1homeownPropensities <- rnorm(length(z1homeownPropensities), z1homeownPropensities, sz1homeown)
        z1overcrowdPropensities <- rnorm(length(z1overcrowdPropensities), z1overcrowdPropensities, sz1overcrowd)
        
        #CALIBRATION
        mult.factor <- c(NA, 1.14, 1.17, 1.10, 1.07, 1.12, 1.12, 1.09, 1.11, 1.12, 1.09, 1.11, 1.13)
        prop1 <- mean(z1homeownLvl1)*mult.factor[iteration]
        calib.cat.adjust <- c(1-prop1, prop1)
        z1homeownLvl1 <- adjustCatVarCalib(z1homeownLvl1, "z1homeown", propens=z1homeownPropensities, desiredProps=calib.cat.adjust)
        
        #SCENARIOS
        z1accomLvl1 <<- adjustCatVar(z1accomLvl1, "z1accom", propens=z1accomPropensities)
        z1homeownLvl1 <<- adjustCatVar(z1homeownLvl1, "z1homeown", propens=z1homeownPropensities)
        z1overcrowdLvl1 <<- adjustCatVar(z1overcrowdLvl1, "z1overcrowd", propens=z1overcrowdPropensities)
        
        checkNAs(z1accomLvl1)
        checkNAs(z1homeownLvl1)
        checkNAs(z1overcrowdLvl1)
      } else {
        #iteration>=14
        z1accomLvl1 <<- NAs
        z1homeownLvl1 <<- NAs
        z1overcrowdLvl1 <<- NAs
      } 
      
      z1accom_ethLvl1 <<- z1accomLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
      z1accom_ethLvl2 <<- z1accomLvl1*r1stchildethnLvl2
      z1accom_ethLvl2 <<- z1accomLvl1*r1stchildethnLvl2
      
      z1accom_previous_ethLvl1 <<- z1accom_previousLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
      z1accom_previous_ethLvl2 <<- z1accom_previousLvl1*r1stchildethnLvl2
      z1accom_previous_ethLvl2 <<- z1accom_previousLvl1*r1stchildethnLvl2
      
      z1homeown_ethLvl1 <<- z1homeownLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
      z1homeown_ethLvl2 <<- z1homeownLvl1*r1stchildethnLvl2
      z1homeown_ethLvl2 <<- z1homeownLvl1*r1stchildethnLvl2
      
      z1homeown_previous_ethLvl1 <<- z1homeown_previousLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
      z1homeown_previous_ethLvl2 <<- z1homeown_previousLvl1*r1stchildethnLvl2
      z1homeown_previous_ethLvl2 <<- z1homeown_previousLvl1*r1stchildethnLvl2
      
      z1overcrowd_ethLvl1 <<- z1overcrowdLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
      z1overcrowd_ethLvl2 <<- z1overcrowdLvl1*r1stchildethnLvl2
      z1overcrowd_ethLvl2 <<- z1overcrowdLvl1*r1stchildethnLvl2
      
      z1overcrowd_previous_ethLvl1 <<- z1overcrowd_previousLvl1*(r1stchildethnLvl1 + r1stchildethnLvl4)
      z1overcrowd_previous_ethLvl2 <<- z1overcrowd_previousLvl1*r1stchildethnLvl2
      z1overcrowd_previous_ethLvl2 <<- z1overcrowd_previousLvl1*r1stchildethnLvl2
    }
    
    simulate_behavioural_factors <- function() {
      if (iteration<=13) {
        #msmoke
        #create binary variable for whether the mother smoked previously or not
        z1msmk.prev <- rep(NA, length(msmoke_previous))
        z1msmk.prev[msmoke_previous>0] <- 1
        z1msmk.prev[msmoke_previous==0] <- 0
        
        #simulate for the current iteration whether the mother works or not
        #first create separate vectors of length number of children for each mumgroup model
        if (iteration<=5) {
          z1msmk1 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a2_5.mg1, models$z1msmoke.prev1.a2_5)	
          z1msmk2 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a2_5.mg2, models$z1msmoke.prev1.a2_5)	
          z1msmk3 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a2_5.mg3, models$z1msmoke.prev1.a2_5)
        } else if (iteration>5) {
          z1msmk1 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a6_13, models$z1msmoke.prev1.a6_13.mg1)	
          z1msmk2 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a6_13, models$z1msmoke.prev1.a6_13.mg2)	
          z1msmk3 <- predSimBinomsSelect_notChangeScores(z1msmk.prev, models$z1msmoke.prev0.a6_13, models$z1msmoke.prev1.a6_13.mg3)
        }
        #then assign to the variable denoting whether the mother works or not, the values from the vector corresponding to her mumgroup
        z1msmk <- rep(NA, length(mhrswrk_previous))
        z1msmk[mumgroup==0] <- 0 
        z1msmk[mumgroup==1] <- z1msmk1[mumgroup==1]
        z1msmk[mumgroup==2] <- z1msmk2[mumgroup==2]
        z1msmk[mumgroup==3] <- z1msmk3[mumgroup==3]
        
        #for mothers that do work, simulate the number of hours they work
        #3 normal models are used
        if (iteration<=5) {
          msmoke.pre <- predSimNorm(models$msmoke.a2_5)
        } else if (iteration>5) {
          msmoke.pre <- predSimNormsSelect3Models(mumgroup, models$msmoke.a6_13.mg1, models$msmoke.a6_13.mg2, models$msmoke.a6_13.mg3)
        }
        msmoke <- msmoke.pre
        msmoke[z1msmk==0] <- 0
        msmoke <- round(msmoke)
        msmoke[msmoke < 0] <- 0
        msmoke[msmoke > 70] <- 70
        
        #generate msmoke propensities for scenario testing
        msmokemodels <- PropensityModels[["msmoke"]]
        msmokePropensities <- predictOrdinal(msmokemodels, NUMCHILDREN, stochastic=TRUE)
        
        #CALIBRATION
        mult.factor <- c(NA, .993, .979, .991, 1.022, .964, .971, .974, .959, .957, .955, .962, .957)
        prop0 <- mean(msmoke==0)*mult.factor[iteration]
        m.bin <- bin(msmoke, binbreaks$msmoke)
        tab <- table(m.bin)/sum(table(m.bin))
        w <- tab[2:4]
        w2 <- w/sum(w)
        props1 <- (1-prop0)*w2
        calib.cat.adjust <- c(prop0, props1)
        msmoke <- adjustContVarCalib(msmoke, "msmoke", propens=msmokePropensities, desiredProps=calib.cat.adjust)
        
        #sceanrios
        msmoke <- adjustContVar(msmoke, "msmoke", propens=msmokePropensities[,-ncol(msmokePropensities)])
        checkNAs(msmoke)
        
        #fsmoke
        #create binary variable for whether the father smokeed previously or not
        z1fsmk.prev <- rep(NA, length(fsmoke_previous))
        z1fsmk.prev[fsmoke_previous>0] <- 1
        z1fsmk.prev[fsmoke_previous==0] <- 0
        
        #simulate for the current iteration whether the father works or not
        #first create separate vectors of length number of children for each dadgroup model
        ##z1fsmk0 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.dg0.prev0, models$z1fsmoke.dg0.prev1)
        if (iteration<=5) {
          z1fsmk1 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a2_5.dg1, models$z1fsmoke.prev1.a2_5.dg1)	
          z1fsmk2 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a2_5.dg2, models$z1fsmoke.prev1.a2_5.dg2)	
          z1fsmk3 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a2_5.dg3, models$z1fsmoke.prev1.a2_5.dg3)
        } else if (iteration>5) {
          z1fsmk1 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a6_13.dg1, models$z1fsmoke.prev1.a6_13)	
          z1fsmk2 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a6_13.dg2, models$z1fsmoke.prev1.a6_13)	
          z1fsmk3 <- predSimBinomsSelect_notChangeScores(z1fsmk.prev, models$z1fsmoke.prev0.a6_13.dg3, models$z1fsmoke.prev1.a6_13)
        }
        #then assign to the variable denoting whether the father works or not, the values from the vector corresponding to his dadgroup
        z1fsmk <- rep(NA, length(fhrswrk_previous))
        z1fsmk[dadgroup==0] <- 0
        z1fsmk[dadgroup==1] <- z1fsmk1[dadgroup==1]
        z1fsmk[dadgroup==2] <- z1fsmk2[dadgroup==2]
        z1fsmk[dadgroup==3] <- z1fsmk3[dadgroup==3]
        
        #for fathers that do work, simulate the number of hours they work
        #3 normal models are used
        if (iteration<=5) {
          fsmoke.pre <- predSimNormsSelect3Models(dadgroup, models$fsmoke.a2_5.dg1, models$fsmoke.a2_5.dg2, models$fsmoke.a2_5.dg3)
        } else if (iteration>5) {
          fsmoke.pre <- predSimNormsSelect3Models(dadgroup, models$fsmoke.a6_13.dg1, models$fsmoke.a6_13.dg2, models$fsmoke.a6_13.dg3)
        }
        fsmoke <- fsmoke.pre
        fsmoke[z1fsmk==0] <- 0
        fsmoke <- round(fsmoke)
        fsmoke[fsmoke < 0] <- 0
        fsmoke[fsmoke > 70] <- 70
        
        #generate fsmoke propensities for scenario testing
        fsmokemodels <- PropensityModels[["fsmoke"]]
        fsmokePropensities <- predictOrdinal(fsmokemodels, NUMCHILDREN, stochastic=TRUE)
        
        #CALIBRATION
        mult.factor <- c(NA, .924, .957, .959, .959, .985, .975, .976, .966, .959, .960, .954, .954)
        prop0 <- mean(fsmoke==0)*mult.factor[iteration]
        f.bin <- bin(fsmoke, binbreaks$fsmoke)
        tab <- table(f.bin)/sum(table(f.bin))
        w <- tab[2:4]
        w2 <- w/sum(w)
        props1 <- (1-prop0)*w2
        calib.cat.adjust <- c(prop0, props1)
        fsmoke <- adjustContVarCalib(fsmoke, "fsmoke", propens=fsmokePropensities, desiredProps=calib.cat.adjust)
        
        #scenarios
        fsmoke <- adjustContVar(fsmoke, "fsmoke", propens=fsmokePropensities[,-ncol(fsmokePropensities)])
        checkNAs(fsmoke)
      } else {
        #iteration>=14
        msmoke <- NAs
        fsmoke <- NAs
      }
      
      msmoke <<- msmoke
      fsmoke <<- fsmoke
    }
      
   
    simulate_health_service_use <- function() {
      #gptotvis
      if (iteration == 2) {
        gptotvis <<- predSimNBinom(models$gptotvis2)	
      } else if (iteration >=3 & iteration <= 5) {
        gptotvis <<- predSimNBinom(models$gptotvis3_5)
      } else if (iteration == 6) {
        gptotvis <<- predSimNBinom(models$gptotvis6)
      } else if (iteration >=7 & iteration <= 10) {
        gptotvis <<- predSimNBinom(models$gptotvis7_10)
      } else {
        gptotvis  <<- NAs
      }
      gptotvis[gptotvis>46] <<- 46
      
      
      #hadmtot
      if (iteration <= 5) {
        hadmtot <<- predSimPois(models$hadmtot2_5)
      } else if (iteration >=6 & iteration<=10) {
        hadmtot <<- predSimPois(models$hadmtot6_10)
      } else {
        hadmtot <<- NAs
      }
      hadmtot[hadmtot>6] <<- 6
      
      
      #houtptot
      if (iteration >= 2 & iteration <= 5) {
        houtptot <<- predSimNBinom(models$houtptot2_5)
      } else if (iteration >= 6 & iteration <= 10) {
        houtptot <<- predSimNBinom(models$houtptot6_10)
      } else {
        houtptot <<- NAs
      }
      houtptot[houtptot>27] <<- 27
      
      
      #gpresp
      if (iteration >= 2 & iteration<=5) {
        alpha <- 0.3716 + 0.1608*iteration
        gpresp <<- predSimNBinom(models$gpresp2_5, alpha=alpha)
        gpresp[gpresp>17] <<- 17
      } else if (iteration >=6 & iteration<=10) {
        alpha <- -.1989 + .225*iteration
        gpresp <<- predSimNBinom(models$gpresp6_10, alpha=alpha)
        gpresp[gpresp>17] <<- 17
      } else {
        gpresp <<- NAs
      }
      
      
      #gpmorb
      if (iteration == 2) {
        gpmorb <<- predSimNBinom(models$gpmorb2)
        gpmorb[gpmorb>23] <<- 23
      } else if ((iteration>=3) & (iteration<=4)) {
        gpmorb <<- predSimNBinom(models$gpmorb3_4)
        gpmorb[gpmorb>23] <<- 23
      } else if (iteration==5) {
        gpmorb <<- predSimNBinom(models$gpmorb5)
        gpmorb[gpmorb>23] <<- 23
      } else if ((iteration>=6) & (iteration<=7)) {
        gpmorb <<- predSimNBinom(models$gpmorb6_7)
        gpmorb[gpmorb>23] <<- 23
      } else {
        gpmorb <<- NAs
      }
      
      
      #gpprev
      if (iteration == 2) {
        gpprev <<- predSimNorm(models$gpprev2)
        gpprev[gpprev < 0.7] <<- 0
        gpprev[gpprev >= 0.7 & gpprev < 1.3] <<- 1
        gpprev[gpprev >= 1.3 & gpprev < 2.7] <<- 2
        gpprev[gpprev >= 2.7 & gpprev < 4.5] <<- 3
        gpprev[gpprev>=4.5] <<- 4
        #assert(gpprev < 4.5)
      } else if (iteration %in% c(3,4)) {
        gpprev <<- predSimPois(models$gpprev3_4)
      } else if (iteration==5) {
        gpprev <<- predSimPois(models$gpprev5)
      } else if (iteration >= 6 & iteration <= 7) {
        gpprev <<- predSimPois(models$gpprev6_7)
        #NB better fit but model couldn't etsimate parameters
      } else {
        gpprev <<- NAs
      }
      gpprev[gpprev>6] <<- 6
      
    }
    
    simulate_conduct <- function() {
      if (iteration < 4) {
        z1condLvl1  <<- NAs
      } else if (iteration == 4) {
        mean_mhrswrk1_3 <<- apply(outcomes$mhrswrk[,1:3], ROW, mean) 
        mean_welfare1_3 <<- apply(outcomes$welfare[,1:3], ROW, mean) 
        mean_z1homeown1_3 <<- apply(outcomes$z1homeown[,1:3], ROW, mean) 
        #mean_msmoke1_3 <<- apply(outcomes$msmoke[,1:3], ROW, mean) 
        z1condLvl1 <<- predSimBinom(models$cond4)
      } else if (iteration==5) {
        z1condLvl1 <<- predSimBinomsSelect_notChangeScores(z1cond_previousLvl1, models$cond.prev0.5, models$cond.prev1.5)
      } else if (iteration>=6 & iteration<=13) {
        z1condLvl1 <<- predSimBinomsSelect_notChangeScores(z1cond_previousLvl1, models$cond.prev0.6_13, models$cond.prev1.6_13)
      } else {
        z1condLvl1 <<- NAs
      }
      z1condLvl1 <<- adjustCatVar(z1condLvl1, "z1cond")
    }
    
    simulate_reading <- function() {
      if (iteration < 8) {
        burt  <<- NAs
      } else if (iteration == 8) {
        mean_z1single1_7 <<- apply(outcomes$z1single[,1:7], ROW, mean)
        mean_fsmoke1_7 <<- apply(outcomes$fsmoke[,1:7], ROW, mean)
        mean_welfare1_7 <<- apply(outcomes$welfareLvl1[,1:7], ROW, mean) 
        burt <<- predSimNorm(models$burt8)
        burt <<- round(burt)
        burt[burt<0] <<- 0
        burt[burt>110] <<- 110
      } else if (iteration >= 9 & iteration <= 13) {
        burt_delta <- predSimNorm(models$burt9_13)
        burt <<- burt_previous + burt_delta
        burt <<- round(burt)
        burt[burt<0] <<- 0
        burt[burt>110] <<- sample(100:110, 1)
      } else {
        burt  <<- NAs
      }
    }
    
    simulate_interact_punish_npresch <- function() {
      if (iteration<5){
        INTERACT <<- NAs
        PUNISH <<- NAs
        NPRESCH <<- NAs
      } else if (iteration==5) {
        #NPRESCH
        mean_householdsize1_5 <<- apply(outcomes$householdsize[,1:5], ROW, mean)
        mean_z1accom1_5 <<- apply(outcomes$z1accomLvl1[,1:5], ROW, mean)
        mean_chres1_5 <<- apply(outcomes$chres[,1:5], ROW, mean) 
        mean_msmoke1_5 <<- apply(outcomes$msmoke[,1:5], ROW, mean) 
        NPRESCH <<- predSimNorm(models$NPRESCH)
        NPRESCH <<- round(NPRESCH)
        NPRESCH[NPRESCH<0] <<- 0
        NPRESCH[NPRESCH>3] <<- 3
        
        #INTERACT
        mean_kids1_5 <<- apply(outcomes$kids[,1:5], ROW, mean)
        mean_mhrswrk1_5 <<- apply(outcomes$mhrswrk[,1:5], ROW, mean)
        mean_welfare1_5 <<- apply(outcomes$welfareLvl1[,1:5], ROW, mean) 
        mean_z1accom1_5 <<- apply(outcomes$z1accomLvl1[,1:5], ROW, mean) 
        mean_z1overcrowd1_5 <<- apply(outcomes$z1overcrowdLvl1[,1:5], ROW, mean) 
        mean_z1chpar1_5 <<- apply(outcomes$z1chparLvl1[,1:5], ROW, mean) 
        mean_chres1_5 <<- apply(outcomes$chres[,1:5], ROW, mean) 
        INTERACT.pre <- predSimNBinom(models$INTERACT)
        INTERACT <<- 10 - INTERACT.pre
        INTERACT[INTERACT<0] <<- 0
        INTERACT[INTERACT>10] <<- 10
        
        #PUNISH
        mean_mhrswrk1_5 <<- apply(outcomes$mhrswrk[,1:5], ROW, mean)
        mean_z1homeown1_5 <<- apply(outcomes$z1accomLvl1[,1:5], ROW, mean) 
        mean_z1chpar1_5 <<- apply(outcomes$z1chparLvl1[,1:5], ROW, mean) 
        PUNISH <- predSimPois(models$PUNISH)
        PUNISH[PUNISH<0] <<- 0
        PUNISH[PUNISH>5] <<- 5	
		
      }
      
    }
    
	
	 simulate_depression <- function() {
      if (iteration==18) {
        alcabuseLvl1 <<- rbinom(NUMCHILDREN, 1, .163)
        depressionLvl1 <<- predSimBinom(models$depression)	
      } else {
        alcabuseLvl1 <<- NAs
        depressionLvl1 <<- NAs
      }
    }
	
	modelCoef <-function(px, py, OR, n = 5000){
		
		f<-function(x)
		(px - (1-py))*n*x + x^2 - OR*(x^2 +(1-px)*(1-py)*n^2 - (2-px-py)*n*x)

		a <- uniroot(f, c(0,n))$root
		c <-(1-py)*n-a
		b <-(py-px)*n+c
		d <-px*n-c

		return( b/a)
		
		#intercept <- log(b/a)
		#coefOR <- log(d/c) -  log(b/a)		
										
		#c(intercept = intercept, coefOR=coefOR)
	}
	
	sumInt <- function(intercept, baseRate)		
		sum(intercept/(1+intercept)*baseRate/sum(baseRate))

	
	simulate_childrenOverweight <- function(outcomes) {
		
		#browser()
		
		pOverweight <- c(NA, 27.39165, 28.09076, 28.82803, 29.60347, 
						30.41708, 31.26884, 32.15878, 33.08687, 
						 34.05314, 35.05756, 36.10015, 37.18091, 
						 38.29983, 39.45691, 40.65216, 41.88558, 
						 43.15716, 44.46690, 45.81481, 47.20088)/100		
		
		pregsmk.baseRate <- 78.34/100
		breast.baseRate <- 35.72/100
		
		gender.baseRate <- 0.5102
		
		maori.baseRate <- 1-0.2494/0.826
		pacific.baseRate <- 1-0.0892/ 0.6658
		asian.baseRate <- 1-0.0848/0.6614
						 
		SESBTHLvl3.baseRate <- 1-0.3466
		
		#browser()
		
		#For the new scenerio
		z1breastLvl1 <- ifelse(BREAST == 0, 0, 1)
		z1pregsmkLvl1 <- ifelse(pregsmk == 0, 0, 1)		
	
	
		pOverweightEth <-pOverweight*0.28/mean(pOverweight, na.rm =TRUE)
	
		if (iteration>=15){
						
			intercept <- sumInt(c(
				modelCoef(1 - pregsmk.baseRate, pOverweight[iteration],
				exp(as.numeric(models$z1OverweightA15_21$coefficients[2][1]))),
				modelCoef(1 - breast.baseRate, pOverweight[iteration], 
				exp(as.numeric(models$z1OverweightA15_21$coefficients[3][1]))),
				modelCoef(1 - gender.baseRate, pOverweight[iteration], 
				exp(as.numeric(models$z1OverweightA15_21$coefficients[4][1]))),
				modelCoef(1 - maori.baseRate, pOverweightEth[iteration],			
				exp(as.numeric(models$z1OverweightA15_21$coefficients[5][1]))),
				modelCoef(1 - pacific.baseRate, pOverweightEth[iteration], 
				exp(as.numeric(models$z1OverweightA15_21$coefficients[6][1]))),
				modelCoef(1 - asian.baseRate, pOverweightEth[iteration], 
				exp(as.numeric(models$z1OverweightA15_21$coefficients[7][1]))),
				modelCoef(1 - SESBTHLvl3.baseRate, pOverweight[iteration], 
				exp(as.numeric(models$z1OverweightA15_21$coefficients[8][1])))), 					
				c(	pregsmk.baseRate, breast.baseRate, gender.baseRate, maori.baseRate, 
					pacific.baseRate, asian.baseRate, SESBTHLvl3.baseRate))	
					
			#probability of overweight in no breastfed group and smoking in pregency group
			
			models$z1OverweightA15_21$coefficients[1][1]  <- log(intercept/(1-intercept))
			
			outcomes$z1OverweightLvl1[,iteration] <- predSimBinom(models$z1OverweightA15_21)	   

		} else if(iteration>=2) {			
			
			
			intercept <- sumInt(c(
				modelCoef(1 - pregsmk.baseRate, pOverweight[iteration],
				exp(as.numeric(models$z1OverweightA2_14$coefficients[2][1]))),
				modelCoef(1 - breast.baseRate, pOverweight[iteration], 
				exp(as.numeric(models$z1OverweightA2_14$coefficients[3][1]))),
				modelCoef(1 - gender.baseRate, pOverweight[iteration], 
				exp(as.numeric(models$z1OverweightA2_14$coefficients[4][1]))),
				modelCoef(1 - maori.baseRate, pOverweightEth[iteration],			
				exp(as.numeric(models$z1OverweightA2_14$coefficients[5][1]))),
				modelCoef(1 - pacific.baseRate, pOverweightEth[iteration], 
				exp(as.numeric(models$z1OverweightA2_14$coefficients[6][1]))),
				modelCoef(1 - asian.baseRate, pOverweightEth[iteration], 
				exp(as.numeric(models$z1OverweightA2_14$coefficients[7][1]))),
				modelCoef(1 - SESBTHLvl3.baseRate, pOverweight[iteration], 
				exp(as.numeric(models$z1OverweightA2_14$coefficients[8][1])))), 					
				c(	pregsmk.baseRate, breast.baseRate, gender.baseRate, maori.baseRate, 
					pacific.baseRate, asian.baseRate, SESBTHLvl3.baseRate))		
			#probability of overweight in no breastfed group and smoking in pregency group
			
			
			
		 
			models$z1OverweightA2_14$coefficients[1][1]  <- log(intercept/(1-intercept))
			
			outcomes$z1OverweightLvl1[,iteration] <- predSimBinom(models$z1OverweightA2_14)	   
		
		} else {
		
			outcomes$z1OverweightLvl1 <- NAs     
			
		}	
		
		return(outcomes)
		#browser()
    }
  


	simulate_childrenObese <- function(outcomes) {
	  
	  pObeseBoy <- c(NA,  7.068566,  7.387689,  7.728898,  8.092193,  8.477572,  
					 8.885038,  9.314588, 9.766224, 10.239946, 10.735753, 11.253645,
					 11.793623, 12.355686, 12.939835, 13.546069, 14.174388, 14.824793,
					 15.497284, 16.191859, 16.908521)/100
	  
	  pObeseGirl <- c(NA,  9.120887,  9.215639,  9.358532,  9.549566,  9.788741,
					  10.076057, 10.411515, 10.795113, 11.226853, 11.706733, 12.234755,
					  12.810918, 13.435222, 14.107667, 14.828253, 15.596980, 16.413848,
					  17.278857, 18.192007, 19.153299)/100
	  
	  pOverweightBoy <- c(NA, 30.99537, 30.34067, 29.87084, 29.58587, 29.48576, 29.57052,
							29.84013, 30.29461, 30.93395, 31.75815, 32.76722, 33.96114, 
							35.33993, 36.90358, 38.65209, 40.58546,  42.70370, 45.00680,
							47.49476, 50.16758 )/100
	  
	  pOverweightGirl <-c(NA, 26.32267, 27.43213, 28.54159, 29.65106, 30.76052, 31.86998, 
						32.97944, 34.08890, 35.19837, 36.30783, 37.41729, 38.52675, 39.63622, 
						40.74568, 41.85514, 42.96460, 44.07406, 45.18353, 46.29299, 47.40245  )/100
	  
	  
	  pObeseBoy <- pObeseBoy/pOverweightBoy
	  
	  pObeseGirl <- pObeseGirl/pOverweightGirl
	  
	  pHighBw.baseRate.Gender1 <- 1- 0.186217
	  pHighBw.baseRate.Gender0 <- 1 - 0.1576642
	  
	  pLowBwGender0 <- 0.160764
	  
	  maori.baseRate.Gender1 <- 1- 0.2527562/0.8272764
	  pacific.baseRate.Gender1 <- 1-0.08983258/ 0.6643528
	  asian.baseRate.Gender1 <- 1-0.08289098/0.6574112
	  
	  SESBTHLvl3.baseRate.Gender1 <- 1-0.3548387
	  
	  maori.baseRate.Gender0 <- 1-0.246178 /0.8247746
	  pacific.baseRate.Gender0 <- 1-0.08859271/ 0.6671893
	  asian.baseRate.Gender0 <- 1-0.08663269 /0.6652293
	  
	  SESBTHLvl3.baseRate.Gender0 <- 1- 0.3527971
	  	  
	  if(iteration>=15){
	  
	    pObeseBoyEth <-pObeseBoy*0.18/mean(pObeseBoy, na.rm =TRUE)
	  
		pObeseGirlEth <-pObeseGirl*0.18/mean(pObeseGirl, na.rm =TRUE)
	
	
		z1ObeseLvl1 <- rep(0, 5000)
		
		intercept.Gender1 <- sumInt(c(
		  modelCoef(1 -  pHighBw.baseRate.Gender1, pObeseBoy[iteration],
					exp(as.numeric(models$z1ObeseGender1A15_21$coefficients[2][1]))),					
		  modelCoef(1 - maori.baseRate.Gender1, pObeseBoyEth[iteration],			
					exp(as.numeric(models$z1ObeseGender1A15_21$coefficients[3][1]))),
		  modelCoef(1 - pacific.baseRate.Gender1, pObeseBoyEth[iteration], 
					exp(as.numeric(models$z1ObeseGender1A15_21$coefficients[4][1]))),
		  modelCoef(1 - asian.baseRate.Gender1, pObeseBoyEth[iteration], 
					exp(as.numeric(models$z1ObeseGender1A15_21$coefficients[5][1]))),
		  modelCoef(1 - SESBTHLvl3.baseRate.Gender1, pObeseBoy[iteration], 
					exp(as.numeric(models$z1ObeseGender1A15_21$coefficients[6][1])))), 					
		  c(	 pHighBw.baseRate.Gender1, maori.baseRate.Gender1, 
			  pacific.baseRate.Gender1, asian.baseRate.Gender1, 
			  SESBTHLvl3.baseRate.Gender1))		
		
		
		intercept.Gender0 <- sumInt(c(
		  modelCoef(1 -  pHighBw.baseRate.Gender0, pObeseGirl[iteration],
					exp(as.numeric(models$z1ObeseGender0A15_21$coefficients[2][1]))),					
		  modelCoef(1 - maori.baseRate.Gender0, pObeseGirlEth[iteration],			
					exp(as.numeric(models$z1ObeseGender0A15_21$coefficients[3][1]))),
		  modelCoef(1 - pacific.baseRate.Gender0, pObeseGirlEth[iteration], 
					exp(as.numeric(models$z1ObeseGender0A15_21$coefficients[4][1]))),
		  modelCoef(1 - asian.baseRate.Gender0, pObeseGirlEth[iteration], 
					exp(as.numeric(models$z1ObeseGender0A15_21$coefficients[5][1]))),
		  modelCoef(1 - SESBTHLvl3.baseRate.Gender0, pObeseGirl[iteration], 
					exp(as.numeric(models$z1ObeseGender0A15_21$coefficients[6][1])))), 					
		  c(	 pHighBw.baseRate.Gender0, maori.baseRate.Gender0, 
			  pacific.baseRate.Gender0, asian.baseRate.Gender0, 
			  SESBTHLvl3.baseRate.Gender0))				
		
		#Boys
		models$z1ObeseGender1A15_21$coefficients[1][1] <- 
		  log(intercept.Gender1/(1-intercept.Gender1))
		
		
		#Girls 
		models$z1ObeseGender0A15_21$coefficients[1][1] <- 
		  log(intercept.Gender0/(1-intercept.Gender0))
		
		
		
		z1ObeseLvl1[as.numeric(outcomes$z1OverweightLvl1[,iteration])==1 & z1genderLvl1==1] <- 
		  predSimBinom(models$z1ObeseGender1A15_21)[as.numeric(outcomes$z1OverweightLvl1[,iteration])==1 & z1genderLvl1==1]
		
		z1ObeseLvl1[as.numeric(outcomes$z1OverweightLvl1[,iteration])==1 & z1genderLvl1==0]  <- 
		  predSimBinom(models$z1ObeseGender0A15_21)[as.numeric(outcomes$z1OverweightLvl1[,iteration])==1 & z1genderLvl1==0] 
		
	  }else if (iteration>=2) {
		
		
	  pObeseBoyEth <-pObeseBoy*0.07/mean(pObeseBoy, na.rm =TRUE)
	  
	  pObeseGirlEth <-pObeseGirl*0.085/mean(pObeseGirl, na.rm =TRUE)
	
	
		z1ObeseLvl1 <- rep(0, 5000)
		
		intercept.Gender1 <- sumInt(c(
		  modelCoef(1 -  pHighBw.baseRate.Gender1, pObeseBoy[iteration],
					exp(as.numeric(models$z1ObeseGender1A2_14$coefficients[2][1]))),					
		  modelCoef(1 - maori.baseRate.Gender1, pObeseBoyEth[iteration],			
					exp(as.numeric(models$z1ObeseGender1A2_14$coefficients[3][1]))),
		  modelCoef(1 - pacific.baseRate.Gender1, pObeseBoyEth[iteration], 
					exp(as.numeric(models$z1ObeseGender1A2_14$coefficients[4][1]))),
		  modelCoef(1 - asian.baseRate.Gender1, pObeseBoyEth[iteration], 
					exp(as.numeric(models$z1ObeseGender1A2_14$coefficients[5][1]))),
		  modelCoef(1 - SESBTHLvl3.baseRate.Gender1, pObeseBoy[iteration], 
					exp(as.numeric(models$z1ObeseGender1A2_14$coefficients[6][1])))), 					
		  c(	 pHighBw.baseRate.Gender1, maori.baseRate.Gender1, 
			  pacific.baseRate.Gender1, asian.baseRate.Gender1, 
			  SESBTHLvl3.baseRate.Gender1))		
		
		
		intercept.Gender0 <- sumInt(c(
		  modelCoef(1 -  pHighBw.baseRate.Gender0, pObeseGirl[iteration],
					exp(as.numeric(models$z1ObeseGender0A2_14$coefficients[2][1]))),					
		  modelCoef(1 - maori.baseRate.Gender0, pObeseGirlEth[iteration],			
					exp(as.numeric(models$z1ObeseGender0A2_14$coefficients[3][1]))),
		  modelCoef(1 - pacific.baseRate.Gender0, pObeseGirlEth[iteration], 
					exp(as.numeric(models$z1ObeseGender0A2_14$coefficients[4][1]))),
		  modelCoef(1 - asian.baseRate.Gender0, pObeseGirlEth[iteration], 
					exp(as.numeric(models$z1ObeseGender0A2_14$coefficients[5][1]))),
		  modelCoef(1 - SESBTHLvl3.baseRate.Gender0, pObeseGirl[iteration], 
					exp(as.numeric(models$z1ObeseGender0A2_14$coefficients[6][1])))), 					
		  c(	 pHighBw.baseRate.Gender0, maori.baseRate.Gender0, 
			  pacific.baseRate.Gender0, asian.baseRate.Gender0, 
			  SESBTHLvl3.baseRate.Gender0))				
		
		#Boys
		models$z1ObeseGender1A2_14$coefficients[1][1] <- 
		  log(intercept.Gender1/(1-intercept.Gender1))
		
		
		#Girls 
		models$z1ObeseGender0A2_14$coefficients[1][1] <- 
		  log(intercept.Gender0/(1-intercept.Gender0))	
		
				  
				  
				  
		z1ObeseLvl1[as.numeric(outcomes$z1OverweightLvl1[,iteration])==1 & z1genderLvl1==1] <- 
		  predSimBinom(models$z1ObeseGender1A2_14)[as.numeric(outcomes$z1OverweightLvl1[,iteration])==1 & z1genderLvl1==1]
		
			#
	
		z1ObeseLvl1[as.numeric(outcomes$z1OverweightLvl1[,iteration])==1 & z1genderLvl1==0] <- 
		  predSimBinom(models$z1ObeseGender0A2_14)[as.numeric(outcomes$z1OverweightLvl1[,iteration])==1 & z1genderLvl1==0]
		
		
	  }else 	
		z1ObeseLvl1  <- NAs	
	  
	  outcomes$z1ObeseLvl1[,iteration] <- z1ObeseLvl1
	  
	  return(outcomes)
	  #browser()
	  
	}


  
	simulate_asthma <- function() {
	  
		  
	  pObese <- c(NA, 8.032883,  8.241457,  8.485419,  8.764769,  9.079508,  9.429635,
				  9.815150, 10.236054, 10.692345,  11.184025, 11.711093, 12.273550,
				  12.871394, 13.504627, 14.173248, 14.877257, 15.616655, 16.391440,
				  17.201614, 18.047177)/100			   
	  
	  
	  pAsthma <-  c(NA, 12.883273, 13.767930, 14.520986, 15.142442, 15.632299, 15.990555, 
					16.217212, 16.312268, 16.275725, 16.107582, 15.807839, 15.376496, 
					14.813553, 14.119011, 13.292868, 12.335125, 11.245783, 10.024840,
					8.672298,  7.188156) /100		
	  
	  #print(table(z1ObeseLvl1))
	  
	   
	   
	   gender.baseRate <- 0.5102
		
		maori.baseRate <- 1-0.2494/0.826
		pacific.baseRate <- 1-0.0892/ 0.6658
		asian.baseRate <- 1-0.0848/0.6614
						 
		SESBTHLvl3.baseRate <- 1-0.3466
		
		#browser()
		
	
		pAsthmaEth <-pAsthma*0.13/mean(pAsthma, na.rm =TRUE)
	
		if (iteration>=15){
						
			intercept <- sumInt(c(
				modelCoef(pObese[iteration], pAsthma[iteration],
				exp(as.numeric(models$AsthmaA15_21$coefficients[2][1]))),
				modelCoef(1 - gender.baseRate, pAsthma[iteration], 
				exp(as.numeric(models$AsthmaA15_21$coefficients[3][1]))),
				modelCoef(1 - maori.baseRate, pAsthmaEth[iteration],			
				exp(as.numeric(models$AsthmaA15_21$coefficients[4][1]))),
				modelCoef(1 - pacific.baseRate, pAsthmaEth[iteration], 
				exp(as.numeric(models$AsthmaA15_21$coefficients[5][1]))),
				modelCoef(1 - asian.baseRate, pAsthmaEth[iteration], 
				exp(as.numeric(models$AsthmaA15_21$coefficients[6][1]))),
				modelCoef(1 - SESBTHLvl3.baseRate, pAsthma[iteration], 
				exp(as.numeric(models$AsthmaA15_21$coefficients[7][1])))), 					
				c(	1- pObese[iteration], gender.baseRate, maori.baseRate, 
					pacific.baseRate, asian.baseRate, SESBTHLvl3.baseRate))		
				
			models$AsthmaA15_21$coefficients[1][1]  <- log(intercept/(1-intercept))
			
			asthmaLvl1 <<- predSimBinom(models$AsthmaA15_21)	   

		} else if(iteration>=2) {			
			
			intercept <- sumInt(c(
				modelCoef(pObese[iteration], pAsthma[iteration],
				exp(as.numeric(models$AsthmaA2_14$coefficients[2][1]))),
				modelCoef(1 - gender.baseRate, pAsthma[iteration], 
				exp(as.numeric(models$AsthmaA2_14$coefficients[3][1]))),
				modelCoef(1 - maori.baseRate, pAsthmaEth[iteration],			
				exp(as.numeric(models$AsthmaA2_14$coefficients[4][1]))),
				modelCoef(1 - pacific.baseRate, pAsthmaEth[iteration], 
				exp(as.numeric(models$AsthmaA2_14$coefficients[5][1]))),
				modelCoef(1 - asian.baseRate, pAsthmaEth[iteration], 
				exp(as.numeric(models$AsthmaA2_14$coefficients[6][1]))),
				modelCoef(1 - SESBTHLvl3.baseRate, pAsthma[iteration], 
				exp(as.numeric(models$AsthmaA2_14$coefficients[7][1])))), 					
				c(	1- pObese[iteration], gender.baseRate, maori.baseRate, 
					pacific.baseRate, asian.baseRate, SESBTHLvl3.baseRate))		
				
			models$AsthmaA2_14$coefficients[1][1]  <- log(intercept/(1-intercept))
			
			asthmaLvl1 <<- predSimBinom(models$AsthmaA2_14)	   
		
		} else {
		
			asthmaLvl1 <<- NAs     
			
		}	

	  
	 # pObese[iteration] <- table(z1ObeseLvl1)[2]/5000
	  
	 	  
	   #browser()
	
	  #print(table(asthmaLvl1)/5000)
	  
	}
 
		
    pre_simulation_setup_MELC <- function() {
      
      # setup constants
      NUMCHILDREN <<- length(A0)
      NAs <<- rep(NA, NUMCHILDREN)
      
      # the propADDHS model uses MEDUC vars which are the inverse of r1stmeduc
      # since r1stmeduc can be modified for scenario testing, we will need
      # to set MEDUC to its (potentially modified) value here
      MEDUCLvl1 <<- r1stmeducLvl3
      MEDUCLvl2 <<- r1stmeducLvl2
      MEDUCLvl3 <<- r1stmeducLvl1
      
      #calculate once at the beginning and use in simulate_family_household  		
      fage_years1_grouped <<- cut(fage_years1, binbreaks$fage_years, labels=FALSE) 
      fage_years1_grouped_same_as_fage <<- fage_years1_grouped == fage
      fage_years1_grouped_same_as_fage[is.na(fage_years1_grouped_same_as_fage)] <<- FALSE
      mage_years1_same_as_MAGE <<- abs(mage_years1 - MAGE) < 2
      
      mage_years <<- MAGE + 1
      #when look at collated means - can see the effect from y2 onwards but doesn't show effect for y1
    }
	
	 store_current_values_in_outcomes <- function(xcol, outcomes) {
      outcomes <<- lapply(outcomes, function(x) {
        x[,xcol] <- get(attr(x,"varname"));x 
      }) 
	  
	  outcomes
    }

    
    do_iteration_year1 <- function() {
      YEAR1 <- 1
      
      store_current_values_in_outcomes(YEAR1, outcomes)
      
      #increment mage_years and fage_years
      #mage_years[mage_years != 99] <<- mage_years[mage_years != 99]+1
      #fage_years[fage_years != 99] <<- fage_years[fage_years != 99]+1
    }
    
    #' Adjust categorical values to desired proportions in cat.adjustments (if any).
    #' 
    #' Does not allow subgroup adjustments.
    #' 
    #' @param x
    #'  categorical values to adjust
    #' @param varname
    #'  varname, used a lookup into cat.adjustments and propensities
    #' @examples
    #'  varname <- "z1msmokeLvl1"
    #'  varname <- "z1singleLvl1"
    #'  adjustCatVar(predSimBinomsSelect(z1single_previousLvl1, models$z1singlePrev0, models$z1singlePrev1), "z1singleLvl1")
    adjustCatVarSimple <- function(x, varname) {
      cat.adjustments <- simenv$cat.adjustments
      
      if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
      
      desiredProps <- cat.adjustments[[varname]][iteration,]
      
      if (any(is.na(desiredProps))) {
        return(x)
      }
      
      cat("Adjusting", varname, ": ", desiredProps, "\n")
      
      modifyProps(x, desiredProps, propensities[[varname]][,,iteration])
    }
    
    #' Adjust categorical values to desired proportions in cat.adjustments (if any).
    #' 
    #' Allows subgroup adjustment if a subgroup expression attribute is attached to the cat.adjustments. 
    #' 
    #' @param x
    #'  categorical values to adjust
    #' @param varname
    #'  varname, used a lookup into cat.adjustments and propensities
    #' @examples
    #'  varname <- "z1msmokeLvl1"
    #'  varname <- "z1singleLvl1"
    #'  adjustCatVar(predSimBinomsSelect(z1single_previousLvl1, models$z1singlePrev0, models$z1singlePrev1), "z1singleLvl1")
    adjustCatVar <- function(x, varname, propens=NULL, desiredProps=NULL) {
      
      cat.adjustments <- simenv$cat.adjustments
      
      varname.no.lvl <- strip_lvl_suffix(varname[1])
      
      if (!varname.no.lvl %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
      
      if (varname %in% c("NPRESCH", "INTERACT", "PUNISH")) {
        iteration <- 1 #just makes it use the 1st (and only) row for cat.adjustments
      }
      if (is.null(desiredProps)) {
        #adjustCatVar is being used for scenario testing - get from cat.adjustments
        desiredProps <- cat.adjustments[[varname.no.lvl]][iteration,]
      }
      
      if (any(is.na(desiredProps))) {
        return(x)
      }
      #attach logisetexpr attribute to desiredProps
      desiredProps <- structure(desiredProps, varname=varname, logisetexpr=attr(cat.adjustments[[varname.no.lvl]], "logisetexpr"), levels=simenv$dict$codings[[varname]])
      
      logiset <- evaluateLogisetExprAttribute(desiredProps, parent.frame())
      
      valid.subgroup <- check.subgroup.expr(cat.adjustments, parent.frame())
      
      if (valid.subgroup==1) {
        cat("Adjusting", varname.no.lvl, ": ", desiredProps, "\n")
        
        adj.x.cat <- adjust.proportions(x, desiredProps, propens, logiset) 
        return(adj.x.cat)
      } else if (valid.subgroup==0) {
        expr <- paste("Scenario adjustments cannot be made for iteration ", iteration, " because the subgroup expression is not defined")
        cat(expr, "\n")
        return(x)
      } else {
        stop("Check valid.subgroup in simulateRun()")
      }
      
    }
    
    
    adjustCatVarCalib <- function(x, varname, propens=NULL, desiredProps=NULL) {
      
      varname.no.lvl <- strip_lvl_suffix(varname[1])
      
      if (varname %in% c("NPRESCH", "INTERACT", "PUNISH")) {
        iteration <- 1 #just makes it use the 1st (and only) row for cat.adjustments
      }
      
      if (any(is.na(desiredProps))) {
        return(x)
      }
      
      desiredProps <- structure(desiredProps, varname=varname, levels=simenv$dict$codings[[varname]])
      
      adj.x.cat <- adjust.proportions(x, desiredProps, propens) 
      return(adj.x.cat)
    }
    
    
    #' Adjust continuous values to desired proportions in cat.adjustments (if any).
    #' 
    #' Does not allow subgroup adjustment
    #' 
    #' @param x
    #' continuous values to adjust
    #' @param varname
    #'  varname, used a lookup into cat.adjustments and propensities
    #' @examples
    #'  
    adjustContVarSimple <- function(x, varname) {
      cat.adjustments <- simenv$cat.adjustments
      
      if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
      
      desiredProps <- cat.adjustments[[varname]][iteration,]
      
      if (any(is.na(desiredProps))) {
        return(x)
      }
      
      cat("Adjusting", varname, ": ", desiredProps, "\n")
      
      catToContModels <- attr(cat.adjustments[[varname]], "catToContModel")
      cont.binbreaks <- attr(cat.adjustments[[varname]], "cont.binbreaks")
      
      adj.x.cont <- modifyPropsContinuous(x, desiredProps, catToContModels, cont.binbreaks, propensities[[varname]][,,iteration])
      
      return(adj.x.cont)
    }
    
    #' Adjust continuous values to desired proportions in cat.adjustments (if any).
    #' 
    #' Allows subgroup adjustment if a global subgroup expression is set. 
    #' 
    #' @param x
    #' continuous values to adjust
    #' @param varname
    #'  varname, used a lookup into cat.adjustments and propensities
    #' @examples
    #' 
    adjustContVar <- function(x, varname, propens=NULL, desiredProps=NULL) {
      cat.adjustments <- simenv$cat.adjustments
      
      if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
      
      if (varname %in% c("INTERACT", "PUNISH", "NPRESCH")) {
        iteration <- 1 #just makes sure the 1st (and only) row from cat.adjustments is used next
      }
      
      if (is.null(desiredProps)) {
        #adjustCatVar is being used for scenario testing - get from cat.adjustments
        desiredProps <- cat.adjustments[[varname]][iteration,]
        
      }
      
      if (any(is.na(desiredProps))) {
        return(x)
      }
      
      #attach logiset attribute to desiredProps
      desiredProps <- structure(desiredProps, varname=varname, logisetexpr=attr(cat.adjustments[[varname]], "logiset"), levels=simenv$dict$codings[[varname]])
      
      logiset <- evaluateLogisetExprAttribute(desiredProps, parent.frame())
      valid.subgroup <- check.subgroup.expr(cat.adjustments, parent.frame())
      
      if (valid.subgroup==1) {
        cat("Adjusting", varname, ": ", desiredProps, "\n")
        
        catToContModels <- attr(cat.adjustments[[varname]], "catToContModel")
        cont.binbreaks <- attr(cat.adjustments[[varname]], "cont.binbreaks")
        
        adj.x.cont <- adjust.proportions(x, desiredProps, propens, logiset, catToContModels, cont.binbreaks, envir=parent.frame())
        return(adj.x.cont)
      } else if (valid.subgroup==0) {
        expr <- paste("Scenario adjustments cannot be made for iteration ", iteration, " because the subgroup expression is not defined")
        cat(expr, "\n")
        return(x)
      } else {
        stop("Check valid.subgroup in simulateRun()")
      }
    }
    
    
    
    adjustContVarCalib <- function(x, varname, propens=NULL, desiredProps=NULL) {
      cat.adjustments <- simenv$cat.adjustments
      
      if (varname %in% c("INTERACT", "PUNISH", "NPRESCH")) {
        iteration <- 1 #just makes sure the 1st (and only) row from cat.adjustments is used next
      }
      
      if (any(is.na(desiredProps))) {
        return(x)
      }
      
      desiredProps <- structure(desiredProps, varname=varname, levels=simenv$dict$codings[[varname]])
      
      catToContModels <- attr(cat.adjustments[[varname]], "catToContModel")
      cont.binbreaks <- attr(cat.adjustments[[varname]], "cont.binbreaks")
      
      adj.x.cont <- adjust.proportions(x, desiredProps, propens, logiset=NULL, catToContModels, cont.binbreaks, envir=parent.frame())
      return(adj.x.cont)
    }
    
    
    
     
    
    #store_current_values_in_outcomes <- function(xcol) {
    #	outcomes <<- lapply(outcomes, function(x) {
    #				x[,xcol] <- get(attr(x,"varname"));x 
    #			}) 
    #}
    
    # setup previous vars with values from current vars, 
    # eg: z1msmoke_previousLvl1 will be assigned the value in z1msmokeLvl1
    store_current_values_in_previous <- function() {
      previous <- attr(simenv$simframe, "previous")
      invisible(mapply(function(var.prev, var.current) {
        #var.prev <- previous[1] ; var.current <- names(previous)[1]
        #var.prev <- "z1msmoke_previousLvl1" ; var.current <- "z1msmokeLvl1"
        assign(var.prev, get(var.current), inherits=T)
        
      }, previous, names(previous)))
    }
    
    
    # SIMULATION STARTS HERE
    # simenv <- env.base
    # simenv <- env.scenario
    
    
    simframe <- simenv$simframe
	
	for(i in names(simframe))
		assign(i, simframe[,i])
	
	#browser()
	
    #attach(simenv$simframe, name="simframe")
    NUM_ITERATIONS <<- 21
    
    outcomes <- createOutcomeMatrices(simframe, "years1_21", c(1:NUM_ITERATIONS))
    
    pre_simulation_setup_MELC()
	
	
    
    do_iteration_year1()
    
	ROW = 1 
	
	#browser()
    for (iteration in 2:NUM_ITERATIONS) {
      #iteration =5
      
      #if(iteration ==5) browser()
      
      #browser()
      
      cat("Run", simenv$num_runs_simulated+1, "year", iteration, "\n")
      
      # setup vars for this iteration
      store_current_values_in_previous()
      age <<- rep(iteration, NUMCHILDREN)
      age_minus1 <<- rep(iteration-1, NUMCHILDREN)
      age_minus1Lvl2 <- as.integer(age_minus1 == 2)
      age_minus1Lvl3 <- as.integer(age_minus1 == 3)
      
	  outcomes <- simulate_childrenOverweight(outcomes)
			
		# sfCat(z1ObeseLvl1)
		 
	  outcomes <- simulate_childrenObese(outcomes)  
      
	 
	  
	  #simulate_asthma()
	  
      #simulate_family_household()
      
      #simulate_psychosocial_factors()
      
      #simulate_employment()
      
      #simulate_material_circumstances()
      
      #simulate_behavioural_factors()
      
      #simulate_depression()
      
      #simulate_health_service_use()
      
      ##simulate_conduct()
      
      #simulate_reading()
      
      #outcomes <- store_current_values_in_outcomes(iteration, outcomes)
      
      #simulate_interact_punish_npresch()
      #cant' get function to work yet
      
      if (iteration==5) {
	  
        #NPRESCH
        mean_householdsize1_5 <- apply(outcomes$householdsize[,1:5], ROW, mean)
        mean_z1accom1_5 <- apply(outcomes$z1accomLvl1[,1:5], ROW, mean)
        mean_chres1_5 <- apply(outcomes$chres[,1:5], ROW, mean) 
        mean_msmoke1_5 <- apply(outcomes$msmoke[,1:5], ROW, mean) 
        NPRESCH <- predSimNorm(models$NPRESCH)
        NPRESCH <- round(NPRESCH)
        NPRESCH[NPRESCH<0] <- 0
        NPRESCH[NPRESCH>3] <- 3
        NPRESCH <- adjustCatVar(NPRESCH, "NPRESCH")
        NPRESCH <<- NPRESCH
        
        
        #INTERACT
        mean_kids1_5 <- apply(outcomes$kids[,1:5], ROW, mean)
        mean_mhrswrk1_5 <- apply(outcomes$mhrswrk[,1:5], ROW, mean)
        mean_welfare1_5 <- apply(outcomes$welfareLvl1[,1:5], ROW, mean) 
        mean_z1accom1_5 <- apply(outcomes$z1accomLvl1[,1:5], ROW, mean) 
        mean_z1overcrowd1_5 <- apply(outcomes$z1overcrowdLvl1[,1:5], ROW, mean) 
        mean_z1chpar1_5 <- apply(outcomes$z1chparLvl1[,1:5], ROW, mean) 
        mean_chres1_5 <- apply(outcomes$chres[,1:5], ROW, mean) 
        INTERACT.pre <- predSimNBinom(models$INTERACT)
        INTERACT <- 10 - INTERACT.pre
        INTERACT[INTERACT<0] <- 0
        INTERACT[INTERACT>10] <- 10
        INTERACT <- adjustContVar(INTERACT, "INTERACT")
        INTERACT <<- INTERACT
        
        #PUNISH
        mean_mhrswrk1_5 <- apply(outcomes$mhrswrk[,1:5], ROW, mean)
        mean_z1homeown1_5 <- apply(outcomes$z1accomLvl1[,1:5], ROW, mean) 
        mean_z1chpar1_5 <- apply(outcomes$z1chparLvl1[,1:5], ROW, mean) 
        PUNISH <- predSimPois(models$PUNISH)
        PUNISH[PUNISH<0] <- 0
        PUNISH[PUNISH>5] <- 5
        PUNISH <- adjustContVar(PUNISH, "PUNISH")
        PUNISH <<- PUNISH
      }
      
    }
    
    #detach("simframe")
    outcomes
  
    
}

