

#get CatToCont model for scores 


# z1ScoreLvl1 <- ifelse(currectScore > 87, 1,0)
# 
# z1DropLvl1 <- ifelse(currectScore <= 80, 1,0)
# 
# z1FailLvl1 <- ifelse(currectScore > 80 & currectScore <= 87, 1,0)


attach(env.base$simframe, name="simframe")


temp <- env.base$modules$run_results$run1$Score[,1]
IQ <- env.base$modules$run_results$run1$IQ[,15]

#Pass
summary(lm(temp ~ z1genderLvl1 + r1stchildethnLvl2 + r1stchildethnLvl3 +
             r1stchildethnLvl4 +
             r1ParentEducLvl2 + r1ParentEducLvl3 + SESBTHLvl2 + SESBTHLvl3 + 
     z1PrintExpLvl1 + z1ECELvl1 + z1ADHDLvl1 + z1ParentInvolveLvl1 + IQ, 
     subset = temp > 87 ))$coef %>% write.csv("temp.csv")

#Fail
summary(lm(temp ~ z1genderLvl1 + r1stchildethnLvl2 + r1stchildethnLvl3 +
             r1stchildethnLvl4 +
             r1ParentEducLvl2 + r1ParentEducLvl3 + SESBTHLvl2 + SESBTHLvl3 + 
             z1PrintExpLvl1 + z1ECELvl1 + z1ADHDLvl1 + z1ParentInvolveLvl1 + IQ, 
           subset = temp > 80 & temp <= 87 ))$coef %>% write.csv("temp.csv")


#Drop
summary(lm(temp ~ z1genderLvl1 + r1stchildethnLvl2 + r1stchildethnLvl3 +
             r1stchildethnLvl4 +
             r1ParentEducLvl2 + r1ParentEducLvl3 + SESBTHLvl2 + SESBTHLvl3 + 
             z1PrintExpLvl1 + z1ECELvl1 + z1ADHDLvl1 + z1ParentInvolveLvl1 + IQ, 
           subset = temp <= 80 ))$coef %>% write.csv("temp.csv")



temp <- env.base$modules$run_results$run1$z1ScoreLvl1[,1]


mylogit <- glm(temp ~ z1genderLvl1 + r1stchildethnLvl2 + r1stchildethnLvl3 +
                 r1stchildethnLvl4 +
                 r1ParentEducLvl2 + r1ParentEducLvl3 + SESBTHLvl2 + SESBTHLvl3 + 
                 z1PrintExpLvl1 + z1ECELvl1 + z1ADHDLvl1 + z1ParentInvolveLvl1 + IQ, 
               family = "binomial")

summary(mylogit)$coef %>% write.csv("temp.csv")


########################################################################################







































