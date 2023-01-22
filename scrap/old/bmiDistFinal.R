




children <- read.csv("base//synthBasefile_MhrswrkFixed_5000_New.csv") 

BMI <- children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])]



BMI[BMI$r1stchildethn==4,grep("^BMI", names(BMI))] <- BMI[BMI$r1stchildethn==4, grep("^BMI", names(BMI))] - 1





#Write back to basefile
children[,c("r1stchildethn", "z1gender", names(children)[grep("^BMI", names(children))])] <- BMI 
write.csv(children, file = "base//synthBasefile_MhrswrkFixed_5000_New1.csv")

