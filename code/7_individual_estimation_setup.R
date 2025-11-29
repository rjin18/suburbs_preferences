library(logr)

#Richard Jin
#Date: 4/18/21

#7_individual_estimation_setup.R
#Prepare data for specs using OLS and IV at the individual level; here we use the ANES and
#CCES survey data to take a deeper look at pol. preferences and separately 
#consider social and economic dimensions. Create an individual-level dataset to feed into
#the next R script where we will actually do the estimation

setwd("/Users/richardjin/Documents/ECON 210A/Project/Data/")

library(plyr)
library(dplyr)
library(foreign)
library(estimatr)
library(zoo)
library(stringr)

#Input
#our previous county-level dataset (1970 counties)
county_output_2class <- read.csv("./Intermediate/counties_step2_logit.csv")

#Load in CCES data for 2012
load("./CCES/commoncontent2012.RData")
CCES_2012 <- x
rm(x)
#keep relevant columns- cc334a = self reported political ideology, cc327 = support for affirmative action, cc328 = deficit financing (spending cuts vs tax increases)
CCES_2012_short <- CCES_2012[,c("V101","birthyr","gender","educ","race","marstat","countyfips","CC334A","CC327","CC328")]

#Load in ANES data (restrict to relevant election years; we can only use 1992 b/c we either don't have the desired vars or are missing county codes for other years)
ANES_1992 <- read.dta("./ANES/anes1992dta/NES1992.dta")

#Process the individual ANES and CCES data

#ANES
#id, county, age, education, gender, race, 'should morality change with the times', 'reduce spending'
ANES_1992_short <- ANES_1992[,c("V923004","V923061","V923903","V923904","V923905","V924201","V924202","V926115","V923701")]
colnames(ANES_1992_short) <- c("id","fips","age","marital","educ","gender","race","morality","spending")
#convert demographic controls
ANES_1992_short$age_sq <- (ANES_1992_short$age)^2
ANES_1992_short$female <- ifelse(as.character(ANES_1992_short$gender) == "2. FEMALE",1,0)
ANES_1992_short$everMarried <- ifelse(ANES_1992_short$marital == "2. NEVER MARRIED" | ANES_1992_short$marital == "0. INAP" | ANES_1992_short$marital == "9. NA" | ANES_1992_short$marital == "7. PARTNERS NOT MARRIED (VOL",0,1)
ANES_1992_short$collegegrad <- ifelse(str_detect(as.character(ANES_1992_short$educ),"16") | str_detect(as.character(ANES_1992_short$educ),"17"),1,0)
ANES_1992_short$race2 <- ifelse(ANES_1992_short$race != "1. WHITE" & ANES_1992_short$race != "2. BLACK","Other",ANES_1992_short$race)
ANES_1992_short$race_factor <- as.factor(ANES_1992_short$race2)
#convert LHS vars to scale
#Should view of morality remain constant as the world keeps on changing, i.e. opposite of progressivism
ANES_1992_short$morality_nochange_scale <- NA
ANES_1992_short$morality_nochange_scale <- ifelse(ANES_1992_short$morality == "5. DISAGREE STRONGLY",1,ANES_1992_short$morality_nochange_scale)
ANES_1992_short$morality_nochange_scale <- ifelse(ANES_1992_short$morality == "4. DISAGREE SOMEWHAT",3/4,ANES_1992_short$morality_nochange_scale)
ANES_1992_short$morality_nochange_scale <- ifelse(ANES_1992_short$morality == "3. NEITHER AGREE NOR DISAGREE",2/4,ANES_1992_short$morality_nochange_scale)
ANES_1992_short$morality_nochange_scale <- ifelse(ANES_1992_short$morality == "2. AGREE SOMEWHAT",1/4,ANES_1992_short$morality_nochange_scale)
ANES_1992_short$morality_nochange_scale <- ifelse(ANES_1992_short$morality == "1. AGREE STRONGLY",0,ANES_1992_short$morality_nochange_scale)
ANES_1992_short$morality_nochange_binary <- ifelse(ANES_1992_short$morality_nochange_scale > 0.5,1,0)
ANES_1992_short$morality_nochange_binary <- ifelse(is.na(ANES_1992_short$morality_nochange_scale),NA,ANES_1992_short$morality_nochange_binary)
#Should government reduce spending
ANES_1992_short$spendingcut_scale <- NA
ANES_1992_short$spendingcut_scale <- ifelse(ANES_1992_short$spending == 1,1,ANES_1992_short$spendingcut_scale)
ANES_1992_short$spendingcut_scale <- ifelse(ANES_1992_short$spending == 2,5/6,ANES_1992_short$spendingcut_scale)
ANES_1992_short$spendingcut_scale <- ifelse(ANES_1992_short$spending == 3,4/6,ANES_1992_short$spendingcut_scale)
ANES_1992_short$spendingcut_scale <- ifelse(ANES_1992_short$spending == 4 | ANES_1992_short$spending == 0,3/6,ANES_1992_short$spendingcut_scale)
ANES_1992_short$spendingcut_scale <- ifelse(ANES_1992_short$spending == 5,2/6,ANES_1992_short$spendingcut_scale)
ANES_1992_short$spendingcut_scale <- ifelse(ANES_1992_short$spending == 6,1/6,ANES_1992_short$spendingcut_scale)
ANES_1992_short$spendingcut_scale <- ifelse(ANES_1992_short$spending == 7,0,ANES_1992_short$spendingcut_scale)
ANES_1992_short$spendingcut_binary <- ifelse(ANES_1992_short$spendingcut_scale > 0.5,1,0)
ANES_1992_short$spendingcut_binary <- ifelse(is.na(ANES_1992_short$spendingcut_scale),NA,ANES_1992_short$spendingcut_binary)
#Convert fips to GISJOIN
temp <- paste(as.character(ANES_1992_short$fips),"0",sep = "")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
countycode <- substrRight(temp,4) #4-digit county; now we need the 3-digit state
state1 <- ifelse(nchar(temp) == 6,substr(temp,1,2),substr(temp,1,1))
statecode <- ifelse(nchar(state1) == 2,paste(state1,"0",sep=""),paste("0",state1,"0",sep=""))
ANES_1992_short$GISJOIN <- paste("G",statecode,countycode,sep="")
rm(temp,countycode,state1,statecode)
#keep relevant vars
ANES_1992_final <- ANES_1992_short[,c("id","GISJOIN","age","age_sq","everMarried","female","collegegrad","race_factor","morality_nochange_scale","morality_nochange_binary","spendingcut_scale","spendingcut_binary")]

#CCES
colnames(CCES_2012_short) <- c("id","birthyr","gender","educ","race","marstat","fips","politics_selfreport","AA_support","deficit_financing")
#convert demographic controls
birthyr_num <- as.numeric(as.character(CCES_2012_short$birthyr))
CCES_2012_short$age <- 2012 - birthyr_num
CCES_2012_short$age_sq <- (CCES_2012_short$age)^2
CCES_2012_short$female <- ifelse(CCES_2012_short$gender == "Female",1,0)
CCES_2012_short$collegegrad <- ifelse(CCES_2012_short$educ == "4-year" | CCES_2012_short$educ == "Post-grad",1,0)
CCES_2012_short$race2 <- ifelse(CCES_2012_short$race != "White" & CCES_2012_short$race != "Black" & CCES_2012_short$race != "Hispanic","Other",CCES_2012_short$race)
CCES_2012_short$race_factor <- as.factor(CCES_2012_short$race2)
CCES_2012_short$everMarried <- ifelse(CCES_2012_short$marstat == "Single" | CCES_2012_short$marstat == "Domestic partnership" | is.na(CCES_2012_short$marstat),0,1)
#convert LHS vars to scale
#self-reported conservative
CCES_2012_short$conservative_scale <- NA
CCES_2012_short$conservative_scale <- ifelse(CCES_2012_short$politics_selfreport == "Very Conservative",1,CCES_2012_short$conservative_scale)
CCES_2012_short$conservative_scale <- ifelse(CCES_2012_short$politics_selfreport == "Conservative",5/6,CCES_2012_short$conservative_scale)
CCES_2012_short$conservative_scale <- ifelse(CCES_2012_short$politics_selfreport == "Somewhat Conservative",4/6,CCES_2012_short$conservative_scale)
CCES_2012_short$conservative_scale <- ifelse(CCES_2012_short$politics_selfreport == "Middle of the Road" | CCES_2012_short$politics_selfreport == "Not Sure",3/6,CCES_2012_short$conservative_scale)
CCES_2012_short$conservative_scale <- ifelse(CCES_2012_short$politics_selfreport == "Somewhat Liberal",2/6,CCES_2012_short$conservative_scale)
CCES_2012_short$conservative_scale <- ifelse(CCES_2012_short$politics_selfreport == "Liberal",1/6,CCES_2012_short$conservative_scale)
CCES_2012_short$conservative_scale <- ifelse(CCES_2012_short$politics_selfreport == "Very Liberal",0,CCES_2012_short$conservative_scale)
CCES_2012_short$conservative_binary <- ifelse(CCES_2012_short$conservative_scale > 0.5,1,0)
#support AA
CCES_2012_short$AA_oppose_binary <- ifelse(str_detect(CCES_2012_short$AA_support,"oppose"),1,0)
#support spending cut to curb deficit (as oppose to raising taxes)
CCES_2012_short$spending_cut <- ifelse(str_detect(CCES_2012_short$deficit_financing,"Cut"),1,0)
#Convert fips to GISJOIN
temp <- paste(CCES_2012_short$fips,"0",sep = "")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
countycode <- substrRight(temp,4) #4-digit county; now we need the 3-digit state
statecode <- paste(substr(temp,1,2),"0",sep="")
CCES_2012_short$GISJOIN <- paste("G",statecode,countycode,sep="")
rm(temp,countycode,statecode)
#keep relevant vars
CCES_2012_final <- CCES_2012_short[,c("id","GISJOIN","age","age_sq","female","collegegrad","race_factor","everMarried","conservative_scale","conservative_binary","AA_oppose_binary","spending_cut")]

#Fix Dade county in CCES- ANES is fine b/c 1992 is before the change from fips 12025 to 12086
CCES_2012_final$GISJOIN <- ifelse(CCES_2012_final$GISJOIN == "G1200860","G1200250",CCES_2012_final$GISJOIN)

#Merge ANES and CCES with county-level vars
county_vars <- county_output_2class[,c("GISJOIN","area_sqmi","county_popdensity_sqmile","MSACMSA","Subregion","Prob_Suburb_logit")]
ANES_1992_output <- inner_join(ANES_1992_final,county_vars,by = "GISJOIN")
CCES_2012_output <- inner_join(CCES_2012_final,county_vars,by = "GISJOIN")

#Save output
write.csv(ANES_1992_output,"./Intermediate/ANES_1992_step7.csv")
write.csv(CCES_2012_output,"./Intermediate/CCES_2012_step7.csv")

