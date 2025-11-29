library(logr)

#Richard Jin
#Date: 4/25/21

#9_individual_IV_noMar.R
#Run individual-level regressions using 1992 ANES and 2012 CCES surveys
#Start with the data created in 7_XX.R
#LHS is social/economic preferences, RHS is county-level suburbanization
#measure, as before, with individual-level demographic controls
#Run specs using highway development as an instrument for suburbanization
#Highway data from Baum-Snow (2007) replication files

setwd("/Users/richardjin/Documents/ECON 210A/Project")

library(plyr)
library(dplyr)
library(foreign)
library(estimatr)

#Input
ANES_1992_estimation_sample <- read.csv("./Data/Intermediate/ANES_1992_step7.csv")
CCES_2012_estimation_sample <- read.csv("./Data/Intermediate/CCES_2012_step7.csv")

#first take log population density
ANES_1992_estimation_sample$log_popd <- log(ANES_1992_estimation_sample$county_popdensity_sqmile)
CCES_2012_estimation_sample$log_popd <- log(CCES_2012_estimation_sample$county_popdensity_sqmile)

#Load in highway data from Baum-Snow- first load in actual highway construction
hwy_data <- read.dta("./Data/hwy-allyr-cnty.dta")
#restrict to 1970 and create a GISJOIN var
hwy70 <- hwy_data[hwy_data$year == 70,]
statechar <- as.character(hwy70$statefips)
state <- ifelse(nchar(statechar) == 2,paste(statechar,"0",sep=""),paste("0",statechar,"0",sep=""))
countychar <- as.character(hwy70$cntyfips)
county <- ifelse(nchar(countychar) == 3,paste(countychar,"0",sep=""),countychar)
county <- ifelse(nchar(countychar) == 2,paste("0",countychar,"0",sep=""),county)
county <- ifelse(nchar(countychar) == 1,paste("00",countychar,"0",sep=""),county)
hwy70$GISJOIN <- paste("G",state,county,sep="")
rm(statechar,state,countychar,county)
hwy70 <- hwy70[,-c(1,2)]

#merge on highway data
ANES_1992_estimation_sample <- left_join(ANES_1992_estimation_sample,hwy70,by = "GISJOIN") 
missingANES <- ANES_1992_estimation_sample[is.na(ANES_1992_estimation_sample$lena),] #keep failed join counties for reference
ANES_1992_estimation_sample_IV <- ANES_1992_estimation_sample[!is.na(ANES_1992_estimation_sample$lena),]

CCES_2012_estimation_sample <- left_join(CCES_2012_estimation_sample,hwy70,by = "GISJOIN") 
missingCCES <- CCES_2012_estimation_sample[is.na(CCES_2012_estimation_sample$lena),] #keep failed join counties for reference
CCES_2012_estimation_sample_IV <- CCES_2012_estimation_sample[!is.na(CCES_2012_estimation_sample$lena),]

#highway mileage is total but does not factor in county size, so apply an adjustment
ANES_1992_estimation_sample_IV$fira_adj <- ANES_1992_estimation_sample_IV$fira/ANES_1992_estimation_sample_IV$area_sqmi
CCES_2012_estimation_sample_IV$fira_adj <- CCES_2012_estimation_sample_IV$fira/CCES_2012_estimation_sample_IV$area_sqmi

#2SLS, with demographic controls and Subregion FE for 4 election years; run 2 specs, with RHS as Pr(Suburb) and as log(population density)
#use 'fira' as the instrument, i.e. federally funded highway miles
#ANES
IVfira_1992_nocont_social_RHSprsub <- iv_robust(morality_nochange_scale ~ Prob_Suburb_logit | fira_adj, data = ANES_1992_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
IVfira_1992_nocont_social_RHSlog_popd <- iv_robust(morality_nochange_scale ~ log_popd | fira_adj, data = ANES_1992_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)

IVfira_1992_nocont_econ_RHSprsub <- iv_robust(spendingcut_scale ~ Prob_Suburb_logit | fira_adj, data = ANES_1992_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
IVfira_1992_nocont_econ_RHSlog_popd <- iv_robust(spendingcut_scale ~ log_popd | fira_adj, data = ANES_1992_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)

IVfira_1992_wcont_social_RHSprsub <- iv_robust(morality_nochange_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
IVfira_1992_wcont_social_RHSlog_popd <- iv_robust(morality_nochange_scale ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)

IVfira_1992_wcont_econ_RHSprsub <- iv_robust(spendingcut_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
IVfira_1992_wcont_econ_RHSlog_popd <- iv_robust(spendingcut_scale ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
#CCES
IVfira_2012_nocont_social_RHSprsub <- iv_robust(AA_oppose_binary ~ Prob_Suburb_logit | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
IVfira_2012_nocont_social_RHSlog_popd <- iv_robust(AA_oppose_binary ~ log_popd | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)

IVfira_2012_nocont_econ_RHSprsub <- iv_robust(spending_cut ~ Prob_Suburb_logit | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
IVfira_2012_nocont_econ_RHSlog_popd <- iv_robust(spending_cut ~ log_popd | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)

IVfira_2012_wcont_social_RHSprsub <- iv_robust(AA_oppose_binary ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
IVfira_2012_wcont_social_RHSlog_popd <- iv_robust(AA_oppose_binary ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)

IVfira_2012_wcont_econ_RHSprsub <- iv_robust(spending_cut ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
IVfira_2012_wcont_econ_RHSlog_popd <- iv_robust(spending_cut ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", diagnostics = TRUE)


#Run first stage specs to get output
FSfira_1992_nocont_social_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj, data = ANES_1992_estimation_sample_IV[!is.na(ANES_1992_estimation_sample_IV$morality_nochange_scale),])
FSfira_1992_nocont_social_RHSlog_popd <- lm(log_popd ~ fira_adj, data = ANES_1992_estimation_sample_IV[!is.na(ANES_1992_estimation_sample_IV$morality_nochange_scale),])

FSfira_1992_nocont_econ_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj, data = ANES_1992_estimation_sample_IV[!is.na(ANES_1992_estimation_sample_IV$spendingcut_scale),])
FSfira_1992_nocont_econ_RHSlog_popd <- lm(log_popd ~ fira_adj, data = ANES_1992_estimation_sample_IV[!is.na(ANES_1992_estimation_sample_IV$spendingcut_scale),])

FSfira_1992_wcont_social_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV[!is.na(ANES_1992_estimation_sample_IV$morality_nochange_scale),])
FSfira_1992_wcont_social_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV[!is.na(ANES_1992_estimation_sample_IV$morality_nochange_scale),])

FSfira_1992_wcont_econ_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV[!is.na(ANES_1992_estimation_sample_IV$spendingcut_scale),])
FSfira_1992_wcont_econ_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV[!is.na(ANES_1992_estimation_sample_IV$spendingcut_scale),])
#CCES
FSfira_2012_nocont_social_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])
FSfira_2012_nocont_social_RHSlog_popd <- lm(log_popd ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])

FSfira_2012_nocont_econ_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])
FSfira_2012_nocont_econ_RHSlog_popd <- lm(log_popd ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])

FSfira_2012_wcont_social_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])
FSfira_2012_wcont_social_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])

FSfira_2012_wcont_econ_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])
FSfira_2012_wcont_econ_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])


#OLS for comparison
#ANES
OLS_1992_nocont_social_RHSprsub <- lm(morality_nochange_scale ~ Prob_Suburb_logit, data = ANES_1992_estimation_sample_IV)
OLS_1992_nocont_social_RHSlog_popd <- lm(morality_nochange_scale ~ log_popd, data = ANES_1992_estimation_sample_IV)

OLS_1992_nocont_econ_RHSprsub <- lm(spendingcut_scale ~ Prob_Suburb_logit, data = ANES_1992_estimation_sample_IV)
OLS_1992_nocont_econ_RHSlog_popd <- lm(spendingcut_scale ~ log_popd, data = ANES_1992_estimation_sample_IV)

OLS_1992_wcont_social_RHSprsub <- lm(morality_nochange_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV)
OLS_1992_wcont_social_RHSlog_popd <- lm(morality_nochange_scale ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV)

OLS_1992_wcont_econ_RHSprsub <- lm(spendingcut_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV)
OLS_1992_wcont_econ_RHSlog_popd <- lm(spendingcut_scale ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion, data = ANES_1992_estimation_sample_IV)
#CCES
OLS_2012_nocont_social_RHSprsub <- lm(AA_oppose_binary ~ Prob_Suburb_logit, data = CCES_2012_estimation_sample_IV)
OLS_2012_nocont_social_RHSlog_popd <- lm(AA_oppose_binary ~ log_popd, data = CCES_2012_estimation_sample_IV)

OLS_2012_nocont_econ_RHSprsub <- lm(spending_cut ~ Prob_Suburb_logit, data = CCES_2012_estimation_sample_IV)
OLS_2012_nocont_econ_RHSlog_popd <- lm(spending_cut ~ log_popd, data = CCES_2012_estimation_sample_IV)

OLS_2012_wcont_social_RHSprsub <- lm(AA_oppose_binary ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wcont_social_RHSlog_popd <- lm(AA_oppose_binary ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)

OLS_2012_wcont_econ_RHSprsub <- lm(spending_cut ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wcont_econ_RHSlog_popd <- lm(spending_cut ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)


#Output

#Table- OLS with the IV sample, LHS social
stargazer(OLS_1992_nocont_social_RHSprsub, OLS_1992_wcont_social_RHSprsub,
          OLS_2012_nocont_social_RHSprsub,OLS_2012_wcont_social_RHSprsub, align=TRUE, header = FALSE,
          keep=c("Prob_Suburb_logit"), covariate.labels=c("Pr(Suburb)"),
          se = starprep(OLS_1992_nocont_social_RHSprsub, OLS_1992_wcont_social_RHSprsub,
                        OLS_2012_nocont_social_RHSprsub,OLS_2012_wcont_social_RHSprsub, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/9_individual_social_OLS_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(OLS_1992_nocont_social_RHSlog_popd, OLS_1992_wcont_social_RHSlog_popd,
          OLS_2012_nocont_social_RHSlog_popd,OLS_2012_wcont_social_RHSlog_popd, align=TRUE, header = FALSE,
          keep=c("log_popd"), covariate.labels=c("$log$(Population Density)"),
          se = starprep(OLS_1992_nocont_social_RHSlog_popd, OLS_1992_wcont_social_RHSlog_popd,
                        OLS_2012_nocont_social_RHSlog_popd,OLS_2012_wcont_social_RHSlog_popd, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/9_individual_social_OLS_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

#Table- OLS with the IV sample, LHS econ
stargazer(OLS_1992_nocont_econ_RHSprsub, OLS_1992_wcont_econ_RHSprsub,
          OLS_2012_nocont_econ_RHSprsub,OLS_2012_wcont_econ_RHSprsub, align=TRUE, header = FALSE,
          keep=c("Prob_Suburb_logit"), covariate.labels=c("Pr(Suburb)"),
          se = starprep(OLS_1992_nocont_econ_RHSprsub, OLS_1992_wcont_econ_RHSprsub,
                        OLS_2012_nocont_econ_RHSprsub,OLS_2012_wcont_econ_RHSprsub, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/9_individual_econ_OLS_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(OLS_1992_nocont_econ_RHSlog_popd, OLS_1992_wcont_econ_RHSlog_popd,
          OLS_2012_nocont_econ_RHSlog_popd,OLS_2012_wcont_econ_RHSlog_popd, align=TRUE, header = FALSE,
          keep=c("log_popd"), covariate.labels=c("$log$(Population Density)"),
          se = starprep(OLS_1992_nocont_econ_RHSlog_popd, OLS_1992_wcont_econ_RHSlog_popd,
                        OLS_2012_nocont_econ_RHSlog_popd,OLS_2012_wcont_econ_RHSlog_popd, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/9_individual_econ_OLS_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)


#Table- IV first stage- LHS social
stargazer(FSfira_1992_nocont_social_RHSprsub,FSfira_1992_wcont_social_RHSprsub,
          FSfira_2012_nocont_social_RHSprsub,FSfira_2012_wcont_social_RHSprsub,align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_1992_nocont_social_RHSprsub,FSfira_1992_wcont_social_RHSprsub,
                        FSfira_2012_nocont_social_RHSprsub,FSfira_2012_wcont_social_RHSprsub, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/9_individual_social_IV_firstStage_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(FSfira_1992_nocont_social_RHSlog_popd,FSfira_1992_wcont_social_RHSlog_popd,
          FSfira_2012_nocont_social_RHSlog_popd,FSfira_2012_wcont_social_RHSlog_popd,align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_1992_nocont_social_RHSlog_popd,FSfira_1992_wcont_social_RHSlog_popd,
                        FSfira_2012_nocont_social_RHSlog_popd,FSfira_2012_wcont_social_RHSlog_popd, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/9_individual_social_IV_firstStage_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

#Table- IV first stage- LHS econ
stargazer(FSfira_1992_nocont_econ_RHSprsub,FSfira_1992_wcont_econ_RHSprsub,
          FSfira_2012_nocont_econ_RHSprsub,FSfira_2012_wcont_econ_RHSprsub,align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_1992_nocont_econ_RHSprsub,FSfira_1992_wcont_econ_RHSprsub,
                        FSfira_2012_nocont_econ_RHSprsub,FSfira_2012_wcont_econ_RHSprsub, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/9_individual_econ_IV_firstStage_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(FSfira_1992_nocont_econ_RHSlog_popd,FSfira_1992_wcont_econ_RHSlog_popd,
          FSfira_2012_nocont_econ_RHSlog_popd,FSfira_2012_wcont_econ_RHSlog_popd,align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_1992_nocont_econ_RHSlog_popd,FSfira_1992_wcont_econ_RHSlog_popd,
                        FSfira_2012_nocont_econ_RHSlog_popd,FSfira_2012_wcont_econ_RHSlog_popd, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/9_individual_econ_IV_firstStage_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

#Table- IV second stage
#We will create this manually from looking at the summary of iv_robust
summary(IVfira_1992_nocont_social_RHSlog_popd)
summary(IVfira_1992_wcont_social_RHSlog_popd)
summary(IVfira_2012_nocont_social_RHSlog_popd)
summary(IVfira_2012_wcont_social_RHSlog_popd)

summary(IVfira_1992_nocont_econ_RHSlog_popd)
summary(IVfira_1992_wcont_econ_RHSlog_popd)
summary(IVfira_2012_nocont_econ_RHSlog_popd)
summary(IVfira_2012_wcont_econ_RHSlog_popd)

summary(IVfira_1992_nocont_social_RHSprsub)
summary(IVfira_1992_wcont_social_RHSprsub)
summary(IVfira_2012_nocont_social_RHSprsub)
summary(IVfira_2012_wcont_social_RHSprsub)

summary(IVfira_1992_nocont_econ_RHSprsub)
summary(IVfira_1992_wcont_econ_RHSprsub)
summary(IVfira_2012_nocont_econ_RHSprsub)
summary(IVfira_2012_wcont_econ_RHSprsub)



