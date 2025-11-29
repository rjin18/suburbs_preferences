library(logr)

#Richard Jin
#Date: 4/25/21

#11_individual_tables.R
#From 9_XX and 10_XX scripts, pick out preferred specs and create tables

#CLUSTER SEs AT THE COUNTY LEVEL

#Restrict sample to individuals who answered all 3 questions

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

#Last sample restriction- must have answered all three questions
CCES_2012_estimation_sample_IV <- CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$conservative_scale) & !is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary) & !is.na(CCES_2012_estimation_sample_IV$spending_cut),]

#2SLS, with demographic controls and Subregion FE for 4 election years; run 2 specs, with RHS as Pr(Suburb) and as log(population density)
#use 'fira' as the instrument, i.e. federally funded highway miles
#CCES
IVfira_2012_nocont_idea_RHSprsub <- iv_robust(conservative_scale ~ Prob_Suburb_logit | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_nocont_idea_RHSlog_popd <- iv_robust(conservative_scale ~ log_popd | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wcont_idea_RHSprsub <- iv_robust(conservative_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wcont_idea_RHSlog_popd <- iv_robust(conservative_scale ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wMar_idea_RHSprsub <- iv_robust(conservative_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wMar_idea_RHSlog_popd <- iv_robust(conservative_scale ~ log_popd+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)

IVfira_2012_nocont_social_RHSprsub <- iv_robust(AA_oppose_binary ~ Prob_Suburb_logit | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_nocont_social_RHSlog_popd <- iv_robust(AA_oppose_binary ~ log_popd | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wcont_social_RHSprsub <- iv_robust(AA_oppose_binary ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wcont_social_RHSlog_popd <- iv_robust(AA_oppose_binary ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wMar_social_RHSprsub <- iv_robust(AA_oppose_binary ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wMar_social_RHSlog_popd <- iv_robust(AA_oppose_binary ~ log_popd+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)

IVfira_2012_nocont_econ_RHSprsub <- iv_robust(spending_cut  ~ Prob_Suburb_logit | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_nocont_econ_RHSlog_popd <- iv_robust(spending_cut  ~ log_popd | fira_adj, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wcont_econ_RHSprsub <- iv_robust(spending_cut  ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wcont_econ_RHSlog_popd <- iv_robust(spending_cut  ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wMar_econ_RHSprsub <- iv_robust(spending_cut ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN, diagnostics = TRUE)
IVfira_2012_wMar_econ_RHSlog_popd <- iv_robust(spending_cut ~ log_popd+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion | fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV,se_type = "stata",clusters = CCES_2012_estimation_sample_IV$GISJOIN,  diagnostics = TRUE)


#Run first stage specs to get output
#CCES
FSfira_2012_nocont_idea_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$conservative_scale),])
FSfira_2012_nocont_idea_RHSlog_popd <- lm(log_popd ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$conservative_scale),])
FSfira_2012_wcont_idea_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$conservative_scale),])
FSfira_2012_wcont_idea_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$conservative_scale),])
FSfira_2012_wMar_idea_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$conservative_scale),])
FSfira_2012_wMar_idea_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$conservative_scale),])

FSfira_2012_nocont_social_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])
FSfira_2012_nocont_social_RHSlog_popd <- lm(log_popd ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])
FSfira_2012_wcont_social_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])
FSfira_2012_wcont_social_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])
FSfira_2012_wMar_social_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])
FSfira_2012_wMar_social_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$AA_oppose_binary),])

FSfira_2012_nocont_econ_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])
FSfira_2012_nocont_econ_RHSlog_popd <- lm(log_popd ~ fira_adj, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])
FSfira_2012_wcont_econ_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])
FSfira_2012_wcont_econ_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])
FSfira_2012_wMar_econ_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])
FSfira_2012_wMar_econ_RHSlog_popd <- lm(log_popd ~ fira_adj+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV[!is.na(CCES_2012_estimation_sample_IV$spending_cut),])


#OLS for comparison
#CCES
OLS_2012_nocont_idea_RHSprsub <- lm(conservative_scale ~ Prob_Suburb_logit, data = CCES_2012_estimation_sample_IV)
OLS_2012_nocont_idea_RHSlog_popd <- lm(conservative_scale ~ log_popd, data = CCES_2012_estimation_sample_IV)
OLS_2012_wcont_idea_RHSprsub <- lm(conservative_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wcont_idea_RHSlog_popd <- lm(conservative_scale ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wMar_idea_RHSprsub <- lm(conservative_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wMar_idea_RHSlog_popd <- lm(conservative_scale ~ log_popd+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV)

OLS_2012_nocont_social_RHSprsub <- lm(AA_oppose_binary ~ Prob_Suburb_logit, data = CCES_2012_estimation_sample_IV)
OLS_2012_nocont_social_RHSlog_popd <- lm(AA_oppose_binary ~ log_popd, data = CCES_2012_estimation_sample_IV)
OLS_2012_wcont_social_RHSprsub <- lm(AA_oppose_binary ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wcont_social_RHSlog_popd <- lm(AA_oppose_binary ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wMar_social_RHSprsub <- lm(AA_oppose_binary ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wMar_social_RHSlog_popd <- lm(AA_oppose_binary ~ log_popd+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV)

OLS_2012_nocont_econ_RHSprsub <- lm(spending_cut ~ Prob_Suburb_logit, data = CCES_2012_estimation_sample_IV)
OLS_2012_nocont_econ_RHSlog_popd <- lm(spending_cut ~ log_popd, data = CCES_2012_estimation_sample_IV)
OLS_2012_wcont_econ_RHSprsub <- lm(spending_cut ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wcont_econ_RHSlog_popd <- lm(spending_cut ~ log_popd+age+age_sq+race_factor+collegegrad+female+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wMar_econ_RHSprsub <- lm(spending_cut ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV)
OLS_2012_wMar_econ_RHSlog_popd <- lm(spending_cut ~ log_popd+age+age_sq+race_factor+collegegrad+female+everMarried+Subregion, data = CCES_2012_estimation_sample_IV)


#First set of tables- 6 cols, 3 pairs (w/out and w/ controls), 3 outcomes (self-reported conservatism, AA opposition, spending cut)

#OLS
stargazer(OLS_2012_nocont_idea_RHSprsub, OLS_2012_wcont_idea_RHSprsub,
          OLS_2012_nocont_social_RHSprsub, OLS_2012_wcont_social_RHSprsub,
          OLS_2012_nocont_econ_RHSprsub,OLS_2012_wcont_econ_RHSprsub, align=TRUE, header = FALSE,
          keep=c("Prob_Suburb_logit"), covariate.labels=c("Pr(Suburb)"),
          se = starprep(OLS_2012_nocont_idea_RHSprsub, OLS_2012_wcont_idea_RHSprsub,
                        OLS_2012_nocont_social_RHSprsub, OLS_2012_wcont_social_RHSprsub,
                        OLS_2012_nocont_econ_RHSprsub,OLS_2012_wcont_econ_RHSprsub, se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/11_individual_OLS_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(OLS_2012_nocont_idea_RHSlog_popd, OLS_2012_wcont_idea_RHSlog_popd,
          OLS_2012_nocont_social_RHSlog_popd, OLS_2012_wcont_social_RHSlog_popd,
          OLS_2012_nocont_econ_RHSlog_popd,OLS_2012_wcont_econ_RHSlog_popd, align=TRUE, header = FALSE,
          keep=c("log_popd"), covariate.labels=c("$log$(Population Density)"),
          se = starprep(OLS_2012_nocont_idea_RHSlog_popd, OLS_2012_wcont_idea_RHSlog_popd,
                        OLS_2012_nocont_social_RHSlog_popd, OLS_2012_wcont_social_RHSlog_popd,
                        OLS_2012_nocont_econ_RHSlog_popd,OLS_2012_wcont_econ_RHSlog_popd, se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/11_individual_OLS_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

#Table- IV first stage
stargazer(FSfira_2012_nocont_idea_RHSprsub, FSfira_2012_wcont_idea_RHSprsub,
          FSfira_2012_nocont_social_RHSprsub, FSfira_2012_wcont_social_RHSprsub,
          FSfira_2012_nocont_econ_RHSprsub,FSfira_2012_wcont_econ_RHSprsub, align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_2012_nocont_idea_RHSprsub, FSfira_2012_wcont_idea_RHSprsub,
                        FSfira_2012_nocont_social_RHSprsub, FSfira_2012_wcont_social_RHSprsub,
                        FSfira_2012_nocont_econ_RHSprsub,FSfira_2012_wcont_econ_RHSprsub, se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/11_individual_IV_firstStage_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(FSfira_2012_nocont_idea_RHSlog_popd, FSfira_2012_wcont_idea_RHSlog_popd,
          FSfira_2012_nocont_social_RHSlog_popd, FSfira_2012_wcont_social_RHSlog_popd,
          FSfira_2012_nocont_econ_RHSlog_popd,FSfira_2012_wcont_econ_RHSlog_popd, align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_2012_nocont_idea_RHSlog_popd, FSfira_2012_wcont_idea_RHSlog_popd,
                        FSfira_2012_nocont_social_RHSlog_popd, FSfira_2012_wcont_social_RHSlog_popd,
                        FSfira_2012_nocont_econ_RHSlog_popd,FSfira_2012_wcont_econ_RHSlog_popd, se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/11_individual_IV_firstStage_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)


#Table- IV second stage
#We will create this manually from looking at the summary of iv_robust
summary(IVfira_2012_nocont_idea_RHSlog_popd)
summary(IVfira_2012_wcont_idea_RHSlog_popd)
summary(IVfira_2012_nocont_social_RHSlog_popd)
summary(IVfira_2012_wcont_social_RHSlog_popd)
summary(IVfira_2012_nocont_econ_RHSlog_popd)
summary(IVfira_2012_wcont_econ_RHSlog_popd)

summary(IVfira_2012_nocont_idea_RHSprsub)
summary(IVfira_2012_wcont_idea_RHSprsub)
summary(IVfira_2012_nocont_social_RHSprsub)
summary(IVfira_2012_wcont_social_RHSprsub)
summary(IVfira_2012_nocont_econ_RHSprsub)
summary(IVfira_2012_wcont_econ_RHSprsub)


#Next set of tables- 3 cols, all with controls+everMarried, 3 outcomes (self-reported conservatism, AA opposition, spending cut)

#OLS
stargazer(OLS_2012_wMar_idea_RHSprsub, OLS_2012_wMar_social_RHSprsub,OLS_2012_wMar_econ_RHSprsub, align=TRUE, header = FALSE,
          keep=c("Prob_Suburb_logit"), covariate.labels=c("Pr(Suburb)"),
          se = starprep(OLS_2012_wMar_idea_RHSprsub, OLS_2012_wMar_social_RHSprsub,OLS_2012_wMar_econ_RHSprsub, se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/11_individual_Mar_OLS_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(OLS_2012_wMar_idea_RHSlog_popd, OLS_2012_wMar_social_RHSlog_popd, OLS_2012_wMar_econ_RHSlog_popd, align=TRUE, header = FALSE,
          keep=c("log_popd"), covariate.labels=c("$log$(Population Density)"),
          se = starprep(OLS_2012_wMar_idea_RHSlog_popd, OLS_2012_wMar_social_RHSlog_popd, OLS_2012_wMar_econ_RHSlog_popd, se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/11_individual_Mar_OLS_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

#Table- IV first stage
stargazer(FSfira_2012_wMar_idea_RHSprsub, FSfira_2012_wMar_social_RHSprsub, FSfira_2012_wMar_econ_RHSprsub, align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_2012_wMar_idea_RHSprsub, FSfira_2012_wMar_social_RHSprsub, FSfira_2012_wMar_econ_RHSprsub, se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/11_individual_Mar_IV_firstStage_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(FSfira_2012_wMar_idea_RHSlog_popd, FSfira_2012_wMar_social_RHSlog_popd, FSfira_2012_wMar_econ_RHSlog_popd, align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_2012_wMar_idea_RHSlog_popd, FSfira_2012_wMar_social_RHSlog_popd, FSfira_2012_wMar_econ_RHSlog_popd, se_type = "stata", clusters = CCES_2012_estimation_sample_IV$GISJOIN),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/11_individual_Mar_IV_firstStage_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)


#Table- IV second stage
#We will create this manually from looking at the summary of iv_robust
summary(IVfira_2012_wMar_idea_RHSlog_popd)
summary(IVfira_2012_wMar_social_RHSlog_popd)
summary(IVfira_2012_wMar_econ_RHSlog_popd)

summary(IVfira_2012_wMar_idea_RHSprsub)
summary(IVfira_2012_wMar_social_RHSprsub)
summary(IVfira_2012_wMar_econ_RHSprsub)




