library(logr)

#Richard Jin
#Date: 4/17/21

#6_estimation_election_IV.R
#Run specs using highway development as an instrument for suburbanization
#Highway data from Baum-Snow (2007) replication files

setwd("/Users/richardjin/Documents/ECON 210A/Project")

library(plyr)
library(dplyr)
library(foreign)
library(estimatr)
library(AER)

#Input
election_estimation_sample <- read.csv("./Data/Intermediate/counties_estimation_sample_step3.csv")
#first take log population density
election_estimation_sample$log_popd <- log(election_estimation_sample$county_popdensity_sqmile)

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
estimation_sample_merge <- left_join(election_estimation_sample,hwy70,by = "GISJOIN") #we fail to join 127 counties
missing <- estimation_sample_merge[is.na(estimation_sample_merge$lena),] #keep failed join counties for reference
estimation_sample_IV <- estimation_sample_merge[!is.na(estimation_sample_merge$lena),]

#highway mileage is total but does not factor in county size, so apply an adjustment
estimation_sample_IV$fira_adj <- estimation_sample_IV$fira/estimation_sample_IV$area_sqmi #total 1970

#2SLS, with demographic controls and Subregion FE for 4 election years; run 2 specs, with RHS as Pr(Suburb) and as log(population density)
#use 'fira' as the instrument, i.e. federally funded highway miles
IVfira_1980_wcont_LHSrep_RHSprsub <- iv_robust(RepublicanShare1980 ~ Prob_Suburb_logit+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion | fira_adj+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion, data = estimation_sample_IV, se_type = "stata",diagnostics = TRUE)
IVfira_1980_wcont_LHSrep_RHSlog_popd <- iv_robust(RepublicanShare1980 ~ log_popd+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion | fira_adj+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion, data = estimation_sample_IV, se_type = "stata",diagnostics = TRUE)

IVfira_1992_wcont_LHSrep_RHSprsub <- iv_robust(RepublicanShare1992 ~ Prob_Suburb_logit+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion | fira_adj+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion, data = estimation_sample_IV,se_type = "stata", diagnostics = TRUE)
IVfira_1992_wcont_LHSrep_RHSlog_popd <- iv_robust(RepublicanShare1992 ~ log_popd+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion | fira_adj+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion, data = estimation_sample_IV, se_type = "stata",diagnostics = TRUE)

IVfira_2000_wcont_LHSrep_RHSprsub <- iv_robust(RepublicanShare2000 ~ Prob_Suburb_logit+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion | fira_adj+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion, data = estimation_sample_IV, se_type = "stata",diagnostics = TRUE)
IVfira_2000_wcont_LHSrep_RHSlog_popd <- iv_robust(RepublicanShare2000 ~ log_popd+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion | fira_adj+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion, data = estimation_sample_IV, se_type = "stata",diagnostics = TRUE)

IVfira_2012_wcont_LHSrep_RHSprsub <- iv_robust(RepublicanShare2012 ~ Prob_Suburb_logit+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion | fira_adj+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion, data = estimation_sample_IV, se_type = "stata",diagnostics = TRUE)
IVfira_2012_wcont_LHSrep_RHSlog_popd <- iv_robust(RepublicanShare2012 ~ log_popd+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion | fira_adj+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion, data = estimation_sample_IV, se_type = "stata",diagnostics = TRUE)

#Run first stage specs to get output
FSfira_1980_wcont_LHSrep_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion, data = estimation_sample_IV)
FSfira_1980_wcont_LHSrep_RHSlog_popd <- lm(log_popd ~ fira_adj+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion, data = estimation_sample_IV)

FSfira_1992_wcont_LHSrep_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion, data = estimation_sample_IV)
FSfira_1992_wcont_LHSrep_RHSlog_popd <- lm(log_popd ~ fira_adj+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion, data = estimation_sample_IV)

FSfira_2000_wcont_LHSrep_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion, data = estimation_sample_IV)
FSfira_2000_wcont_LHSrep_RHSlog_popd <- lm(log_popd ~ fira_adj+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion, data = estimation_sample_IV)

FSfira_2012_wcont_LHSrep_RHSprsub <- lm(Prob_Suburb_logit ~ fira_adj+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion, data = estimation_sample_IV)
FSfira_2012_wcont_LHSrep_RHSlog_popd <- lm(log_popd ~ fira_adj+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion, data = estimation_sample_IV)

#OLS for comparison

OLS_1980_wcontSubregionFE_LHSrep_RHSprsub <- lm(RepublicanShare1980 ~ Prob_Suburb_logit+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion, data = estimation_sample_IV)
OLS_1980_wcontSubregionFE_LHSrep_RHSlog_popd <- lm(RepublicanShare1980 ~ log_popd+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion, data = estimation_sample_IV)

OLS_1992_wcontSubregionFE_LHSrep_RHSprsub <- lm(RepublicanShare1992 ~ Prob_Suburb_logit+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion, data = estimation_sample_IV)
OLS_1992_wcontSubregionFE_LHSrep_RHSlog_popd <- lm(RepublicanShare1992 ~ log_popd+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion, data = estimation_sample_IV)

OLS_2000_wcontSubregionFE_LHSrep_RHSprsub <- lm(RepublicanShare2000 ~ Prob_Suburb_logit+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion, data = estimation_sample_IV)
OLS_2000_wcontSubregionFE_LHSrep_RHSlog_popd <- lm(RepublicanShare2000 ~ log_popd+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion, data = estimation_sample_IV)

OLS_2012_wcontSubregionFE_LHSrep_RHSprsub <- lm(RepublicanShare2012 ~ Prob_Suburb_logit+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion, data = estimation_sample_IV)
OLS_2012_wcontSubregionFE_LHSrep_RHSlog_popd <- lm(RepublicanShare2012 ~ log_popd+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion, data = estimation_sample_IV)

#Output

#Table- OLS with the IV sample, i.e. N = 554 instead of N = 681
stargazer(OLS_1980_wcontSubregionFE_LHSrep_RHSprsub, OLS_1992_wcontSubregionFE_LHSrep_RHSprsub,
          OLS_2000_wcontSubregionFE_LHSrep_RHSprsub,OLS_2012_wcontSubregionFE_LHSrep_RHSprsub, align=TRUE, header = FALSE,
          keep=c("Prob_Suburb_logit"), covariate.labels=c("Pr(Suburb)"),
          se = starprep(OLS_1980_wcontSubregionFE_LHSrep_RHSprsub,OLS_1992_wcontSubregionFE_LHSrep_RHSprsub,
                        OLS_2000_wcontSubregionFE_LHSrep_RHSprsub,OLS_2012_wcontSubregionFE_LHSrep_RHSprsub, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/6_election_OLS_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(OLS_1980_wcontSubregionFE_LHSrep_RHSlog_popd, OLS_1992_wcontSubregionFE_LHSrep_RHSlog_popd,
          OLS_2000_wcontSubregionFE_LHSrep_RHSlog_popd,OLS_2012_wcontSubregionFE_LHSrep_RHSlog_popd, align=TRUE, header = FALSE,
          keep=c("log_popd"), covariate.labels=c("$log$(Population Density)"),
          se = starprep(OLS_1980_wcontSubregionFE_LHSrep_RHSlog_popd,OLS_1992_wcontSubregionFE_LHSrep_RHSlog_popd,
                        OLS_2000_wcontSubregionFE_LHSrep_RHSlog_popd,OLS_2012_wcontSubregionFE_LHSrep_RHSlog_popd, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/6_election_OLS_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

#Table- IV first stage
stargazer(FSfira_1980_wcont_LHSrep_RHSprsub,FSfira_1992_wcont_LHSrep_RHSprsub,
          FSfira_2000_wcont_LHSrep_RHSprsub,FSfira_2012_wcont_LHSrep_RHSprsub,align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_1980_wcont_LHSrep_RHSprsub,FSfira_1992_wcont_LHSrep_RHSprsub,
                        FSfira_2000_wcont_LHSrep_RHSprsub,FSfira_2012_wcont_LHSrep_RHSprsub, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/6_election_IV_firstStage_PrSub_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

stargazer(FSfira_1980_wcont_LHSrep_RHSlog_popd,FSfira_1992_wcont_LHSrep_RHSlog_popd,
          FSfira_2000_wcont_LHSrep_RHSlog_popd,FSfira_2012_wcont_LHSrep_RHSlog_popd,align=TRUE, header = FALSE,
          keep=c("fira_adj"), covariate.labels=c("Highway mileage"),
          se = starprep(FSfira_1980_wcont_LHSrep_RHSlog_popd,FSfira_1992_wcont_LHSrep_RHSlog_popd,
                        FSfira_2000_wcont_LHSrep_RHSlog_popd,FSfira_2012_wcont_LHSrep_RHSlog_popd, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/6_election_IV_firstStage_popd_tab.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

#Table- IV second stage
#We will create this manually from looking at the summary of iv_robust
summary(IVfira_1980_wcont_LHSrep_RHSprsub)
summary(IVfira_1992_wcont_LHSrep_RHSprsub)
summary(IVfira_2000_wcont_LHSrep_RHSprsub)
summary(IVfira_2012_wcont_LHSrep_RHSprsub)

summary(IVfira_1980_wcont_LHSrep_RHSlog_popd)
summary(IVfira_1992_wcont_LHSrep_RHSlog_popd)
summary(IVfira_2000_wcont_LHSrep_RHSlog_popd)
summary(IVfira_2012_wcont_LHSrep_RHSlog_popd)
