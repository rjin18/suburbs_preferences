library(logr)

#Richard Jin
#Date: 4/14/21

#5_estimation_election_OLS.R
#Start with county-level dataset created in step 3 of the R scripts, which contains
#all of our RHS and LHS vars. Here we run our main specs with Republican Vote Share
#as the main dependent variable. Recall our key RHS variable is based on 1970 data

#Note we will report robust SEs but to do so we need to use the starprep command

setwd("/Users/richardjin/Documents/ECON 210A/Project")

library(plyr)
library(dplyr)
library(foreign)
library(estimatr)

#Input
election_estimation_sample <- read.csv("./Data/Intermediate/counties_estimation_sample_step3.csv")

#first take log population density
election_estimation_sample$log_popd <- log(election_estimation_sample$county_popdensity_sqmile)

#OLS, no controls for 4 election years; run 2 specs, with RHS as Pr(Suburb) and as log(population density)
#Note that Pr(Suburb) and pop density go in opposite directions; any way to make them consistent? Inverse transformation of pop density is sketchy
#1980
OLS_1980_nocont_LHSrep_RHSprsub <- lm(RepublicanShare1980 ~ Prob_Suburb_logit, data = election_estimation_sample)
OLS_1980_nocont_LHSrep_RHSlog_popd <- lm(RepublicanShare1980 ~ log_popd, data = election_estimation_sample)
#1992
OLS_1992_nocont_LHSrep_RHSprsub <- lm(RepublicanShare1992 ~ Prob_Suburb_logit, data = election_estimation_sample)
OLS_1992_nocont_LHSrep_RHSlog_popd <- lm(RepublicanShare1992 ~ log_popd, data = election_estimation_sample)
#2000
OLS_2000_nocont_LHSrep_RHSprsub <- lm(RepublicanShare2000 ~ Prob_Suburb_logit, data = election_estimation_sample)
OLS_2000_nocont_LHSrep_RHSlog_popd <- lm(RepublicanShare2000 ~ log_popd, data = election_estimation_sample)
#2012
OLS_2012_nocont_LHSrep_RHSprsub <- lm(RepublicanShare2012 ~ Prob_Suburb_logit, data = election_estimation_sample)
OLS_2012_nocont_LHSrep_RHSlog_popd <- lm(RepublicanShare2012 ~ log_popd, data = election_estimation_sample)

#OLS, with demographic controls for 4 election years; run 2 specs, with RHS as Pr(Suburb) and as log(population density)
#1980
OLS_1980_wcont_LHSrep_RHSprsub <- lm(RepublicanShare1980 ~ Prob_Suburb_logit+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980, data = election_estimation_sample)
OLS_1980_wcont_LHSrep_RHSlog_popd <- lm(RepublicanShare1980 ~ log_popd+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980, data = election_estimation_sample)
#1992
OLS_1992_wcont_LHSrep_RHSprsub <- lm(RepublicanShare1992 ~ Prob_Suburb_logit+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990, data = election_estimation_sample)
OLS_1992_wcont_LHSrep_RHSlog_popd <- lm(RepublicanShare1992 ~ log_popd+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990, data = election_estimation_sample)
#2000
OLS_2000_wcont_LHSrep_RHSprsub <- lm(RepublicanShare2000 ~ Prob_Suburb_logit+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000, data = election_estimation_sample)
OLS_2000_wcont_LHSrep_RHSlog_popd <- lm(RepublicanShare2000 ~ log_popd+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000, data = election_estimation_sample)
#2012
OLS_2012_wcont_LHSrep_RHSprsub <- lm(RepublicanShare2012 ~ Prob_Suburb_logit+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010, data = election_estimation_sample)
OLS_2012_wcont_LHSrep_RHSlog_popd <- lm(RepublicanShare2012 ~ log_popd+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010, data = election_estimation_sample)

#OLS, with added Subregion fixed effects
#1980
OLS_1980_wcontSubregionFE_LHSrep_RHSprsub <- lm(RepublicanShare1980 ~ Prob_Suburb_logit+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion, data = election_estimation_sample)
OLS_1980_wcontSubregionFE_LHSrep_RHSlog_popd <- lm(RepublicanShare1980 ~ log_popd+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+Subregion, data = election_estimation_sample)
#1992
OLS_1992_wcontSubregionFE_LHSrep_RHSprsub <- lm(RepublicanShare1992 ~ Prob_Suburb_logit+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion, data = election_estimation_sample)
OLS_1992_wcontSubregionFE_LHSrep_RHSlog_popd <- lm(RepublicanShare1992 ~ log_popd+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+Subregion, data = election_estimation_sample)
#2000
OLS_2000_wcontSubregionFE_LHSrep_RHSprsub <- lm(RepublicanShare2000 ~ Prob_Suburb_logit+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion, data = election_estimation_sample)
OLS_2000_wcontSubregionFE_LHSrep_RHSlog_popd <- lm(RepublicanShare2000 ~ log_popd+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+Subregion, data = election_estimation_sample)
#2012
OLS_2012_wcontSubregionFE_LHSrep_RHSprsub <- lm(RepublicanShare2012 ~ Prob_Suburb_logit+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion, data = election_estimation_sample)
OLS_2012_wcontSubregionFE_LHSrep_RHSlog_popd <- lm(RepublicanShare2012 ~ log_popd+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+Subregion, data = election_estimation_sample)

#OLS, with added MSA fixed effects
#1980
OLS_1980_wcontMSAFE_LHSrep_RHSprsub <- lm(RepublicanShare1980 ~ Prob_Suburb_logit+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+as.factor(MSACMSA), data = election_estimation_sample)
OLS_1980_wcontMSAFE_LHSrep_RHSlog_popd <- lm(RepublicanShare1980 ~ log_popd+share_nonwhite_1980+share_Bachplus_1980+hhinc_1980+average_age_1980+share_Married_1980+as.factor(MSACMSA), data = election_estimation_sample)
#1992
OLS_1992_wcontMSAFE_LHSrep_RHSprsub <- lm(RepublicanShare1992 ~ Prob_Suburb_logit+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+as.factor(MSACMSA), data = election_estimation_sample)
OLS_1992_wcontMSAFE_LHSrep_RHSlog_popd <- lm(RepublicanShare1992 ~ log_popd+share_nonwhite_1990+share_Bachplus_1990+hhinc_1990+average_age_1990+share_Married_1990+as.factor(MSACMSA), data = election_estimation_sample)
#2000
OLS_2000_wcontMSAFE_LHSrep_RHSprsub <- lm(RepublicanShare2000 ~ Prob_Suburb_logit+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+as.factor(MSACMSA), data = election_estimation_sample)
OLS_2000_wcontMSAFE_LHSrep_RHSlog_popd <- lm(RepublicanShare2000 ~ log_popd+share_nonwhite_2000+share_Bachplus_2000+hhinc_2000+average_age_2000+share_Married_2000+as.factor(MSACMSA), data = election_estimation_sample)
#2012
OLS_2012_wcontMSAFE_LHSrep_RHSprsub <- lm(RepublicanShare2012 ~ Prob_Suburb_logit+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+as.factor(MSACMSA), data = election_estimation_sample)
OLS_2012_wcontMSAFE_LHSrep_RHSlog_popd <- lm(RepublicanShare2012 ~ log_popd+share_nonwhite_2010+share_Bachplus_2010+hhinc_2010+average_age_2010+share_Married_2010+as.factor(MSACMSA), data = election_estimation_sample)

#Output

#First table- 8 columns, grouped in 2's by the 4 election years; each group of 2 will be 1st col no controls, 2nd col with controls and subregion FE
#RHS is Pr(Sub)
stargazer(OLS_1980_nocont_LHSrep_RHSprsub,OLS_1980_wcontSubregionFE_LHSrep_RHSprsub,
          OLS_1992_nocont_LHSrep_RHSprsub,OLS_1992_wcontSubregionFE_LHSrep_RHSprsub,
          OLS_2000_nocont_LHSrep_RHSprsub,OLS_2000_wcontSubregionFE_LHSrep_RHSprsub,
          OLS_2012_nocont_LHSrep_RHSprsub,OLS_2012_wcontSubregionFE_LHSrep_RHSprsub, align=TRUE, header = FALSE,
          keep=c("Prob_Suburb_logit"), covariate.labels=c("Pr(Suburb)"),
          se = starprep(OLS_1980_nocont_LHSrep_RHSprsub,OLS_1980_wcontSubregionFE_LHSrep_RHSprsub,OLS_1992_nocont_LHSrep_RHSprsub,OLS_1992_wcontSubregionFE_LHSrep_RHSprsub,
                        OLS_2000_nocont_LHSrep_RHSprsub,OLS_2000_wcontSubregionFE_LHSrep_RHSprsub,OLS_2012_nocont_LHSrep_RHSprsub,OLS_2012_wcontSubregionFE_LHSrep_RHSprsub, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/5_election_OLS_PrSub_tab1.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)

#Second table- 8 columns, grouped in 2's by the 4 election years; each group of 2 will be 1st col no controls, 2nd col with controls and subregion FE
#RHS is Pr(Sub)
stargazer(OLS_1980_nocont_LHSrep_RHSlog_popd,OLS_1980_wcontSubregionFE_LHSrep_RHSlog_popd,
          OLS_1992_nocont_LHSrep_RHSlog_popd,OLS_1992_wcontSubregionFE_LHSrep_RHSlog_popd,
          OLS_2000_nocont_LHSrep_RHSlog_popd,OLS_2000_wcontSubregionFE_LHSrep_RHSlog_popd,
          OLS_2012_nocont_LHSrep_RHSlog_popd,OLS_2012_wcontSubregionFE_LHSrep_RHSlog_popd, align=TRUE, header = FALSE,
          keep=c("log_popd"), covariate.labels=c("$log$(Population Density)"),
          se = starprep(OLS_1980_nocont_LHSrep_RHSlog_popd,OLS_1980_wcontSubregionFE_LHSrep_RHSlog_popd,OLS_1992_nocont_LHSrep_RHSlog_popd,OLS_1992_wcontSubregionFE_LHSrep_RHSlog_popd,
                        OLS_2000_nocont_LHSrep_RHSlog_popd,OLS_2000_wcontSubregionFE_LHSrep_RHSlog_popd,OLS_2012_nocont_LHSrep_RHSlog_popd,OLS_2012_wcontSubregionFE_LHSrep_RHSlog_popd, se_type = "stata"),
          keep.stat=c("n","rsq"), no.space=TRUE, out="./Output/Tables/5_election_OLS_popd_tab1.tex", out.header=FALSE,
          colnames = FALSE, model.numbers = FALSE)


