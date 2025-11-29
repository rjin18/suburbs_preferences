library(logr)

#Richard Jin
#Date: 4/18/21

#8_individual_OLS.R
#Run individual-level regressions using 1992 ANES and 2012 CCES surveys
#Start with the data created in 7_XX.R
#LHS is social/economic preferences, RHS is county-level suburbanization
#measure, as before, with individual-level demographic controls

setwd("/Users/richardjin/Documents/ECON 210A/Project/Data/")

library(plyr)
library(dplyr)
library(foreign)
library(estimatr)

#Input
ANES_1992_estimation_sample <- read.csv("./Intermediate/ANES_1992_step7.csv")
CCES_2012_estimation_sample <- read.csv("./Intermediate/CCES_2012_step7.csv")

#first scale pop density by 1/1000
ANES_1992_estimation_sample$popdensity_1000 <- ANES_1992_estimation_sample$county_popdensity_sqmile/1000
CCES_2012_estimation_sample$popdensity_1000 <- CCES_2012_estimation_sample$county_popdensity_sqmile/1000

lm1 <- lm_robust(conservative_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female, data = CCES_2012_estimation_sample, se_type = "stata")
lm2 <- lm_robust(conservative_scale ~ popdensity_1000+age+age_sq+race_factor+collegegrad+female, data = CCES_2012_estimation_sample, se_type = "stata")
lm3 <- lm_robust(AA_oppose_binary ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female, data = CCES_2012_estimation_sample, se_type = "stata")
lm4 <- lm_robust(spending_cut ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female, data = CCES_2012_estimation_sample, se_type = "stata")
lm5 <- lm_robust(AA_oppose_binary ~ popdensity_1000+age+age_sq+race_factor+collegegrad+female, data = CCES_2012_estimation_sample, se_type = "stata")
lm6 <- lm_robust(spending_cut ~ popdensity_1000+age+age_sq+race_factor+collegegrad+female, data = CCES_2012_estimation_sample, se_type = "stata")

lm1a <- lm_robust(morality_nochange_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female, data = ANES_1992_estimation_sample, se_type = "stata")
lm2a <- lm_robust(morality_nochange_scale ~ popdensity_1000+age+age_sq+race_factor+collegegrad+female, data = ANES_1992_estimation_sample, se_type = "stata")
lm3a <- lm_robust(spendingcut_scale ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female, data = ANES_1992_estimation_sample, se_type = "stata")
lm4a <- lm_robust(spendingcut_scale ~ popdensity_1000+age+age_sq+race_factor+collegegrad+female, data = ANES_1992_estimation_sample, se_type = "stata")
lm1b <- lm_robust(morality_nochange_binary ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female, data = ANES_1992_estimation_sample, se_type = "stata")
lm2b <- lm_robust(morality_nochange_binary ~ popdensity_1000+age+age_sq+race_factor+collegegrad+female, data = ANES_1992_estimation_sample, se_type = "stata")
lm3b <- lm_robust(spendingcut_binary ~ Prob_Suburb_logit+age+age_sq+race_factor+collegegrad+female, data = ANES_1992_estimation_sample, se_type = "stata")
lm4b <- lm_robust(spendingcut_binary ~ popdensity_1000+age+age_sq+race_factor+collegegrad+female, data = ANES_1992_estimation_sample, se_type = "stata")






