#Richard Jin
#Date: 4/13/21

#3_estimation_setup.R
#Start with county-level dataset created in step 2 of the R scripts and merge on relevant
#years of Election data and Census Vars. Goal is to create final county-level dataset
#with 682 counties and then run specs in step 4

setwd("/yourpath")

library(plyr)
library(dplyr)
library(zoo)

#Input
#our previous county-level dataset (1970 counties)
county_output_2class <- read.csv("./Intermediate/counties_step2_logit.csv")
#Election files
Election_raw_1980 <- read.csv("./Leip_Atlas_1976_2016/1980_0_0_2.csv")
Election_raw_1980 <- Election_raw_1980[-1,] #remove extra headers
Election_raw_1992 <- read.csv("./Leip_Atlas_1976_2016/1992_0_0_2.csv")
Election_raw_1992 <- Election_raw_1992[-1,]
Election_raw_2000 <- read.csv("./Leip_Atlas_1976_2016/2000_0_0_2.csv")
Election_raw_2000 <- Election_raw_2000[-1,]
Election_raw_2012 <- read.csv("./Leip_Atlas_1976_2016/2012_0_0_2.csv")
Election_raw_2012 <- Election_raw_2012[-1,]
#1980,1990,2000,2010 Census
Census_raw_ts <- read.csv("./nhgis0006_csv/nhgis0006_ts_nominal_county.csv")
Census_raw_income1980 <- read.csv("./nhgis0007_csv/nhgis0007_ds107_1980_county.csv") #median income; non-inflation adjusted but this is fine b/c we are running only cross-sectional specs
income_1980 <- Census_raw_income1980[,c("GISJOIN","DIE001")]
Census_raw_income1990 <- read.csv("./nhgis0007_csv/nhgis0007_ds123_1990_county.csv")
income_1990 <- Census_raw_income1990[,c("GISJOIN","E4U001")]
Census_raw_income2000 <- read.csv("./nhgis0007_csv/nhgis0007_ds151_2000_county.csv")
income_2000 <- Census_raw_income2000[,c("GISJOIN","GMY001")]
Census_raw_income2010 <- read.csv("./nhgis0007_csv/nhgis0007_ds176_20105_2010_county.csv")
income_2010 <- Census_raw_income2010[,c("GISJOIN","JOIE001")]
#merge on income
incmerge1 <- full_join(income_1980,income_1990)
incmerge2 <- full_join(income_2000,income_2010)
incmerge <- full_join(incmerge1,incmerge2)
colnames(incmerge) <- c("GISJOIN","hhinc_1980","hhinc_1990","hhinc_2000","hhinc_2010")
Census_raw_ts <- left_join(Census_raw_ts,incmerge)
#fold in marital status
mar <- read.csv("./nhgis0008_csv/nhgis0008_ts_nominal_county.csv")
Census_raw_ts <- left_join(Census_raw_ts,mar, by = "GISJOIN")

#Process Election data

#combine 1980,1992,2000,2012 elections
#1980
Election_raw_1980$RepublicanShare1980 <- as.numeric(Election_raw_1980$Ronald.Reagan)/as.numeric(Election_raw_1980$Total.Vote)
Election_raw_1980$DemocratShare1980 <- as.numeric(Election_raw_1980$James.Carter)/as.numeric(Election_raw_1980$Total.Vote)
Election_raw_1980$ThirdShare1980 <- as.numeric(Election_raw_1980$John.Anderson)/as.numeric(Election_raw_1980$Total.Vote) #Anderson was later a Republican
Election_raw_1980$RepThirdCombShare1980 <- Election_raw_1980$RepublicanShare1980 + Election_raw_1980$ThirdShare1980
Election_1980_premerge <- Election_raw_1980[,c("FIPS","Total.Vote","RepublicanShare1980","DemocratShare1980","ThirdShare1980","RepThirdCombShare1980")]
colnames(Election_1980_premerge) <- c("FIPS","TotalVotes1980","RepublicanShare1980","DemocratShare1980","ThirdShare1980","RepThirdCombShare1980")
#1992
Election_raw_1992$RepublicanShare1992 <- as.numeric(Election_raw_1992$George.Bush)/as.numeric(Election_raw_1992$Total.Vote)
Election_raw_1992$DemocratShare1992 <- as.numeric(Election_raw_1992$William.Clinton)/as.numeric(Election_raw_1992$Total.Vote)
Election_raw_1992$ThirdShare1992 <- as.numeric(Election_raw_1992$H..Ross.Perot)/as.numeric(Election_raw_1992$Total.Vote) #Perot was later a Republican, but affiliation here unclear
Election_raw_1992$RepThirdCombShare1992 <- Election_raw_1992$RepublicanShare1992 + Election_raw_1992$ThirdShare1992
Election_1992_premerge <- Election_raw_1992[,c("FIPS","Total.Vote","RepublicanShare1992","DemocratShare1992","ThirdShare1992","RepThirdCombShare1992")]
colnames(Election_1992_premerge) <- c("FIPS","TotalVotes1992","RepublicanShare1992","DemocratShare1992","ThirdShare1992","RepThirdCombShare1992")
#2000
Election_raw_2000$RepublicanShare2000 <- as.numeric(Election_raw_2000$George.W..Bush)/as.numeric(Election_raw_2000$Total.Vote)
Election_raw_2000$DemocratShare2000 <- as.numeric(Election_raw_2000$Albert.Gore.Jr.)/as.numeric(Election_raw_2000$Total.Vote)
Election_raw_2000$ThirdShare2000 <- as.numeric(Election_raw_2000$Ralph.Nader)/as.numeric(Election_raw_2000$Total.Vote) #Nader was Green Party, so definitely not Republican
Election_raw_2000$DemThirdCombShare2000 <- Election_raw_2000$DemocratShare2000 + Election_raw_2000$ThirdShare2000
Election_2000_premerge <- Election_raw_2000[,c("FIPS","Total.Vote","RepublicanShare2000","DemocratShare2000","ThirdShare2000","DemThirdCombShare2000")]
colnames(Election_2000_premerge) <- c("FIPS","TotalVotes2000","RepublicanShare2000","DemocratShare2000","ThirdShare2000","DemThirdCombShare2000")
#2012
Election_raw_2012$RepublicanShare2012 <- as.numeric(Election_raw_2012$Willard.Mitt.Romney)/as.numeric(Election_raw_2012$Total.Vote)
Election_raw_2012$DemocratShare2012 <- as.numeric(Election_raw_2012$Barack.H..Obama)/as.numeric(Election_raw_2012$Total.Vote)
Election_raw_2012$ThirdShare2012 <- as.numeric(Election_raw_2012$Gary.Johnson)/as.numeric(Election_raw_2012$Total.Vote) #Johnson is libertarian
Election_raw_2012$RepThirdCombShare2012 <- Election_raw_2012$RepublicanShare2012 + Election_raw_2012$ThirdShare2012
Election_2012_premerge <- Election_raw_2012[,c("FIPS","Total.Vote","RepublicanShare2012","DemocratShare2012","ThirdShare2012","RepThirdCombShare2012")]
colnames(Election_2012_premerge) <- c("FIPS","TotalVotes2012","RepublicanShare2012","DemocratShare2012","ThirdShare2012","RepThirdCombShare2012")
#merge
Elections_merged1 <- inner_join(Election_1980_premerge,Election_1992_premerge,by = "FIPS")
Elections_merged2 <- inner_join(Election_2000_premerge,Election_2012_premerge,by = "FIPS")
Elections_merged <- inner_join(Elections_merged1,Elections_merged2,by = "FIPS")

Elections_merged$TotalVotes1980 <- as.numeric(Elections_merged$TotalVotes1980)
Elections_merged$TotalVotes1992 <- as.numeric(Elections_merged$TotalVotes1992)
Elections_merged$TotalVotes2000 <- as.numeric(Elections_merged$TotalVotes2000)
Elections_merged$TotalVotes2012 <- as.numeric(Elections_merged$TotalVotes2012)

#Fix FIPS codes based on David Dorn's document- here only one change we have to make, which is Miami Dade county from 12086 -> 12025
Elections_merged$FIPS <- ifelse(Elections_merged$FIPS == "12086","12025",Elections_merged$FIPS)

#Create a GISJOIN equivalent from FIPS codes for easy merging
temp <- paste(Elections_merged$FIPS,"0",sep = "")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
countycode <- substrRight(temp,4) #4-digit county; now we need the 3-digit state
state1 <- ifelse(nchar(temp) == 6,substr(temp,1,2),substr(temp,1,1))
statecode <- ifelse(nchar(state1) == 2,paste(state1,"0",sep=""),paste("0",state1,"0",sep=""))
Elections_merged$GISJOIN <- paste("G",statecode,countycode,sep="")
Elections_merged <- Elections_merged[,-c(1)]
rm(temp,countycode,state1,statecode)

#Merge onto our existing county-level dataset
#Note that we will lose one of our 682 counties and drop to 681
#The lost county is Nansemond in Virginia, which actually became extinct in 1974, so this is fine
counties_withelection <- inner_join(county_output_2class,Elections_merged,by="GISJOIN")

#Now, merge on the 1980 and 2000 census vars

#Process Census timeseries
#Vars we want: Share nonwhite, Share Bach+, income, age, Share married
Census_raw_1980_2010 <- Census_raw_ts %>% select(-contains(c("1790","1800","1810","1820","1830","1840","1850","1860","1870","1880","1890","1900","1910","1920","1930","1940","1950","1960","1970")))
Census_raw_1980_2010 <- rename(Census_raw_1980_2010, county_pop1980 = A00AA1980, county_pop1990 = A00AA1990, county_pop2000 = A00AA2000, county_pop2010 = A00AA2010)
#Share nonwhite
Census_raw_1980_2010$share_nonwhite_1980 <- (Census_raw_1980_2010$county_pop1980 - Census_raw_1980_2010$B18AA1980)/Census_raw_1980_2010$county_pop1980
Census_raw_1980_2010$share_nonwhite_1990 <- (Census_raw_1980_2010$county_pop1990 - Census_raw_1980_2010$B18AA1990)/Census_raw_1980_2010$county_pop1990
Census_raw_1980_2010$share_nonwhite_2000 <- (Census_raw_1980_2010$county_pop2000 - Census_raw_1980_2010$B18AA2000)/Census_raw_1980_2010$county_pop2000
Census_raw_1980_2010$share_nonwhite_2010 <- (Census_raw_1980_2010$county_pop2010 - Census_raw_1980_2010$B18AA2010)/Census_raw_1980_2010$county_pop2010
#Share (25+ pop) with Bach+
Census_raw_1980_2010 <- mutate(Census_raw_1980_2010, share_Bachplus_1980 = B69AC1980/(B69AA1980+B69AB1980+B69AC1980),share_Bachplus_1990 = B69AC1990/(B69AA1990+B69AB1990+B69AC1990),share_Bachplus_2000 = B69AC2000/(B69AA2000+B69AB2000+B69AC2000),share_Bachplus_2010 = B69AC125/(B69AA125+B69AB125+B69AC125))
#Average age (take midpoints of age brackets, except for top bracket, and calculate pop weighted average)
Census_raw_1980_2010 <- mutate(Census_raw_1980_2010, average_age_1980 = (2.5*B57AA1980+7*B57AB1980+12*B57AC1980+16*B57AD1980+18.5*B57AE1980+20*B57AF1980+21*B57AG1980+23*B57AH1980+27*B57AI1980+32*B57AJ1980+39.5*B57AK1980+49.5*B57AL1980+57*B57AM1980+60.5*B57AN1980+63*B57AO1980+69.5*B57AP1980+79.5*B57AQ1980+85*B57AR1980)/county_pop1980)
Census_raw_1980_2010 <- mutate(Census_raw_1980_2010, average_age_1990 = (2.5*B57AA1990+7*B57AB1990+12*B57AC1990+16*B57AD1990+18.5*B57AE1990+20*B57AF1990+21*B57AG1990+23*B57AH1990+27*B57AI1990+32*B57AJ1990+39.5*B57AK1990+49.5*B57AL1990+57*B57AM1990+60.5*B57AN1990+63*B57AO1990+69.5*B57AP1990+79.5*B57AQ1990+85*B57AR1990)/county_pop1990)
Census_raw_1980_2010 <- mutate(Census_raw_1980_2010, average_age_2000 = (2.5*B57AA2000+7*B57AB2000+12*B57AC2000+16*B57AD2000+18.5*B57AE2000+20*B57AF2000+21*B57AG2000+23*B57AH2000+27*B57AI2000+32*B57AJ2000+39.5*B57AK2000+49.5*B57AL2000+57*B57AM2000+60.5*B57AN2000+63*B57AO2000+69.5*B57AP2000+79.5*B57AQ2000+85*B57AR2000)/county_pop2000)
Census_raw_1980_2010 <- mutate(Census_raw_1980_2010, average_age_2010 = (2.5*B57AA2010+7*B57AB2010+12*B57AC2010+16*B57AD2010+18.5*B57AE2010+20*B57AF2010+21*B57AG2010+23*B57AH2010+27*B57AI2010+32*B57AJ2010+39.5*B57AK2010+49.5*B57AL2010+57*B57AM2010+60.5*B57AN2010+63*B57AO2010+69.5*B57AP2010+79.5*B57AQ2010+85*B57AR2010)/county_pop2010)
#Share married (conditional on being of married age)
Census_raw_1980_2010 <- mutate(Census_raw_1980_2010, share_Married_1980 = (BL1AB1980+BL1AC1980+BL1AD1980+BL1AE1980+BL1AF1980+BL1AH1980+BL1AI1980+BL1AJ1980+BL1AK1980+BL1AL1980)/(BL1AA1980+BL1AB1980+BL1AC1980+BL1AD1980+BL1AE1980+BL1AF1980+BL1AG1980+BL1AH1980+BL1AI1980+BL1AJ1980+BL1AK1980+BL1AL1980))
Census_raw_1980_2010 <- mutate(Census_raw_1980_2010, share_Married_1990 = (BL1AB1990+BL1AC1990+BL1AD1990+BL1AE1990+BL1AF1990+BL1AH1990+BL1AI1990+BL1AJ1990+BL1AK1990+BL1AL1990)/(BL1AA1990+BL1AB1990+BL1AC1990+BL1AD1990+BL1AE1990+BL1AF1990+BL1AG1990+BL1AH1990+BL1AI1990+BL1AJ1990+BL1AK1990+BL1AL1990))
Census_raw_1980_2010 <- mutate(Census_raw_1980_2010, share_Married_2000 = (BL1AB2000+BL1AC2000+BL1AD2000+BL1AE2000+BL1AF2000+BL1AH2000+BL1AI2000+BL1AJ2000+BL1AK2000+BL1AL2000)/(BL1AA2000+BL1AB2000+BL1AC2000+BL1AD2000+BL1AE2000+BL1AF2000+BL1AG2000+BL1AH2000+BL1AI2000+BL1AJ2000+BL1AK2000+BL1AL2000))
Census_raw_1980_2010 <- mutate(Census_raw_1980_2010, share_Married_2010 = (BL1AB125+BL1AC125+BL1AD125+BL1AE125+BL1AF125+BL1AH125+BL1AI125+BL1AJ125+BL1AK125+BL1AL125)/(BL1AA125+BL1AB125+BL1AC125+BL1AD125+BL1AE125+BL1AF125+BL1AG125+BL1AH125+BL1AI125+BL1AJ125+BL1AK125+BL1AL125))

#Keep the control vars and fix Dade county
Census_controls <- subset(Census_raw_1980_2010, select = c("GISJOIN","county_pop1980","county_pop1990","county_pop2000","county_pop2010","share_nonwhite_1980","share_nonwhite_1990","share_nonwhite_2000","share_nonwhite_2010","share_Bachplus_1980","share_Bachplus_1990","share_Bachplus_2000","share_Bachplus_2010","hhinc_1980","hhinc_1990","hhinc_2000","hhinc_2010","average_age_1980","average_age_1990","average_age_2000","average_age_2010","share_Married_1980","share_Married_1990","share_Married_2000","share_Married_2010"))
Dade_id <- c("G1200250","G1200860")
isDade <- Census_controls$GISJOIN %in% Dade_id
Dade_controls <- Census_controls[which(isDade),] #Dade/Miami-Dade county
Dade_controls$GISJOIN <- "G1200250"
Dade_controls <- na.locf(Dade_controls)
Census_controls2 <- Census_controls[-which(isDade),]
Census_controls3 <- rbind(Census_controls2,Dade_controls)

#Merge and save
counties_withelection_controls <- inner_join(counties_withelection,Census_controls3, by = "GISJOIN")
write.csv(counties_withelection_controls,"./Intermediate/counties_estimation_sample_step3.csv")

