#Richard Jin
#Date: 5/8/21

#4_descriptive_tabfig.R
#Use this R-script to create descriptive tables/figures for the paper

setwd("/Users/richardjin/Documents/ECON 210A/Project/Data/")

library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)
require(gridExtra)
library(reshape)
library(stringr)

########################################################
#Figure 2- plot time series of Republican Vote Share
########################################################

#Input
#our previous county-level dataset (1970 counties)
county_output_2class <- read.csv("./Intermediate/counties_step2_logit.csv")
county_simplified <- county_output_2class[,c("GISJOIN","Prob_Suburb_logit")]
#Election files
Election_raw_1972 <- read.csv("./Leip_Atlas_1976_2016/1972_0_0_2.csv")
Election_raw_1972 <- Election_raw_1972[-1,] #remove extra headers
Election_raw_1976 <- read.csv("./Leip_Atlas_1976_2016/1976_0_0_2.csv")
Election_raw_1976 <- Election_raw_1976[-1,] 
Election_raw_1980 <- read.csv("./Leip_Atlas_1976_2016/1980_0_0_2.csv")
Election_raw_1980 <- Election_raw_1980[-1,] 
Election_raw_1984 <- read.csv("./Leip_Atlas_1976_2016/1984_0_0_2.csv")
Election_raw_1984 <- Election_raw_1984[-1,] 
Election_raw_1988 <- read.csv("./Leip_Atlas_1976_2016/1988_0_0_2.csv")
Election_raw_1988 <- Election_raw_1988[-1,] 
Election_raw_1992 <- read.csv("./Leip_Atlas_1976_2016/1992_0_0_2.csv")
Election_raw_1992 <- Election_raw_1992[-1,]
Election_raw_1996 <- read.csv("./Leip_Atlas_1976_2016/1996_0_0_2.csv")
Election_raw_1996 <- Election_raw_1996[-1,]
Election_raw_2000 <- read.csv("./Leip_Atlas_1976_2016/2000_0_0_2.csv")
Election_raw_2000 <- Election_raw_2000[-1,]
Election_raw_2004 <- read.csv("./Leip_Atlas_1976_2016/2004_0_0_2.csv")
Election_raw_2004 <- Election_raw_2004[-1,]
Election_raw_2008 <- read.csv("./Leip_Atlas_1976_2016/2008_0_0_2.csv")
Election_raw_2008 <- Election_raw_2008[-1,]
Election_raw_2012 <- read.csv("./Leip_Atlas_1976_2016/2012_0_0_2.csv")
Election_raw_2012 <- Election_raw_2012[-1,]
Election_raw_2016 <- read.csv("./Leip_Atlas_1976_2016/2016_0_0_2.csv")
Election_raw_2016 <- Election_raw_2016[-1,]

#Process Election data

#combine all elections
#1972
Election_raw_1972$TotalVotes1972 <- as.numeric(Election_raw_1972$Total.Vote)
Election_raw_1972$RepublicanVotes1972 <- as.numeric(Election_raw_1972$Richard.Nixon)
Election_1972_premerge <- Election_raw_1972[,c("FIPS","TotalVotes1972","RepublicanVotes1972")]
#1976
Election_raw_1976$TotalVotes1976 <- as.numeric(Election_raw_1976$Total.Vote)
Election_raw_1976$RepublicanVotes1976 <- as.numeric(Election_raw_1976$Gerald.Ford)
Election_1976_premerge <- Election_raw_1976[,c("FIPS","TotalVotes1976","RepublicanVotes1976")]
#1980
Election_raw_1980$TotalVotes1980 <- as.numeric(Election_raw_1980$Total.Vote)
Election_raw_1980$RepublicanVotes1980 <- as.numeric(Election_raw_1980$Ronald.Reagan)
Election_1980_premerge <- Election_raw_1980[,c("FIPS","TotalVotes1980","RepublicanVotes1980")]
#1984
Election_raw_1984$TotalVotes1984 <- as.numeric(Election_raw_1984$Total.Vote)
Election_raw_1984$RepublicanVotes1984 <- as.numeric(Election_raw_1984$Ronald.Reagan)
Election_1984_premerge <- Election_raw_1984[,c("FIPS","TotalVotes1984","RepublicanVotes1984")]
#1988
Election_raw_1988$TotalVotes1988 <- as.numeric(Election_raw_1988$Total.Vote)
Election_raw_1988$RepublicanVotes1988 <- as.numeric(Election_raw_1988$George.Bush)
Election_1988_premerge <- Election_raw_1988[,c("FIPS","TotalVotes1988","RepublicanVotes1988")]
#1992
Election_raw_1992$TotalVotes1992 <- as.numeric(Election_raw_1992$Total.Vote)
Election_raw_1992$RepublicanVotes1992 <- as.numeric(Election_raw_1992$George.Bush)
Election_1992_premerge <- Election_raw_1992[,c("FIPS","TotalVotes1992","RepublicanVotes1992")]
#1996
Election_raw_1996$TotalVotes1996 <- as.numeric(Election_raw_1996$Total.Vote)
Election_raw_1996$RepublicanVotes1996 <- as.numeric(Election_raw_1996$Robert.Dole)
Election_1996_premerge <- Election_raw_1996[,c("FIPS","TotalVotes1996","RepublicanVotes1996")]
#2000
Election_raw_2000$TotalVotes2000 <- as.numeric(Election_raw_2000$Total.Vote)
Election_raw_2000$RepublicanVotes2000 <- as.numeric(Election_raw_2000$George.W..Bush)
Election_2000_premerge <- Election_raw_2000[,c("FIPS","TotalVotes2000","RepublicanVotes2000")]
#2004
Election_raw_2004$TotalVotes2004 <- as.numeric(Election_raw_2004$Total.Vote)
Election_raw_2004$RepublicanVotes2004 <- as.numeric(Election_raw_2004$George.W..Bush)
Election_2004_premerge <- Election_raw_2004[,c("FIPS","TotalVotes2004","RepublicanVotes2004")]
#2008
Election_raw_2008$TotalVotes2008 <- as.numeric(Election_raw_2008$Total.Vote)
Election_raw_2008$RepublicanVotes2008 <- as.numeric(Election_raw_2008$John.S..McCain.III)
Election_2008_premerge <- Election_raw_2008[,c("FIPS","TotalVotes2008","RepublicanVotes2008")]
#2012
Election_raw_2012$TotalVotes2012 <- as.numeric(Election_raw_2012$Total.Vote)
Election_raw_2012$RepublicanVotes2012 <- as.numeric(Election_raw_2012$Willard.Mitt.Romney)
Election_2012_premerge <- Election_raw_2012[,c("FIPS","TotalVotes2012","RepublicanVotes2012")]
#2016
Election_raw_2016$TotalVotes2016 <- as.numeric(Election_raw_2016$Total.Vote)
Election_raw_2016$RepublicanVotes2016 <- as.numeric(Election_raw_2016$Donald.J..Trump)
Election_2016_premerge <- Election_raw_2016[,c("FIPS","TotalVotes2016","RepublicanVotes2016")]

#merge
Elections_merged1 <- inner_join(Election_1972_premerge,Election_1976_premerge,by = "FIPS")
Elections_merged2 <- inner_join(Election_1980_premerge,Election_1984_premerge,by = "FIPS")
Elections_1972_1984 <- inner_join(Elections_merged1,Elections_merged2,by = "FIPS")
Elections_merged3 <- inner_join(Election_1988_premerge,Election_1992_premerge,by = "FIPS")
Elections_merged4 <- inner_join(Election_1996_premerge,Election_2000_premerge,by = "FIPS")
Elections_1988_2000 <- inner_join(Elections_merged3,Elections_merged4,by = "FIPS")
Elections_merged5 <- inner_join(Election_2004_premerge,Election_2008_premerge,by = "FIPS")
Elections_merged6 <- inner_join(Election_2012_premerge,Election_2016_premerge,by = "FIPS")
Elections_2004_2016 <- inner_join(Elections_merged5,Elections_merged6,by = "FIPS")
temp1 <- inner_join(Elections_1972_1984,Elections_1988_2000,by = "FIPS")
Elections_all <- inner_join(temp1,Elections_2004_2016,by = "FIPS")
rm(temp1)

#Fix FIPS codes based on David Dorn's document- here only one change we have to make, which is Miami Dade county from 12086 -> 12025
Elections_all$FIPS <- ifelse(Elections_all$FIPS == "12086","12025",Elections_all$FIPS)

#Create a GISJOIN equivalent from FIPS codes for easy merging
temp <- paste(Elections_all$FIPS,"0",sep = "")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
countycode <- substrRight(temp,4) #4-digit county; now we need the 3-digit state
state1 <- ifelse(nchar(temp) == 6,substr(temp,1,2),substr(temp,1,1))
statecode <- ifelse(nchar(state1) == 2,paste(state1,"0",sep=""),paste("0",state1,"0",sep=""))
Elections_all$GISJOIN <- paste("G",statecode,countycode,sep="")
Elections_all <- Elections_all[,-c(1)]
rm(temp,countycode,state1,statecode)

#Merge onto our existing county-level dataset
#Note that we will lose two of our 682 counties and drop to 680
#One lost county is Nansemond in Virginia, which actually became extinct in 1974, so this is fine; other is undetermined
counties_allelection <- inner_join(county_simplified,Elections_all,by="GISJOIN")

#Create dummy out of Pr(Sub), 1 if >= 0.5. Then Reshape data to long
counties_allelection$PrSub_dummy <- ifelse(counties_allelection$Prob_Suburb_logit >= 0.5,1,0)
counties_allelection <- counties_allelection[,-c(2)] #keep dummy, drop Pr(Sub)
#reshape
counties_reshaped <- reshape(counties_allelection, direction = 'long',
                             varying=c('TotalVotes1972','RepublicanVotes1972','TotalVotes1976','RepublicanVotes1976',
                                       'TotalVotes1980','RepublicanVotes1980','TotalVotes1984','RepublicanVotes1984',
                                       'TotalVotes1988','RepublicanVotes1988','TotalVotes1992','RepublicanVotes1992',
                                       'TotalVotes1996','RepublicanVotes1996','TotalVotes2000','RepublicanVotes2000',
                                       'TotalVotes2004','RepublicanVotes2004','TotalVotes2008','RepublicanVotes2008',
                                       'TotalVotes2012','RepublicanVotes2012','TotalVotes2016','RepublicanVotes2016'),
                             timevar='year',
                             times=c('1972','1976','1980','1984','1988','1992','1996','2000','2004','2008','2012','2016'),
                             v.names=c('RepublicanVotes','TotalVotes'),
                             idvar=c('GISJOIN','PrSub_dummy'))
#total votes and republican votes got flipped, e.g. more republican votes than total votes; manually flip correctly
counties_reshaped$total_correct <- counties_reshaped$RepublicanVotes
counties_reshaped$rep_correct <- counties_reshaped$TotalVotes
counties_reshaped_correct <- counties_reshaped[,-c(4,5)]

#collapse to the subdummyxyear level (vote share) and year level (difference in vote share)
subyear_election <- aggregate(cbind(counties_reshaped_correct$rep_correct,counties_reshaped_correct$total_correct),
                              by = list(counties_reshaped_correct$PrSub_dummy,counties_reshaped_correct$year), FUN = sum)
subyear_election$RepublicanVoteShare <- subyear_election$V1/subyear_election$V2
subyear_election <- subyear_election[,-c(3,4)]
colnames(subyear_election) <- c("subdummy","year","RepublicanVoteShare")
year_election <- reshape(subyear_election,idvar = "year",timevar = "subdummy",direction="wide")
year_election$difference <- year_election$RepublicanVoteShare.1 - year_election$RepublicanVoteShare.0

#Plot the sub-notsub time series of Rep vote share and the time series of the difference side by side
#convert Pr(Sub) >= 0.5 dummy to character for easy legend label
subyear_election$Type1970 <- ifelse(subyear_election$subdummy == 1,"Suburban","Urban")
subyear_plot <- ggplot(subyear_election, aes(x=year, y=RepublicanVoteShare, group=Type1970)) +
  theme_minimal()+
  ggtitle("Republican Vote Share Over Time, Urban and Suburban")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name = "Republican Vote Share", limits = c(0,0.8))+
  geom_line(aes(linetype=Type1970))+
  geom_point()+
  theme(legend.position=c(0.2,0.2))
year_plot <- ggplot(year_election, aes(x=year, y=difference,group=1)) +
  theme_minimal()+
  ggtitle("Difference Over Time, Suburban minus Urban")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name = "Difference", limits = c(0,0.15))+
  geom_line()+
  geom_point()

#Final plot
grid.arrange(subyear_plot, year_plot, ncol=2)

########################################################
#Figure 3- bar charts for CCES responses
########################################################

#Load in CCES data
load("./CCES/commoncontent2012.RData")
CCES_2012 <- x
rm(x)
#keep relevant columns- cc334a = self reported political ideology, cc327 = support for affirmative action, cc328 = deficit financing (spending cuts vs tax increases)
CCES_2012_short <- CCES_2012[,c("V101","countyfips","CC334A","CC327","CC328")]
colnames(CCES_2012_short) <- c("id","fips","politics_selfreport","AA_support","deficit_financing")
#Three main outcomes
#self-reported conservative
CCES_2012_short$conservative_binary <- ifelse(CCES_2012_short$politics_selfreport == "Very Conservative" | CCES_2012_short$politics_selfreport == "Conservative" | CCES_2012_short$politics_selfreport == "Somewhat Conservative",1,0)
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

#Fix Dade county in CCES
CCES_2012_short$GISJOIN <- ifelse(CCES_2012_short$GISJOIN == "G1200860","G1200250",CCES_2012_short$GISJOIN)

#Merge CCES with county-level vars
CCES_output <- inner_join(CCES_2012_short,county_simplified,by = "GISJOIN")
CCES_output_nomissing <- CCES_output[!is.na(CCES_output$conservative_binary) & !is.na(CCES_output$AA_oppose_binary) & !is.na(CCES_output$spending_cut),]

#As we did for Figure 2, create dummy from Pr(Suburb) and collapse to dummy level
CCES_output_nomissing$PrSub_dummy <- ifelse(CCES_output_nomissing$Prob_Suburb_logit >= 0.5,1,0)
CCES_precollapse <- CCES_output_nomissing[,c(6:8,11)]
CCES_ideology <- aggregate(list(Ideology = CCES_precollapse$conservative_binary),by = list(CCES_precollapse$PrSub_dummy), mean)
CCES_social <- aggregate(list(Social = CCES_precollapse$AA_oppose_binary),by = list(CCES_precollapse$PrSub_dummy), mean)
CCES_econ <- aggregate(list(Economic = CCES_precollapse$spending_cut),by = list(CCES_precollapse$PrSub_dummy), mean)
merge1 <- inner_join(CCES_ideology,CCES_social,by="Group.1")
CCES_collapsed <- inner_join(merge1,CCES_econ,by="Group.1")
CCES_collapsed_reshaped <- melt(CCES_collapsed, id="Group.1")
CCES_collapsed_reshaped$Type1970 <- ifelse(CCES_collapsed_reshaped$Group.1 == 1,"Suburban","Urban")

#Make bar plot displaying proportions of respondents for each of the three outcomes across the two area types
ggplot(CCES_collapsed_reshaped,aes(x=variable,y=value,fill=Type1970))+
  theme_minimal()+
  ggtitle("Dimensions of Conservatism, Urban and Suburban")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c("navyblue", "grey60"))+
  xlab("Response")+ylab("Share of respondents")

