#Richard Jin
#Date: 4/7/21

#1_cleanGISoutput.R
#Take countyXcity level dataset produced in GIS and process it to the county level

setwd("/yourpath")

library(plyr)
library(dplyr)
library(zoo)

#Input
countyXcity_data_orig <- read.dbf("./Intermediate/GIS_shape/countyXcity_sample.dbf") #countyXcity data from GIS
countyXcity_data_orig$MSACMSA <- as.character(countyXcity_data_orig$MSACMSA)
countyXcity_data_orig$inSamp <- as.numeric(as.character(countyXcity_data_orig$inSamp))
cities_XY <- read.csv("./Intermediate/places_1970_XY.csv") #XY coordinates of cities
cities_XY <- cities_XY[,c(1,2,4)]
colnames(cities_XY) <- c("city_X","city_Y","GISJOIN_2")
allcounties1970 <- read.dbf("./nhgis0006_shape/nhgis0006_shapefile_tl2000_us_county_1970/US_county_1970.dbf")
Census1_counties1970 <- read.csv("./nhgis0006_csv/nhgis0006_ds94_1970_county.csv") #load in Census data with 1970 vars
Census2_counties1970 <- read.csv("./nhgis0006_csv/nhgis0006_ds95_1970_county.csv") #has additional housing vars
Census3_counties1970 <- read.csv("./nhgis0006_csv/nhgis0006_ds98_1970_county.csv") #has education and employment
Census4_counties1970 <- read.csv("./nhgis0006_csv/nhgis0006_ds99_1970_county.csv") #has income

#1. Manually clean up

#First, find the county that didn't get matched to an MSA (erroneously); add it back
missingcounty <- allcounties1970[allcounties1970$NHGISNAM == "Plaquemines",]
missingcounty$MSACMSA <- "5560"
missingcounty$inSamp <- 1
countyXcity_data <- rbind.fill(countyXcity_data_orig,missingcounty)

#Next, manually check/fix the cities that belong to multiple counties; in most cases, these cities
#will land overwhelmingly in one county, in which the centroid belongs as well, so it is properly
#identified for the most part and we leave as is
#Fix NYC
NYC_countylist <- c("Queens","Bronx","New York","Richmond","Kings")
isNYC_county <- countyXcity_data$NHGISNAM %in% NYC_countylist & countyXcity_data$STATENAM == "New York"
NYC_counties <- countyXcity_data[which(isNYC_county),] #The 5 NYC boroughs
#Fill in the missing NYC place/city variables for the other 4 boroughs/counties; store separately
NYC_counties$STATE_2 <- na.locf(NYC_counties$STATE_2)
NYC_counties$NHGISST_2 <- na.locf(NYC_counties$NHGISST_2)
NYC_counties$PLACE <- na.locf(NYC_counties$PLACE)
NYC_counties$NAME <- na.locf(NYC_counties$NAME)
NYC_counties$NHGISPLACE <- na.locf(NYC_counties$NHGISPLACE)
NYC_counties$YEAR <- na.locf(NYC_counties$YEAR)
NYC_counties$GISJOIN_2 <- na.locf(NYC_counties$GISJOIN_2)
NYC_counties$field_1_2 <- na.locf(NYC_counties$field_1_2)
NYC_counties$CBC001 <- na.locf(NYC_counties$CBC001)
#Now remove the 5 NYC boroughs and add them back
isnotNYC_county <- !(countyXcity_data$NHGISNAM %in% NYC_countylist) | countyXcity_data$STATENAM != "New York"
counties_noNYC <- countyXcity_data[which(isnotNYC_county),]
countyXcity_data <- rbind(counties_noNYC,NYC_counties)

#Now within countyXcity_data, cut out un-needed vars
countyXcity_short <- countyXcity_data[,c(1:4,8,15:17,19,22,23,27,30,32)]

#merge on XY coordinates of cities
countyXcity_data2 <- left_join(countyXcity_short,cities_XY,by="GISJOIN_2")

#2. Processing RHS vars

#First, we want to calculate two important variables- distance to largest city in MSA and share of population
#in each county living in city with 50,000+

#identify largest city in each MSA and tack on to the countyXcity data
countyXcity_data2$citypop <- as.numeric(as.character(countyXcity_data2$CBC001))
countyXcity_data2 <- countyXcity_data2[,-14]
temp <- countyXcity_data2[!is.na(countyXcity_data2$citypop),]
MSA_largestcities <- temp %>%
  group_by(MSACMSA) %>%
  summarize(largestpop = max(citypop)) #at the MSA level
rm(temp)
temp2 <- left_join(countyXcity_data2,MSA_largestcities,by="MSACMSA")
temp2 <- temp2[order(temp2$MSACMSA,-temp2$citypop),] #sort by MSA and descending pop
temp2$largestname <- ifelse(temp2$citypop == temp2$largestpop, as.character(temp2$NAME),NA) #name of largest city
temp2$largestname <- na.locf(temp2$largestname) #fill in for the rest
temp2$largestX <- ifelse(temp2$citypop == temp2$largestpop, temp2$city_X,NA) #coordinates of largest city
temp2$largestX <- na.locf(temp2$largestX)
temp2$largestY <- ifelse(temp2$citypop == temp2$largestpop, temp2$city_Y,NA) 
temp2$largestY <- na.locf(temp2$largestY)
#calculate distance to largest city based on polygon centroid to largest city distance
temp2$distance_to_largestcity <- sqrt((temp2$X_CENTROID - temp2$largestX)^2+(temp2$Y_CENTROID - temp2$largestY)^2)
#create county-level indicator for contains largest city in MSA
temp2$dummy_largestcity <- ifelse(temp2$NAME == temp2$largestname,1,0)

#Now to calculate share of population in each county living in city with 50,000+, we need the 1970 Census vars
#So let's merge on the variables to our existing countyXcity dataset, temp2
#trim 1970 Census data
Census1_short <- subset(Census1_counties1970,select = c("GISJOIN","CBC001","CBH001","CBW001","CB4001","CB4002"))
colnames(Census1_short) <- c("GISJOIN","county_pop","county_totalhomevalue","county_whitepop","county_housingoccunits","county_rentoccunits")
Census3_short <- subset(Census3_counties1970,select = c("GISJOIN","C06001","C06002","C06003","C06004","C06005","C06006","C06007","C06008","C06009","C06010","C07001","C07002","C07003","C07004"))
colnames(Census3_short) <- c("GISJOIN","county_25p_nosch","county_25p_1to4","county_25p_5to6","county_25p_7","county_25p_8","county_25p_someHS","county_25p_HSgrad","county_25p_someCol","county_25p_Bach","county_25p_Mastersplus","county_16p_empl_military","county_16p_empl_employed","county_16p_empl_unemployed","county_16p_empl_notinforce")
Census4_short <- subset(Census4_counties1970,select = c("GISJOIN","C1K001"))
colnames(Census4_short) <- c("GISJOIN","county_totalincome")
#collect Census data into single source
merge1 <- inner_join(Census1_short,Census3_short,by = "GISJOIN")
merge2 <- inner_join(merge1,Census4_short,by = "GISJOIN") #at county level, with vars for 1970
#Collapse cityXcounty data to county level
temp2$citypop <- ifelse(is.na(temp2$citypop), 0,temp2$citypop) #next few lines give us total city pop
citypop_data <- temp2 %>%
  group_by(GISJOIN) %>%
  summarize(citypop_total = sum(citypop),contains_largestcity = max(dummy_largestcity)) #at the county level
temp3 <- subset(temp2,select = c("DECADE","GISJOIN","NHGISST","NHGISCTY","NHGISNAM","STATENAM","SHAPE_AREA","MSACMSA","inSamp","largestname","distance_to_largestcity"))
master_county <- unique(temp3)
#merge on city populations
county_join1 <- left_join(master_county,citypop_data,by="GISJOIN")
#merge on Census vars
county_join2 <- left_join(county_join1,merge2,by="GISJOIN")

#3. Final cleanup and save county-level dataset
#calculate population density
county_join2$area_sqmi <- 0.0000003861*county_join2$SHAPE_AREA #convert area from sq meters to sq miles
county_join2$county_popdensity_sqmile <- county_join2$county_pop/county_join2$area_sqmi
#calculate share of county pop that is in cities of 50,000+
county_join2$citypop_total <- ifelse(county_join2$citypop_total > county_join2$county_pop, county_join2$county_pop, county_join2$citypop_total)
county_join2$county_citypop_share <- county_join2$citypop_total/county_join2$county_pop
#dummy for suburb- 1 if citypopshare < 0.5, 0 if citypopshare >= 0.5
county_join2$county_suburbdummy <- ifelse(county_join2$county_citypop_share < 0.5,1,0)
#convert distance from meters to miles
county_join2$distance_to_largestcity_mi <- 0.000621371*county_join2$distance_to_largestcity
#average home value conditional on home ownership
county_join2$average_homevalue_owned <- county_join2$county_totalhomevalue/county_join2$county_housingoccunits
#share of housing units that are owned and not rented
county_join2$homeowner_share <- county_join2$county_housingoccunits/(county_join2$county_housingoccunits+county_join2$county_rentoccunits)
#share of pop that is nonwhite
county_join2$share_nonwhite <- (county_join2$county_pop - county_join2$county_whitepop)/county_join2$county_pop
#share of 25+ pop that graduated HS
county_join2$share_HS <- (county_join2$county_25p_HSgrad+county_join2$county_25p_someCol+county_join2$county_25p_Bach+county_join2$county_25p_Mastersplus)/(county_join2$county_25p_nosch+county_join2$county_25p_1to4+county_join2$county_25p_5to6+county_join2$county_25p_7+county_join2$county_25p_8+county_join2$county_25p_someHS+county_join2$county_25p_HSgrad+county_join2$county_25p_someCol+county_join2$county_25p_Bach+county_join2$county_25p_Mastersplus)
#share of 25+ pop that graduated Col
county_join2$share_Bachplus <- (county_join2$county_25p_Bach+county_join2$county_25p_Mastersplus)/(county_join2$county_25p_nosch+county_join2$county_25p_1to4+county_join2$county_25p_5to6+county_join2$county_25p_7+county_join2$county_25p_8+county_join2$county_25p_someHS+county_join2$county_25p_HSgrad+county_join2$county_25p_someCol+county_join2$county_25p_Bach+county_join2$county_25p_Mastersplus)
#share of labor force unemployed (include military)
county_join2$unemployment_rate <- county_join2$county_16p_empl_unemployed/(county_join2$county_16p_empl_military+county_join2$county_16p_empl_employed+county_join2$county_16p_empl_unemployed)
#income per capita
county_join2$income_percapita <- county_join2$county_totalincome/county_join2$county_pop
#clean up 'contains largest city' dummy
county_join2$contains_largestcity <- ifelse(is.na(county_join2$contains_largestcity),0,county_join2$contains_largestcity)

#Keep relevant vars and save output
county_output_data <- subset(county_join2, select = c("DECADE","GISJOIN","NHGISST","NHGISCTY","NHGISNAM","STATENAM","area_sqmi","MSACMSA","inSamp","largestname","contains_largestcity","county_pop","county_popdensity_sqmile","county_citypop_share","county_suburbdummy","distance_to_largestcity_mi","average_homevalue_owned","homeowner_share","share_nonwhite","share_HS","share_Bachplus","unemployment_rate","income_percapita"))
write.csv(county_output_data,"./Intermediate/county_1970_characteristics_step1.csv")

