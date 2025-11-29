#Richard Jin
#Date: 4/13/21

#2_county_UDgradient.R
#Start with county-level dataset created in 1_cleanGISoutput.R and create an index of suburbanization based on 
#Clark (1951), known as the urban density gradient. Provides a simpler, more canonical measurement of suburbanization. 
#Save separately and then merge in 3_estimation_setup.R

setwd("/yourpath")

library(plyr)
library(dplyr)

#Input
county_output_data <- read.csv("./Intermediate/county_1970_characteristics_step1.csv")
#The following step is crucial- for all counties containing the MSA's largest city, we set the distance to largest city as 0. Previously, the distance
#would be nonzero b/c the county centroid was not the same as the point location of the largest city
county_output_data$distance_to_largestcity2 <- ifelse(county_output_data$contains_largestcity == 1,0,county_output_data$distance_to_largestcity_mi)

#The formula in Clark (1951) is d_x = d_0*e^(-bx), where x is distance to metropolitan center, d_x is density, and b is the density gradient
#We want to isolate b: b = (ln d_0 - ln d_x)/x
#smaller the b, the more suburban the metropolitan area is

#NOTE: PUT THIS ON THE BACKBURNER
#I'M NOT SURE THIS IS THE BEST MEASURE; TAKE THIS FOR INSTANCE, TWO COUNTIES 2000 MILES AWAY FROM CITY CENTER (A, B), AND A COUNTY 200 MILES AWAY (C)
#Suppose ln(d_0) = 500, ln(d_A) = 300, ln(d_B) = 100, and ln(d_C) = 300. Then the implied b for each is: A- 0.1, B- 0.2, C- 1
#So, county A is the most suburban, B is less suburban, and C is really not suburban
#The problem is we don't have a gradient with a defined order; C has a higher population density, but b/c of proximity to city center it
#has a really high b and thus isn't considered 'suburban'. But it's being 'less suburban' doesn't make it more rural, whereas in the case
#of county B, its higher b stems from its lower population density and is more akin to a rural county
#Thus b doesn't tell us anything meaningful; there seems to be some sweet spot to minimize b, but counties with higher b could be both
#pretty urban and pretty rural

#Alternative strategy: use population density as our benchmark
