#Richard Jin
#Date: 4/12/21

#2_county_classification_logit.R
#Start with county-level dataset created in 1_cleanGISoutput.R and create classification model using logistic regression
#goal is to classify each county as suburb or city/urban based on the features defined in 1_cleanGISoutput.R

setwd("/Users/richardjin/Documents/ECON 210A/Project/")

library(plyr)
library(dplyr)
require(caret)

#consistent randomization to split into training and test sets
set.seed(123456)

#Input
county_output_data <- read.csv("./Data/Intermediate/county_1970_characteristics_step1.csv")
#The following step is crucial- for all counties containing the MSA's largest city, we set the distance to largest city as 0. Previously, the distance
#would be nonzero b/c the county centroid was not the same as the point location of the largest city; this resulted in some inaccuracy issues with classification
county_output_data$distance_to_largestcity2 <- ifelse(county_output_data$contains_largestcity == 1,0,county_output_data$distance_to_largestcity_mi)
state_region_xwalk <- read.csv("./Data/Intermediate/StateRegion_xwalk.csv") #state-level dataset with Census regions
colnames(state_region_xwalk)<-c("STATENAM","Region","Subregion")
state_region_xwalk$Region <- as.factor(state_region_xwalk$Region)
state_region_xwalk$Subregion <- as.factor(state_region_xwalk$Subregion)

#Split into training and testing sets by MSA
MSA_list <- unique(county_output_data$MSACMSA)
alpha <- 0.7
inTrain <- sample(1:length(MSA_list), alpha*length(MSA_list)) #row numbers of MSAs that go into training set
train_MSA <- MSA_list[inTrain]
test_MSA <- MSA_list[-inTrain]
train_MSA_id <- as.data.frame(train_MSA) #MSA codes for training set
test_MSA_id <- as.data.frame(test_MSA) #MSA codes for test set
colnames(train_MSA_id) <- "MSACMSA"
colnames(test_MSA_id) <- "MSACMSA"
train_counties <- inner_join(county_output_data,train_MSA_id,by="MSACMSA") #training set
train_counties2 <- inner_join(train_counties,state_region_xwalk,by = 'STATENAM')
test_counties <- inner_join(county_output_data,test_MSA_id,by="MSACMSA") #test set
test_counties2 <- inner_join(test_counties,state_region_xwalk,by = 'STATENAM')

#Logistic regression
require(caret)
TrainingParameters <- trainControl(method = "cv", number = 5, classProbs = TRUE,summaryFunction = twoClassSummary)

train_counties2$county_suburbdummy_char <- ifelse(train_counties2$county_suburbdummy == 1,"S","C")
test_counties2$county_suburbdummy_char <- ifelse(test_counties2$county_suburbdummy == 1,"S","C")
train_counties2$county_suburbdummy_factor <- as.factor(train_counties2$county_suburbdummy_char)
test_counties2$county_suburbdummy_factor <- as.factor(test_counties2$county_suburbdummy_char)

train_counties2$contains_largestcity_char <- ifelse(train_counties2$contains_largestcity == 1,"Y","N")
test_counties2$contains_largestcity_char <- ifelse(test_counties2$contains_largestcity == 1,"Y","N")
train_counties2$contains_largestcity_factor <- as.factor(train_counties2$contains_largestcity_char)
test_counties2$contains_largestcity_factor <- as.factor(test_counties2$contains_largestcity_char)

#GLM - Logistic Regression- test 6 different models; based on performance, 'mod4' has highest accuracy- over 87%; we calibrate to 'Sensitivity', i.e. accurately classifying cities
mod1 <- train(county_suburbdummy_factor~county_popdensity_sqmile+contains_largestcity_factor+distance_to_largestcity_mi+Region+homeowner_share+share_nonwhite+income_percapita, data = train_counties2, method = 'glm',metric = 'Sens',family='binomial',trControl = TrainingParameters)
mod2 <- train(county_suburbdummy_factor~county_popdensity_sqmile+distance_to_largestcity2+Subregion+average_homevalue_owned+homeowner_share+share_nonwhite+unemployment_rate+income_percapita, data = train_counties2, method = 'glm',metric = 'Sens',family='binomial',trControl = TrainingParameters) 
mod3 <- train(county_suburbdummy_factor~county_popdensity_sqmile+distance_to_largestcity2+Subregion, data = train_counties2, method = 'glm',metric = 'Sens',family='binomial',trControl = TrainingParameters) 
mod4 <- train(county_suburbdummy_factor~county_popdensity_sqmile+distance_to_largestcity2+share_HS+share_nonwhite+Subregion, data = train_counties2, method = 'glm',metric = 'Sens',family='binomial',trControl = TrainingParameters) 
mod5 <- train(county_suburbdummy_factor~county_popdensity_sqmile+distance_to_largestcity2+share_Bachplus+share_nonwhite+Subregion, data = train_counties2, method = 'glm',metric = 'Sens',family='binomial',trControl = TrainingParameters) 
mod6 <- train(county_suburbdummy_factor~county_popdensity_sqmile+distance_to_largestcity2+share_nonwhite+average_homevalue_owned+Subregion, data = train_counties2, method = 'glm',metric = 'Sens',family='binomial',trControl = TrainingParameters) 

#Train Errors
pred_train = predict(mod4,train_counties2)
confusionMatrix(pred_train, train_counties2$county_suburbdummy_factor)

#Test Errors
pred_test = predict(mod4,test_counties2)
confusionMatrix(pred_test, test_counties2$county_suburbdummy_factor)

#Now, use the model developed on the training set to apply predictions for all observations
#specifically, we calculate the implied probability P(Suburb)
pred_train_prob <- predict(mod4,train_counties2,type = "prob")
pred_test_prob <- predict(mod4,test_counties2,type = "prob")
predictions_all <- rbind(pred_train_prob,pred_test_prob)
probabilities_logit <- as.data.frame(predictions_all[,2])
colnames(probabilities_logit) <- c("Prob_Suburb_logit")

#Merge onto county-level dataset; keep relevant vars that go into the main spec
fullsamp <- rbind(train_counties2,test_counties2)
fullsamp2 <- cbind(fullsamp,probabilities_logit)
county_output_2class <- fullsamp2[,c(3:12,14:16,26,27,32)]
write.csv(county_output_2class,"./Data/Intermediate/counties_step2_logit.csv")

#Make Variable Importance Plot for our model
varImp_mod <- varImp(mod4)
#jpeg("./Output/Figures/classification_2_VarImpPlot.jpeg", width = 650, height = 650, quality = 100)
plot(varImp_mod)
#dev.off()


