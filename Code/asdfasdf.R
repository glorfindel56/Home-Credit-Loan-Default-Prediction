.libPaths("C:\\Users\\jonca\\Desktop\\School\\R")
library(caret)
library(kknn)
library(rmarkdown)
library(tidyverse)
library(tidyr)
library(dplyr)
library(Hmisc)
library(kernlab)
library(ggplot2)
library(reshape2)
library(psychTools)
library(psych)
library(hot.deck)
library(mice)
library(cv)
library(pROC)
library(randomForest)
library(boot)
library(steprf)
set.seed(5678) #lucky seed-could be anything really
options(scipen=999) #turn off pesky scientific notation
options(max.print = 10000) #allow for more options to print

#JONCARLO-commenting out a large section because they were used for data exploration

# #dummies <- read_csv("C:\\Users\\jonca\\Box\\MGT 6203\\application_imputed_dummy_vars.csv", col_names =TRUE)
# imputed <- read_csv("C:\\Users\\jonca\\Box\\MGT 6203\\application_imputed_dummy_vars.csv", col_names =TRUE)
# 
# rawimputed <- read_csv("C:\\Users\\jonca\\Box\\MGT 6203\\application_imputed.csv", col_names = TRUE)
# #imputed<-merge(dummies,rawimputed,by="SK_ID_CURR", all.x=TRUE, suffixes=c("", ""))
# 
# # asdf<-as.data.frame(prop.table(table(rawimputed$EDUCATION_LEVEL,rawimputed$TARGET),1))
# # asdf
# 
# rawimputed <- rawimputed %>%
#   mutate(CREDIT_TO_INCOME_RATIO = AMT_CREDIT/AMT_INCOME_TOTAL,
#          CREDIT_TO_ANNUITY_RATIO = AMT_CREDIT/AMT_ANNUITY,
#          CREDIT_TO_GOODS_PRICE_RATIO = AMT_CREDIT/AMT_GOODS_PRICE)
# 
# rawimputed$OCCUPATION_TYPE[is.na(rawimputed$OCCUPATION_TYPE)] <- "Unknown"
# rawimputed$TARGET <- as.factor(rawimputed$TARGET)
# 
# 
# #write.csv(rawimputed,file = "C:\\Users\\jonca\\Box\\MGT 6203\\joncarlo_temp_data.csv")
# 
# 
# 
# # describe(imputed$CREDIT_TO_INCOME_RATIO)
# # describe(imputed$CREDIT_TO_ANNUITY_RATIO)
# # describe(imputed$CREDIT_TO_GOODS_PRICE_RATIO)
# 
# # missing_values <- colSums(is.na(imputed)) > 0
# # variables_with_missingness <- names(missing_values[missing_values])
# # print(variables_with_missingness)
# 
# # table(rawimputed$TARGET,rawimputed$HOUSING_TYPE)
# 
# 
# 
# imputed <- imputed %>%
#   mutate(CREDIT_TO_INCOME_RATIO = AMT_CREDIT/AMT_INCOME_TOTAL,
#          CREDIT_TO_ANNUITY_RATIO = AMT_CREDIT/AMT_ANNUITY,
#          CREDIT_TO_GOODS_PRICE_RATIO = AMT_CREDIT/AMT_GOODS_PRICE)
# imputed$TARGET <- as.factor(imputed$TARGET)
# names(imputed)<-gsub(" ","_", names(imputed))
# names(imputed)<-gsub("-","_", names(imputed))
# names(imputed)<-gsub("\\+","_UP", names(imputed))
# 
# #write.csv(imputed,file = "C:\\Users\\jonca\\Box\\MGT 6203\\application_imputed_cleaner.csv")
# 
# 
# train_control <- trainControl(method = "cv", number = 5)
# demographics_model <- train(as.factor(TARGET) ~ `HAS_CHILDREN`+`AGE_IN_YEARS`+`EMPLOYED_IN_YEARS`+`CODE_GENDERF`+`NAME_CONTRACT_TYPERevolving_loans`+`FLAG_OWN_CARY`+`EDUCATION_LEVELhighschool_graduate`+`EDUCATION_LEVELcollege_graduate`+`INCOME_BRACKET100k_150k`+`INCOME_BRACKET150k_200k`+`INCOME_BRACKET200k_250k`+`INCOME_BRACKET250k_300k`+`INCOME_BRACKET300k_UP`+`INCOME_BRACKET50k_100k`
#                  ,            
#                data = imputed,           
#                method = "glm",             
#                trControl = train_control,
#                family="binomial")   
# summary(demographics_model)
# 
# predicted<-predict(demographics_model,type="prob")
# hehe<-predicted[, "1"]
# #hehedataframe<-as.data.frame(hehe)
# for (p in seq(0.01,0.25,0.01)) {
#   
#   monkey <- ifelse(hehe < p,0,1) ## You have to make sure it is aligned with your base line definition.
#   #print(paste('cutoff p =',p,'==>','Accuracy = ',round(mean(monkey == imputed$TARGET),4)))
#   paste0("THRESHOLD: ", p)
#   print(confusionMatrix(as.factor(monkey),imputed$TARGET,positive='0'))
# }
# 
# plot(predicted)
# 
# haha<-ifelse(hehe > 0.1,1,0)
# hahadataframe<-as.data.frame(haha)
# confusionMatrix(as.factor(hahadataframe$haha),imputed$TARGET,positive='1')
# 
# 
# prop.table(table(imputed$TARGET))






v2 <- read_csv("C:\\Users\\jonca\\Box\\MGT 6203\\application_imputed_cleaner_v2.csv", col_names =TRUE)

v3 <-subset(v2, select = -c(CODE_GENDERM,NAME_CONTRACT_TYPECash_loans,FLAG_OWN_CARN,NAME_FAMILY_STATUSUnknown,OCCUPATION_TYPEUnknown,EDUCATION_LEVELhighschool_incomplete,INCOME_BRACKET0_50k))

v3$TARGETLABEL<-factor(v3$TARGET,levels=c(0,1),labels=c("No payment difficulties", "Payment difficulties"))
#write.csv(v3,file = "C:\\Users\\jonca\\Box\\MGT 6203\\application_imputed_cleaner_v3.csv")


# rawimputed$TARGETLABEL<-factor(rawimputed$TARGET,levels=c(0,1),labels=c("No payment difficulties", "Payment difficulties"))



# # Convert the table to a rawimputed frame for ggplot
# prop_df <- as.data.frame(prop_cross_tab <- prop.table(table(rawimputed$INCOME_BRACKET, rawimputed$targetlabel), margin = 1))
# prop_df$INCOME_BRACKET <- rownames(prop_cross_tab)
# # Plot the bar chart
# ggplot(prop_df, aes(x = Var1, y = Freq, fill = Var2)) +
#   geom_bar(stat = "identity", position = "stack") +
#   labs(title = "Proportion of TARGET by Income Bracket",
#        x = "Income Bracket",
#        y = "Proportion",
#        fill = "TARGET") +
#   theme_minimal()




############ random forest classification
train_index <- sample(1:nrow(v3), 0.8 * nrow(v3))
train_data <- v3[train_index,]
test_data <- v3[-train_index,]
prop.table(table(train_data$TARGET))
prop.table(table(test_data$TARGET))

forest <- randomForest(as.factor(TARGET)~`AMT_INCOME_TOTAL`+`AMT_CREDIT`+`AMT_ANNUITY`+`AMT_GOODS_PRICE`+`EXT_SOURCE_1`+`EXT_SOURCE_2`+`OBS_30_CNT_SOCIAL_CIRCLE`+`DEF_30_CNT_SOCIAL_CIRCLE`+`OBS_60_CNT_SOCIAL_CIRCLE`+`DEF_60_CNT_SOCIAL_CIRCLE`+`DAYS_LAST_PHONE_CHANGE`+`AMT_REQ_CREDIT_BUREAU_YEAR`+`HAS_CHILDREN`+`AGE_IN_YEARS`+`EMPLOYED_IN_YEARS`+`CODE_GENDERF`+`NAME_CONTRACT_TYPERevolving_loans`+`FLAG_OWN_CARY`+`NAME_FAMILY_STATUSCivil_marriage`+`NAME_FAMILY_STATUSMarried`+`NAME_FAMILY_STATUSSeparated`+`NAME_FAMILY_STATUSSingle_not_married`+`NAME_FAMILY_STATUSWidow`+`OCCUPATION_TYPEAccountants`+`OCCUPATION_TYPECleaning_staff`+`OCCUPATION_TYPECooking_staff`+`OCCUPATION_TYPECore_staff`+`OCCUPATION_TYPEDrivers`+`OCCUPATION_TYPEHigh_skill_tech_staff`+`OCCUPATION_TYPEHR_staff`+`OCCUPATION_TYPEIT_staff`+`OCCUPATION_TYPELaborers`+`OCCUPATION_TYPELow_skill_Laborers`+`OCCUPATION_TYPEManagers`+`OCCUPATION_TYPEMedicine_staff`+`OCCUPATION_TYPEPrivate_service_staff`+`OCCUPATION_TYPERealty_agents`+`OCCUPATION_TYPESales_staff`+`OCCUPATION_TYPESecretaries`+`OCCUPATION_TYPESecurity_staff`+`OCCUPATION_TYPEWaiters_barmen_staff`+`EDUCATION_LEVELcollege_graduate`+`EDUCATION_LEVELhighschool_graduate`+`INCOME_BRACKET100k_150k`+`INCOME_BRACKET150k_200k`+`INCOME_BRACKET200k_250k`+`INCOME_BRACKET250k_300k`+`INCOME_BRACKET300k_UP`+`INCOME_BRACKET50k_100k`
                       , data = train_data, classwt=c("0"=1, "1"=2))

predictions <- predict(forest, test_data, type="response")

confusion_matrix <- table(predictions, test_data$TARGET)
confusionMatrix(confusion_matrix)


table(test_data$TARGET)










forest2 <- randomForest(as.factor(TARGET)~`AMT_INCOME_TOTAL`+`AMT_CREDIT`+`AMT_ANNUITY`+`AMT_GOODS_PRICE`+`EXT_SOURCE_1`+`EXT_SOURCE_2`+`OBS_30_CNT_SOCIAL_CIRCLE`+`DEF_30_CNT_SOCIAL_CIRCLE`+`OBS_60_CNT_SOCIAL_CIRCLE`+`DEF_60_CNT_SOCIAL_CIRCLE`+`DAYS_LAST_PHONE_CHANGE`+`AMT_REQ_CREDIT_BUREAU_YEAR`+`HAS_CHILDREN`+`AGE_IN_YEARS`+`EMPLOYED_IN_YEARS`+`CODE_GENDERF`+`NAME_CONTRACT_TYPERevolving_loans`+`FLAG_OWN_CARY`+`NAME_FAMILY_STATUSCivil_marriage`+`NAME_FAMILY_STATUSMarried`+`NAME_FAMILY_STATUSSeparated`+`NAME_FAMILY_STATUSSingle_not_married`+`NAME_FAMILY_STATUSWidow`+`OCCUPATION_TYPEAccountants`+`OCCUPATION_TYPECleaning_staff`+`OCCUPATION_TYPECooking_staff`+`OCCUPATION_TYPECore_staff`+`OCCUPATION_TYPEDrivers`+`OCCUPATION_TYPEHigh_skill_tech_staff`+`OCCUPATION_TYPEHR_staff`+`OCCUPATION_TYPEIT_staff`+`OCCUPATION_TYPELaborers`+`OCCUPATION_TYPELow_skill_Laborers`+`OCCUPATION_TYPEManagers`+`OCCUPATION_TYPEMedicine_staff`+`OCCUPATION_TYPEPrivate_service_staff`+`OCCUPATION_TYPERealty_agents`+`OCCUPATION_TYPESales_staff`+`OCCUPATION_TYPESecretaries`+`OCCUPATION_TYPESecurity_staff`+`OCCUPATION_TYPEWaiters_barmen_staff`+`EDUCATION_LEVELcollege_graduate`+`EDUCATION_LEVELhighschool_graduate`+`INCOME_BRACKET100k_150k`+`INCOME_BRACKET150k_200k`+`INCOME_BRACKET200k_250k`+`INCOME_BRACKET250k_300k`+`INCOME_BRACKET300k_UP`+`INCOME_BRACKET50k_100k`
                       , data = train_data, classwt=c("0"=1, "1"=5))

predictions2 <- predict(forest2, test_data, type="response")

confusion_matrix2 <- table(predictions2, test_data$TARGET)
confusionMatrix(confusion_matrix2)


#################
forest3 <- randomForest(as.factor(TARGET)~`AMT_INCOME_TOTAL`+`AMT_CREDIT`+`AMT_ANNUITY`+`AMT_GOODS_PRICE`+`EXT_SOURCE_1`+`EXT_SOURCE_2`+`OBS_30_CNT_SOCIAL_CIRCLE`+`DEF_30_CNT_SOCIAL_CIRCLE`+`OBS_60_CNT_SOCIAL_CIRCLE`+`DEF_60_CNT_SOCIAL_CIRCLE`+`DAYS_LAST_PHONE_CHANGE`+`AMT_REQ_CREDIT_BUREAU_YEAR`+`HAS_CHILDREN`+`AGE_IN_YEARS`+`EMPLOYED_IN_YEARS`+`CODE_GENDERF`+`NAME_CONTRACT_TYPERevolving_loans`+`FLAG_OWN_CARY`+`NAME_FAMILY_STATUSCivil_marriage`+`NAME_FAMILY_STATUSMarried`+`NAME_FAMILY_STATUSSeparated`+`NAME_FAMILY_STATUSSingle_not_married`+`NAME_FAMILY_STATUSWidow`+`OCCUPATION_TYPEAccountants`+`OCCUPATION_TYPECleaning_staff`+`OCCUPATION_TYPECooking_staff`+`OCCUPATION_TYPECore_staff`+`OCCUPATION_TYPEDrivers`+`OCCUPATION_TYPEHigh_skill_tech_staff`+`OCCUPATION_TYPEHR_staff`+`OCCUPATION_TYPEIT_staff`+`OCCUPATION_TYPELaborers`+`OCCUPATION_TYPELow_skill_Laborers`+`OCCUPATION_TYPEManagers`+`OCCUPATION_TYPEMedicine_staff`+`OCCUPATION_TYPEPrivate_service_staff`+`OCCUPATION_TYPERealty_agents`+`OCCUPATION_TYPESales_staff`+`OCCUPATION_TYPESecretaries`+`OCCUPATION_TYPESecurity_staff`+`OCCUPATION_TYPEWaiters_barmen_staff`+`EDUCATION_LEVELcollege_graduate`+`EDUCATION_LEVELhighschool_graduate`+`INCOME_BRACKET100k_150k`+`INCOME_BRACKET150k_200k`+`INCOME_BRACKET200k_250k`+`INCOME_BRACKET250k_300k`+`INCOME_BRACKET300k_UP`+`INCOME_BRACKET50k_100k`
                        , data = train_data, classwt=c("0"=1, "1"=20))

predictions3 <- predict(forest3, test_data, type="response")

confusion_matrix3 <- table(predictions3, test_data$TARGET)
confusionMatrix(confusion_matrix3)


asdf <- predict(forest3, test_data, type="prob")
roc_curve <- roc(test_data$TARGET, asdf[,2])
auc_value <- auc(roc_curve)
auc_value










forest4 <- randomForest(as.factor(TARGET)~`AMT_INCOME_TOTAL`+`AMT_CREDIT`+`AMT_ANNUITY`+`AMT_GOODS_PRICE`+`EXT_SOURCE_1`+`EXT_SOURCE_2`+`OBS_30_CNT_SOCIAL_CIRCLE`+`DEF_30_CNT_SOCIAL_CIRCLE`+`OBS_60_CNT_SOCIAL_CIRCLE`+`DEF_60_CNT_SOCIAL_CIRCLE`+`DAYS_LAST_PHONE_CHANGE`+`AMT_REQ_CREDIT_BUREAU_YEAR`+`HAS_CHILDREN`+`AGE_IN_YEARS`+`EMPLOYED_IN_YEARS`+`CODE_GENDERF`+`NAME_CONTRACT_TYPERevolving_loans`+`FLAG_OWN_CARY`+`NAME_FAMILY_STATUSCivil_marriage`+`NAME_FAMILY_STATUSMarried`+`NAME_FAMILY_STATUSSeparated`+`NAME_FAMILY_STATUSSingle_not_married`+`NAME_FAMILY_STATUSWidow`+`OCCUPATION_TYPEAccountants`+`OCCUPATION_TYPECleaning_staff`+`OCCUPATION_TYPECooking_staff`+`OCCUPATION_TYPECore_staff`+`OCCUPATION_TYPEDrivers`+`OCCUPATION_TYPEHigh_skill_tech_staff`+`OCCUPATION_TYPEHR_staff`+`OCCUPATION_TYPEIT_staff`+`OCCUPATION_TYPELaborers`+`OCCUPATION_TYPELow_skill_Laborers`+`OCCUPATION_TYPEManagers`+`OCCUPATION_TYPEMedicine_staff`+`OCCUPATION_TYPEPrivate_service_staff`+`OCCUPATION_TYPERealty_agents`+`OCCUPATION_TYPESales_staff`+`OCCUPATION_TYPESecretaries`+`OCCUPATION_TYPESecurity_staff`+`OCCUPATION_TYPEWaiters_barmen_staff`+`EDUCATION_LEVELcollege_graduate`+`EDUCATION_LEVELhighschool_graduate`+`INCOME_BRACKET100k_150k`+`INCOME_BRACKET150k_200k`+`INCOME_BRACKET200k_250k`+`INCOME_BRACKET250k_300k`+`INCOME_BRACKET300k_UP`+`INCOME_BRACKET50k_100k`
                        , data = train_data, classwt=c("0"=1, "1"=100))

predictions4 <- predict(forest4, test_data, type="response")

confusion_matrix4 <- table(predictions4, test_data$TARGET)
confusionMatrix(confusion_matrix3)


asdf4 <- predict(forest4, test_data, type="prob")
roc_curve4 <- roc(test_data$TARGET, asdf4[,2])
auc_value4 <- auc(roc_curve4)
auc_value4





























data_0 <- v3 %>% 
  filter(TARGET == 0)
data_1 <- v3 %>%
  filter(TARGET == 1)
# separate the dataset into 80/20 (80% training and validation for cross validation and 20% for test)
n_1 <- sum(v3$TARGET == 1)
n_0 <- sum(v3$TARGET == 0)
split_value <- 0.80
#we are randomly shuffling the entire dataset and then splitting it up according the the split_value we set above.
training_valid_data_points_0 <- sample(x = 1:n_0, size = as.integer(split_value*n_0), replace = FALSE)
training_valid_data_points_1 <- sample(x = 1:n_1, size = as.integer(split_value*n_1), replace = FALSE)
# subsetting the data based on TARGET = 0 and TARGET = 1 to get the same distribution for the TARGET variable in 
# the train/validation/test datasets
# train/validation dataset that will be used for k-fold cross-validation
train_valid_data_0 <- data_0[training_valid_data_points_0, ]
train_valid_data_1 <- data_1[training_valid_data_points_1, ]
# merging the separate train/validation datasets into one
train_data <- bind_rows(train_valid_data_0, train_valid_data_1) %>% 
  select(-c(SK_ID_CURR, TARGETLABEL, AMT_INCOME_TOTAL, DAYS_BIRTH, DAYS_EMPLOYED, HAS_CHILDREN_NUM, AGE_IN_YEARS_NUM,AGE_IN_YEARS, NAME_EDUCATION_TYPEAcademic_degree, NAME_EDUCATION_TYPEHigher_education, NAME_EDUCATION_TYPEIncomplete_higher, NAME_EDUCATION_TYPELower_secondary, NAME_EDUCATION_TYPESecondary_secondary_special))
# final dataset that will be used to analyze how well the best model (from k-fold cross-validation) performs
final_test_data_0 <- data_0[-training_valid_data_points_0, ]
final_test_data_1 <- data_1[-training_valid_data_points_1, ]
# merging the separate test datasets into one
test_data <- bind_rows(final_test_data_0, final_test_data_1) %>% 
  select(-c(SK_ID_CURR, TARGETLABEL, AMT_INCOME_TOTAL, DAYS_BIRTH, DAYS_EMPLOYED, HAS_CHILDREN_NUM, AGE_IN_YEARS_NUM, AGE_IN_YEARS, NAME_EDUCATION_TYPEAcademic_degree, NAME_EDUCATION_TYPEHigher_education, NAME_EDUCATION_TYPEIncomplete_higher, NAME_EDUCATION_TYPELower_secondary, NAME_EDUCATION_TYPESecondary_secondary_special))

forestjoyce <- randomForest(as.factor(TARGET)~., data = train_data, classwt=c("0"=1, "1"=20))

predictionsjoyce <- predict(forestjoyce, test_data, type="response")

confusion_matrixjoyce <- table(predictionsjoyce, test_data$TARGET)
confusionMatrix(confusion_matrixjoyce)


asdfjoyce <- predict(forestjoyce, test_data, type="prob")
roc_curvejoyce <- roc(test_data$TARGET, asdfjoyce[,2])
auc_valuejoyce <- auc(roc_curvejoyce)
auc_valuejoyce










n <- 100 # number of iterations, 60 to 100 is recommended.
measures <- NULL
for (i in 1:n) {
  rfcv1 <- RFcv2(as.data.frame(train_data[,2:ncol(train_data)]), unlist(train_data[,1]), predacc = "kappa", importance=TRUE)
  measures <- rbind(measures, rfcv1)
}
plot(measures ~ c(1:n), xlab = "Iteration for RF", ylab = "Correct
classification Rate (%)")
points(cumsum(measures) / c(1:n) ~ c(1:n), col = 2)
abline(h = mean(measures), col = 'blue', lwd = 2)