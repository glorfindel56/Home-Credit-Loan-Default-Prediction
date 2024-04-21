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
#install.packages("hot.deck.match")
#library(hot.deck.match)
#install.packages("mice")
library(mice)
set.seed(5678) #lucky seed-could be anything really
options(scipen=999) #turn off pesky scientific notation
options(max.print = 10000) #allow for more options to print

# reading in the data
application <- read_csv("C:\\Users\\jonca\\Box\\MGT 6203\\application_data.csv", 
                        col_names = TRUE)
#picking up from trimmed
application_trimmed <- read.csv(file = "C:\\Users\\jonca\\Box\\MGT 6203\\application_trimmed.csv")
#time for some preliminary data analysis to get a better feel for our data
df<-application
str(df)
table(df$NAME_INCOME_TYPE)
table(df$CODE_GENDER)
cor(df$TARGET,df$AMT_CREDIT)

#get list of variables starting with "AMT_", which are distinct from "AMT_REQ_CREDIT_BUREAU" vars
amt_list<-grep("^AMT_(?!REQ_CREDIT_BUREAU)",names(df),value=TRUE,perl=TRUE)

plot_list <- list()
# Iterate over each variable in the list
for (variable in amt_list) {
  # Create box plot for the variable
  plot <- ggplot(df, aes(x = "", y = !!rlang::sym(variable))) +
    geom_boxplot() +
    labs(title = paste("Box Plot of", variable))
  # Add the plot to the list
  plot_list[[variable]] <- plot
}
# Print or display the box plots
for (variable in amt_list) {
  print(plot_list[[variable]])
}
#data highly skewed with presence of outliers, with very large data ranges-will transform
#need to impute missing values prior to transformation
#want to apply hot-deck imputation, which requires "donor" variables for similar respondents: traditionally used with demographic variables
demographics_list<-c("CODE_GENDER","CNT_CHILDREN","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","DAYS_BIRTH","NAME_HOUSING_TYPE")
#table(df_trimmed$NAME_EDUCATION_TYPE,df_trimmed$INCOME_BRACKET)

num_demographic <- numeric(length(demographics_list))
# Iterate over each variable in the demographics_list
for (i in seq_along(demographics_list)) {
  # Calculate the number of missing values for the current variable
  num_demographic[i] <- sum(is.na(application_trimmed[[demographics_list[i]]]))
}
# Create a dataframe with variable names and their corresponding missingness counts
missing_demographic <- data.frame(variable = demographics_list, missing_count = num_demographic)
# Print the missingness counts
print(missing_demographic)
#there is no missingness in the demographic variables-we will create valuable constructed-variables for use in imputation
df <- df %>%
  mutate(HAS_CHILDREN = ifelse(CNT_CHILDREN > 0, 1, 0),
#         EDUCATION_LEVEL = case_when( NAME_EDUCATION_TYPE %in% 
#                                        c("Higher education", "Academic degree") ~ "college_graduate",
#                                      TRUE ~ "highschool_graduate"),
         EDUCATION_LEVEL = case_when( NAME_EDUCATION_TYPE %in% 
                                        c("Higher education", "Academic degree") ~ "college_graduate",
                                      NAME_EDUCATION_TYPE %in% c("Incomplete higher", "Secondary / secondary special")~"highschool_graduate",
                                      TRUE ~ "highschool_incomplete"),         
         AGE_IN_YEARS = floor(-DAYS_BIRTH / 365.25),
         AGE_BUCKET = cut(AGE_IN_YEARS, breaks = c(18, 26, 46, 65, Inf),
                          labels = c("18-25", "26-45", "46-64", "65+"), right = FALSE),
         MARRIED = case_when(NAME_FAMILY_STATUS %in% c("Civil marriage","Married") ~ 1, 
                             TRUE ~ 0),
         EMPLOYED_IN_YEARS = ifelse(DAYS_EMPLOYED == 365243, 50, #assign retired people a value of 50 years employed-easy to differentiate from rest
                                    floor( -DAYS_EMPLOYED / 365.25)),
         INCOME_BRACKET = cut(AMT_INCOME_TOTAL, 
                              breaks = c(0, 50001, 100001, 150001, 200001, 250001, 300001, Inf),
                              labels = c("0-50k", "50k-100k", "100k-150k", "150k-200k", 
                                         "200k-250k", "250k-300k", "300k+"), include.lowest = TRUE))


#Investigate other variables that should be dropped prior to imputation
table(df$NAME_INCOME_TYPE)
#Avoid using NAME_INCOME_TYPE as its criteria are not well documented/established in the datafile source
table(df$ORGANIZATION_TYPE)
#ORGANIZATION_TYPE has significant amount of NA values. The best source of Industry/Occupation upcoding is 
#the Census NIOCCS system-if we were employees of this organization we would pursue that level of detail,
#but that is beyond the scope of a student project

table(df$NAME_HOUSING_TYPE, df$AGE_BUCKET)
table(df$NAME_CONTRACT_TYPE) #useful-describe in more detail
table(df$NAME_TYPE_SUITE) # not useful-describe in more detail

table(df$NAME_HOUSING_TYPE) # not useful-describe in more detail

sum(is.na(df$DAYS_EMPLOYED)) # all (-) values, when 365243 == pensioner or unemployed (but still has income?)
sum(is.na(df$AMT_INCOME_TOTAL)) #income is always filled out

retired_rows <- df[df$DAYS_EMPLOYED == 365243, ] #checking these odd values
table(retired_rows$NAME_INCOME_TYPE) #22 unemployed, 55352 pensioners

table(df$REGION_POPULATION_RELATIVE, df$AGE_BUCKET) #not useful
table(df$DAYS_REGISTRATION) # not useful

table(df$DAYS_ID_PUBLISH) # not useful
#id_change <- df[df$DAYS_ID_PUBLISH == 0,] #could be due to getting married
table(df$OCCUPATION_TYPE) # maybe useful? 31.3% data missing (96391/307511) but there's 55352 retired so in actuality there's only 13.3% missing data
table(df$CNT_FAM_MEMBERS) # not useful
table(df$REGION_RATING_CLIENT) # not useful

table(df$OCCUPATION_TYPE, df$NAME_INCOME_TYPE) #checking what the "" entails in occuptation type

keeplist<-c("SK_ID_CURR","TARGET","NAME_CONTRACT_TYPE","CODE_GENDER","FLAG_OWN_CAR",
            "CNT_CHILDREN","AMT_INCOME_TOTAL","AMT_CREDIT","AMT_ANNUITY","AMT_GOODS_PRICE",
            "NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","DAYS_BIRTH","DAYS_EMPLOYED",
            "OCCUPATION_TYPE","EXT_SOURCE_1","EXT_SOURCE_2","OBS_30_CNT_SOCIAL_CIRCLE",
            "DEF_30_CNT_SOCIAL_CIRCLE","OBS_60_CNT_SOCIAL_CIRCLE","DEF_60_CNT_SOCIAL_CIRCLE",
            "DAYS_LAST_PHONE_CHANGE","AMT_REQ_CREDIT_BUREAU_YEAR","HAS_CHILDREN",
            "EDUCATION_LEVEL","AGE_IN_YEARS","AGE_BUCKET","MARRIED","EMPLOYED_IN_YEARS",
            "INCOME_BRACKET")
df_trimmed <- df %>%
  select(keeplist)

write.csv(df_trimmed,file = "C:\\Users\\jonca\\Box\\MGT 6203\\application_trimmed_kai_30_cols.csv")

num_keeplist <- numeric(length(keeplist))
for (i in seq_along(keeplist)) {
  num_keeplist[i] <- sum(is.na(application_trimmed[[keeplist[i]]]))}
missing_keeplist <- data.frame(variable = keeplist, missing_count = num_keeplist)
print(missing_keeplist)

# Example using hot.deck package
# Specify matching criteria (e.g., demographic variables) - will create numerical versions
matching_criteria <- c("AGE_IN_YEARS_NUM", "INCOME_BRACKET_NUM", "CODE_GENDER_NUM", "MARRIED_NUM", "EDUCATION_LEVEL_NUM", "HAS_CHILDREN_NUM")
table(df_trimmed$CODE_GENDER) #4 cases of missing gender not caught before due to having values of XNA instead of "NA"
#we lack the information required to properly impute gender, I suggest dropping these 4 cases as their gender will be used to impute other values 
df_trimmed <- subset(df_trimmed, CODE_GENDER != "XNA")
table(df_trimmed$CODE_GENDER) 
#hot-deck imputation requires numeric variables as inputs, converting them to numeric just for this step to serve as donors
temp_for_imp <- df_trimmed %>%
  mutate(HAS_CHILDREN_NUM=HAS_CHILDREN,
         #         EDUCATION_LEVEL = case_when( NAME_EDUCATION_TYPE %in% 
         #                                        c("Higher education", "Academic degree") ~ "college_graduate",
         #                                      TRUE ~ "highschool_graduate"),
         EDUCATION_LEVEL_NUM = case_when(EDUCATION_LEVEL %in% 
                                        c("college_graduate") ~ 3,
                                        EDUCATION_LEVEL %in% c("highschool_graduate")~2,
                                      TRUE ~ 1),         
         AGE_IN_YEARS_NUM =  AGE_IN_YEARS,
         MARRIED_NUM = MARRIED,
         CODE_GENDER_NUM=case_when(CODE_GENDER %in% c("M")~0, TRUE~1),
#         INCOME_BRACKET_NUM = cut(AMT_INCOME_TOTAL, 
#                              breaks = c(0, 50001, 100001, 150001, 200001, 250001, 300001, Inf),
#                              labels = c("0-50k", "50k-100k", "100k-150k", "150k-200k", 
#                                         "200k-250k", "250k-300k", "300k+"), include.lowest = TRUE))
         INCOME_BRACKET_NUM = case_when(INCOME_BRACKET %in% c("0-50k")~1,
                                        INCOME_BRACKET %in% c("50k-100k")~2,
                                        INCOME_BRACKET %in% c("100k-150k")~3,
                                        INCOME_BRACKET %in% c("150k-200k")~4,
                                        INCOME_BRACKET %in% c("200k-250k")~5,
                                        INCOME_BRACKET %in% c("250k-300k")~6,
                                        INCOME_BRACKET %in% c("300k+")~7))
#table(temp_for_imp$MARRIED,temp_for_imp$MARRIED_NUM)
#table(temp_for_imp$INCOME_BRACKET,temp_for_imp$INCOME_BRACKET_NUM)
#table(temp_for_imp$AGE_IN_YEARS,temp_for_imp$AGE_IN_YEARS_NUM)
#table(temp_for_imp$EDUCATION_LEVEL,temp_for_imp$EDUCATION_LEVEL_NUM)
#table(temp_for_imp$HAS_CHILDREN,temp_for_imp$HAS_CHILDREN_NUM)
#table(temp_for_imp$CODE_GENDER,temp_for_imp$CODE_GENDER_NUM)

# Perform hot-deck imputation with nearest neighbor matching\
str(temp_for_imp)

#charvars_and_id<-c("SK_ID_CURR", names(temp_for_imp)[sapply(temp_for_imp, function(x) !is.numeric(x))])
#hot deck imputation requires only numeric variables in dataset, we will merge these back in after imputation
#they don't have any missingness it will be fine

#vars_with_missing <- colnames(temp_for_imp)[colSums(is.na(temp_for_imp)) > 0]
# Create the imputation model
imputation_model <- mice(temp_for_imp, match.matrix = matching_criteria)

# Perform multiple imputation
application_imputed <- complete(imputation_model)

num_keeplist <- numeric(length(keeplist))
for (i in seq_along(keeplist)) {
  num_keeplist[i] <- sum(is.na(application_imputed[[keeplist[i]]]))}
missing_keeplist <- data.frame(variable = keeplist, missing_count = num_keeplist)
print(missing_keeplist) #OCCUPATION_TYPE would be difficult to impute without access to NIOCCS
#in the future we may consider converting OCCUPATION_TYPE to numeric codes and then imputing for similar respondent demographics

write.csv(application_imputed,file = "C:\\Users\\jonca\\Box\\MGT 6203\\application_imputed.csv")

#time to begin a preliminary model
simplemodel<-glm(data=application_imputed,TARGET~AMT_INCOME_TOTAL+AMT_CREDIT+AMT_ANNUITY+AMT_GOODS_PRICE)
#plot(simplemodel)
summary(simplemodel)

simplemodel_log<-glm(data=application_imputed,TARGET~log(AMT_INCOME_TOTAL)+log(AMT_CREDIT)+log(AMT_ANNUITY)+log(AMT_GOODS_PRICE))
#plot(simplemodel_log)
summary(simplemodel_log) #very bad fitness, it's clear we need to create interactions between variables
#This gives us a sense of direction, as we should create ratios of income to credit, income to annuity, and income to goods price

testmodel<-glm(data=application_imputed,TARGET~log(AMT_INCOME_TOTAL)+log(AMT_CREDIT)+log(AMT_ANNUITY)+log(AMT_GOODS_PRICE)+CODE_GENDER+AGE_IN_YEARS+EDUCATION_LEVEL+MARRIED+HAS_CHILDREN+EMPLOYED_IN_YEARS)
summary(testmodel)
# Perform stepwise regression using AIC as the criterion
#stepwise_model <- step(initial_model, direction = "both") - very long runtime, omit for now
# Print summary of the final stepwise model
#summary(stepwise_model)

#demoonly<-glm(data=application_imputed,TARGET~CODE_GENDER+AGE_IN_YEARS+EDUCATION_LEVEL_NUM+MARRIED+HAS_CHILDREN+EMPLOYED_IN_YEARS)
#summary(demoonly)
