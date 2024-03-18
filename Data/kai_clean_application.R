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
#install.packages("mice")
library(mice)
set.seed(5678) #lucky seed-could be anything really
options(scipen=999) #turn off pesky scientific notation
options(max.print = 10000) #allow for more options to print

# reading in the data
application <- read_csv("/Users/kfung/Library/CloudStorage/Box-Box/MGT 6203/application_data.csv", 
                        col_names = TRUE)
#picking up from trimmed
application_trimmed <- read.csv(file = "/Users/kfung/Library/CloudStorage/Box-Box/MGT 6203/application_trimmed.csv")
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
         EDUCATION_LEVEL = case_when( NAME_EDUCATION_TYPE %in% 
                  c("Higher education", "Academic degree") ~ "college_graduate",
                  TRUE ~ "highschool_graduate"),
         AGE_IN_YEARS = floor(-DAYS_BIRTH / 365.25),
         AGE_BUCKET = cut(AGE_IN_YEARS, breaks = c(18, 26, 46, 65, Inf),
                          labels = c("18-25", "26-45", "46-64", "65+"), right = FALSE),
         MARRIED = case_when(NAME_FAMILY_STATUS %in% c("Civil marriage","Married") ~ 1, 
                             TRUE ~ 0),
         EMPLOYED_IN_YEARS = ifelse(DAYS_EMPLOYED == 365243, NA,
                                   round( -DAYS_EMPLOYED / 365.25,2)),
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

df_trimmed <- df %>%
  select(SK_ID_CURR, TARGET, NAME_CONTRACT_TYPE, CODE_GENDER,FLAG_OWN_CAR,
         CNT_CHILDREN,AMT_INCOME_TOTAL,AMT_CREDIT,AMT_ANNUITY,AMT_GOODS_PRICE,
         NAME_EDUCATION_TYPE,NAME_FAMILY_STATUS,DAYS_BIRTH,DAYS_EMPLOYED,
         OCCUPATION_TYPE,EXT_SOURCE_1,EXT_SOURCE_2,OBS_30_CNT_SOCIAL_CIRCLE,
         DEF_30_CNT_SOCIAL_CIRCLE,OBS_60_CNT_SOCIAL_CIRCLE,DEF_60_CNT_SOCIAL_CIRCLE,
         DAYS_LAST_PHONE_CHANGE,AMT_REQ_CREDIT_BUREAU_YEAR,HAS_CHILDREN,
         EDUCATION_LEVEL,AGE_IN_YEARS,AGE_BUCKET,MARRIED,EMPLOYED_IN_YEARS,
         INCOME_BRACKET)

write.csv(df_trimmed,file = "/Users/kfung/Library/CloudStorage/Box-Box/MGT 6203/application_trimmed_kai_30_cols.csv")



