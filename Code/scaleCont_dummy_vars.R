library(dplyr)
library(tidyverse)

app_data <-read_csv(file = "/Users/kfung/Library/CloudStorage/Box-Box/MGT 6203/application_imputed.csv",col_names = TRUE)

#remove ...1 and other cols from data
#removing CODE_GENDER_NUM etc bc will create indicator variable below, MARRIED IS NOT USEFUL
data <- subset(app_data,select=-c(DAYS_BIRTH,DAYS_EMPLOYED,CODE_GENDER_NUM,AGE_IN_YEARS_NUM,HAS_CHILDREN_NUM,MARRIED,MARRIED_NUM,INCOME_BRACKET_NUM,EDUCATION_LEVEL_NUM))

#add the feature engineering features
data <- data %>%
  mutate(CREDIT_TO_INCOME_RATIO = AMT_CREDIT/AMT_INCOME_TOTAL,
         CREDIT_TO_ANNUITY_RATIO = AMT_CREDIT/AMT_ANNUITY,
         CREDIT_TO_GOODS_PRICE_RATIO = AMT_CREDIT/AMT_GOODS_PRICE)

#scale the continous data!!!!
columns_to_scale <- c('CREDIT_TO_INCOME_RATIO', 'CREDIT_TO_ANNUITY_RATIO', 'CREDIT_TO_GOODS_PRICE_RATIO','AMT_INCOME_TOTAL' ,'AMT_CREDIT' ,'AMT_ANNUITY' ,'AMT_GOODS_PRICE' ,'EXT_SOURCE_1' ,'EXT_SOURCE_2' ,'OBS_30_CNT_SOCIAL_CIRCLE' ,'DEF_30_CNT_SOCIAL_CIRCLE' ,'OBS_60_CNT_SOCIAL_CIRCLE' ,'DEF_60_CNT_SOCIAL_CIRCLE' ,'DAYS_LAST_PHONE_CHANGE' ,'AMT_REQ_CREDIT_BUREAU_YEAR' ,'AGE_IN_YEARS' ,'EMPLOYED_IN_YEARS')
scaled_numeric_data <- scale(data[, columns_to_scale])
# Replace the scaled columns in the original data frame
data[, columns_to_scale] <- scaled_numeric_data

#impute OCCUPATION_TYPE
# Convert OCCUPATION_TYPE to factor
data$OCCUPATION_TYPE <- as.factor(data$OCCUPATION_TYPE)

# Add "Unknown" as a level to the factor
data$OCCUPATION_TYPE <- factor(data$OCCUPATION_TYPE, levels = c(levels(data$OCCUPATION_TYPE), "Unknown"))
# Replace NAs with "Unknown"
data$OCCUPATION_TYPE[is.na(data$OCCUPATION_TYPE)] <- "Unknown"
#can check imputation, there are 96389 unknowns
#table(data$OCCUPATION_TYPE)

#transform categorical variables to dummy binary vars
cat_vars <-c("CODE_GENDER","NAME_CONTRACT_TYPE","FLAG_OWN_CAR","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","OCCUPATION_TYPE","EDUCATION_LEVEL","AGE_BUCKET","INCOME_BRACKET")
# Loop over each categorical variable and create dummy variables
for (var in cat_vars) {
  print(paste("cur var:", var))
  # Create dummy variables using model.matrix
  dummy_variables <- model.matrix(~ . - 1, data = data[, var, drop = FALSE])
  
  # Bind the dummy variables to the original data frame
  data <- cbind(data, dummy_variables)
}

# Remove the original categorical variables
data <- data[, !names(data) %in% cat_vars, drop = FALSE]