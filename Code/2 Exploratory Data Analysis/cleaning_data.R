######### cleaning combined Home Credit data #########
# this script will be cleaning the combined Home Credit data
# this includes removing variables that 1) contain too many missing values,
# 2) have regulatory concerns, 3) correlated with other variables

# read in the combined Home Credit data
home_credit <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/combined_data.csv", 
                        col_names = TRUE)

######### missing values #########
# missing value counts for each variable in the combined data set
# determine which variables to drop based on the amount of missing values it contains

# getting rid of scientific notation
options(scipen = 999)

# count missing values in each column
missing_counts <- home_credit %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(., cols = everything(), names_to = "variable", values_to = "missing_count") %>%
  arrange(desc(missing_count)) %>%
  mutate(missing_count_percentage = round((missing_count / nrow(home_credit)) * 100, 2))

# count missing values in each column for target = 1 (clients that defaulted)
# don't want to remove valuable data if the variables that have a significant amount of missing values 
# aren't missing for clients that defaulted on their loan
total_rows_d <- nrow(home_credit %>%
                       filter(target_a == 1))

missing_counts_d <- home_credit %>%
  filter(target_a == 1) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(., cols = everything(), names_to = "variable", values_to = "missing_count_d") %>%
  arrange(desc(missing_count_d)) %>%
  mutate(missing_count_percentage_d = round((missing_count_d / total_rows_d) * 100, 2))

# merging missing_counts and missing_counts_d to see if there is a pattern 
# do variables with a lot of missing values in the entire dataset also have a lot of missing values 
# in the default dataset?
missing_counts_2 <- missing_counts %>%
  left_join(., missing_counts_d, by = c("variable")) %>%
  mutate(diff = missing_count_percentage - missing_count_percentage_d)

# based on the results of missing_counts_2, there wasn't a big difference in missing count percentage between 
# the total dataset and default only dataset

# the following can be removed:

# those with greater than 90% missing values
greater_than_90p_na <- missing_counts$variable[missing_counts$missing_count_percentage > 90]

home_credit_2 <- home_credit %>%
  select(-all_of(greater_than_90p_na))

# variables containing information about the building the client lives in
# contains a lot of missing data and not very predictive in determining whether a client will default or not
# will be dropping the avg, mode, and median metrics for the building the client lives in

building_info <- c('commonarea_avg_a', 
                   'commonarea_mode_a',
                   'commonarea_medi_a',
                   'nonlivingapartments_avg_a',
                   'nonlivingapartments_mode_a',
                   'nonlivingapartments_medi_a',
                   'livingapartments_avg_a',
                   'livingapartments_mode_a',
                   'livingapartments_medi_a',
                   'floorsmin_avg_a',
                   'floorsmin_mode_a',
                   'floorsmin_medi_a',
                   'basementarea_avg_a',
                   'basementarea_mode_a',
                   'basementarea_median_a')

# filtering on variables that have more than 50% missing values




######### categorizing independent variables into categorical or continuous #########
# based on column descriptions given by Home Credit
# also based on intuition and studying the contents of the variable
# ex: zip code is a categorical variable even though it looks like a continuous variable
# the underlying packages in tidyverse might interpret it as continuous based on the first x entries
# up to modelers to inspect the data and ensure that the data is of the correct data type


######### finding variables that are highly correlated w each other #########
# categorical-categorical: finding if there are any correlations between the categorical independent variables
# categorical-continuous: correlations between the categorical and continuous independent variables
# continuous-continuous: correlations between the continuous independent variables
# categorical-target (dependent variable)
# continuous-target (dependent variable)
# vif

######### finding outliers #########
# using box-and-whisker plots for visualization
# another metric to determine outliers - Cook's distance
# interquartile range check (max = 1.5xIQ + 3rd quartile, min = ?)
# we have to be careful because clients that default may have some "outlier" inputs for certain variables






######### conducting exploratory data analysis #########

# installing necessary packages
# install.packages("gridExtra")
library(tidyverse)
library(gridExtra)

# reading in the data
application <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/application_data.csv", 
                        col_names = TRUE)

# converting column names to lower case for each dataset
colnames(application) <- str_to_lower(colnames(application))

# renaming columns by appending dataset initials to the end of each column in each dataset
application <- application %>% rename_with(~ paste0(., "_a"), everything())

# any duplicate rows in the application dataset?
# no duplicate rows, all distinct policy ID numbers
n_distinct(application$sk_id_curr_a)

# count of defaulters vs non-defaulters for categorical variables in a graph and table
combined <- function(data, variable, variable_english){
  
  graph <- data %>%
    mutate(target = factor(target, levels = c(0, 1), labels = c("No", "Yes"))) %>%
    group_by(!!sym(variable), target) %>%
    summarize(n = n(), .groups = "drop") %>% # suppress warning message
    mutate(!!sym(variable) := factor(!!sym(variable))) %>%
    ggplot(aes(x = !!sym(variable), y = n, fill = target)) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    labs(y = "Count", x = variable_english, title = "Number of Loans - Repaid vs Defaulted") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(name = "Default")
  
  table <- data %>%
    group_by(!!sym(variable), target) %>%
    summarize(n = n(), .groups = "drop") %>%
    pivot_wider(., names_from = target, values_from = n) %>%
    mutate(Default = round(.[ , c(3)] / rowSums(.[ , c(2, 3)]), 2)) %>%
    rename(!!!setNames(c(variable, 0, 1), c(variable_english, "No", "Yes")))
    
  table <- tableGrob(table)
  
  combined <- grid.arrange(table, graph, nrow = 2)
}

# categorical variables: exploratory analysis
contract_type <- combined(application, "name_contract_type", "Contract Type") # mostly cash loans, higher default rate w cash loans
gender <- combined(application, "code_gender", "Gender") # women take out much more number of loans than men
# women have a lower default rate too
own_car <- combined(application, "flag_own_car", "Own Car") # most clients do not own a car
own_real_estate <- combined(application, "flag_own_realty", "Own Real Estate") # most clients own a home
# interestingly, the default rate is about the same for those that own vs do not own a home (not intuitive)
num_of_children <- combined(application, "cnt_children_capped", "Own Children") # those with no children have a lower default rate
name_type <- combined(application, "name_type_suite", "Suite Type") # not very helpful visual, this variable can be dropped
income_type <- combined(application, "name_income_type", "Income Type")
education_type <- combined(application, "name_education_type", "Education Type")
family_status <- combined(application, "name_family_status", "Family Status")
housing_type <- combined(application, "name_housing_type", "Housing Type") # not very helpful visual, most are categorized under
# House / apartment - this variable can be dropped
own_car_age <- combined(application, "own_car_age", "Own Car") # most entries are NAs, drop the var
mobile_flag <- combined(application, "flag_mobil", "Phone Number Flag") # most entries are in the 1 group, drop the var

# creating a list of categorical variables that need to be inspected w exploratory analysis
cat_var <- c("flag_emp_phone_a", "flag_work_phone_a", "flag_cont_mobile_a", 
             "flag_phone_a", "flag_email_a", "occupation_type_a",
             "cnt_fam_members_a", "region_rating_client_a", "region_rating_client_w_city_a",
             "weekday_appr_process_start_a", "hour_appr_process_start_a", "reg_region_not_live_region_a",
             "reg_region_not_work_region_a", "live_region_not_work_region_a", "reg_city_not_live_city_a",
             "reg_city_not_work_city_a", "live_city_not_work_city_a", "organization_type_a",
             "fondkapremont_mode_a", "housetype_mode_a", "wallsmaterial_mode_a", 
             "obs_30_cnt_social_circle_a", "def_30_cnt_social_circle_a", "obs_60_cnt_social_circle_a",
             "def_60_cnt_social_circle_a", "flag_document_2_a", "flag_document_3_a", "flag_document_4_a",
             "flag_document_5_a", "flag_document_6_a", "flag_document_7_a",
             "flag_document_8_a", "flag_document_9_a", "flag_document_10_a",            
             "flag_document_11_a", "flag_document_12_a", "flag_document_13_a",           
             "flag_document_14_a", "flag_document_15_a", "flag_document_16_a",            
             "flag_document_17_a", "flag_document_18_a", "flag_document_19_a",           
             "flag_document_20_a", "flag_document_21_a", "amt_req_credit_bureau_hour_a",  
             "amt_req_credit_bureau_day_a", "amt_req_credit_bureau_week_a", "amt_req_credit_bureau_mon_a",   
             "amt_req_credit_bureau_qrt_a", "amt_req_credit_bureau_year_a")

for (i in cat_var) {
  combined(application, {{i}})
}

# defaulters vs non-defaulters for continuous variables in a graph
combined_2 <- function(data, variable, variable_english){
  
  graph <- data %>%
    filter(!is.na(!!sym(variable))) %>%
    mutate(target = factor(target, levels = c(0, 1), labels = c("No", "Yes"))) %>%
    group_by(!!sym(variable), target) %>%
    summarize(n = n(), .groups = "drop") %>% # suppress warning message
    ggplot(aes(x = !!sym(variable), fill = target)) +
    geom_density(alpha = 0.5) +
    labs(y = "Density", x = variable_english, title = "Number of Loans - Repaid vs Defaulted") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(name = "Default")
  
  table <- data %>%
    mutate(!!sym(variable) := if_else(is.na(!!sym(variable)), "missing", "not missing")) %>%
    group_by(!!sym(variable)) %>%
    summarize(Count = n(), .groups = "drop") %>%
    mutate(Percentage = round(Count / sum(Count), 4)) %>%
    rename(!!!setNames(variable, variable_english))
  
  table <- tableGrob(table)
  
  combined_2 <- grid.arrange(table, graph, nrow = 2)
}

# get rid of scientific notation
scipen(option = 99999)

# continuous variables: exploratory analysis
ext_source_1_a <- combined_2(application, "ext_source_1", "External Source 1") # density function looks really different between the two!
ext_source_2_a <- combined_2(application, "ext_source_2", "External Source 2") # looks different between the two
ext_source_3_a <- combined_2(application, "ext_source_3", "External Source 3") # does not look different between the two 
credit <- combined_2(application, "amt_credit", "Credit Amount")
age <- combined_2(application, "days_birth_in_years", "Age") # surprisingly no difference
apartments_avg <- combined_2(application, "apartments_avg", "Apartment - Average") # not much difference
years_build_avg <- combined_2(application, "years_build_avg", "Years Build - Average") # clients that live in homes constructed more 
# recently have a higher default rate?

# reading in the data
application_imputed <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/application_imputed.csv", 
                        col_names = TRUE)

# converting column names to lower case for each dataset
colnames(application_imputed) <- str_to_lower(colnames(application_imputed))

income_bracket <- combined(application_imputed, "income_bracket", "Income Bracket")
years_of_employment <- combined_2(application_imputed, "employed_in_years", "Years of Employment")
