
######### conducting exploratory data analysis #########

# installing necessary packages
#install.packages("gridExtra")
library(tidyverse)
library(gridExtra)

# reading in the data
application <- read_csv("/Users/kfung/Library/CloudStorage/Box-Box/MGT 6203/application_data.csv", 
                        col_names = TRUE)

# converting column names to lower case for each dataset

colnames(application) <- str_to_lower(colnames(application))

# renaming columns by appending dataset initials to the end of each column in each dataset
application <- application %>% rename_with(~ paste0(., "_a"), everything())

# any duplicate rows in the application dataset?
# no duplicate rows, all distinct policy ID numbers
n_distinct(application$sk_id_curr_a)

# count of defaulters vs non-defaulters for categorical variables in a graph and table
combined <- function(data, variable){
  
  graph <- data %>%
    mutate(target_a = factor(target_a)) %>%
    group_by(!!sym(variable), target_a) %>%
    summarize(n = n(), .groups = "drop") %>% # suppress warning message
    mutate(!!sym(variable) := factor(!!sym(variable))) %>%
    ggplot(aes(x = !!sym(variable), y = n, fill = target_a)) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    labs(y = "Count", title = "Number of Loans - Repaid vs Defaulted") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  table <- data %>%
    group_by(!!sym(variable), target_a) %>%
    summarize(n = n(), .groups = "drop") %>%
    pivot_wider(., names_from = target_a, values_from = n) %>%
    mutate(default_rate = round(.[ , c(3)] / rowSums(.[ , c(2, 3)]), 2)) %>%
    arrange(default_rate$`1`)
  
  table <- tableGrob(table)
  
  combined <- grid.arrange(table, graph, ncol = 2)
}

# categorical variables: exploratory analysis
contract_type <- combined(application, "name_contract_type_a") # mostly cash loans, higher default rate w cash loans
gender <- combined(application, "code_gender_a") # women take out much more number of loans than men
# women have a lower default rate too
own_car <- combined(application, "flag_own_car_a") # most clients do not own a car
own_real_estate <- combined(application, "flag_own_realty_a") # most clients own a home
# interestingly, the default rate is about the same for those that own vs do not own a home (not intuitive)
num_of_children <- combined(application, "cnt_children_a") # those with no children have a lower default rate
name_type <- combined(application, "name_type_suite_a") # not very helpful visual, this variable can be dropped
income_type <- combined(application, "name_income_type_a")
education_type <- combined(application, "name_education_type_a")
family_status <- combined(application, "name_family_status_a")
housing_type <- combined(application, "name_housing_type_a") # not very helpful visual, most are categorized under
# House / apartment - this variable can be dropped
own_car_age <- combined(application, "own_car_age_a") # most entries are NAs, drop the var
mobile_flag <- combined(application, "flag_mobil_a") # most entries are in the 1 group, drop the var
organization_type <- combined(application, "organization_type_a") # drop, a lot of NAs
occupation_type <- combined(application, "occupation_type_a")
#days_employed <- combined(application, "days_employed_a")

retired_rows <- application[application$days_employed_a == 365243, ] #checking these odd values
table(retired_rows$name_income_type_a) #22 unemployed, 55352 pensioners

breaks <- c(0, 50001, 100001, 150001, 200001, 250001, 300001, Inf)
# Define labels for the income brackets
labels <- c("0-50k", "50k-100k", "100k-150k", "150k-200k", "200k-250k", "250k-300k", "300k+")
# Create a new column 'income_bracket' with income brackets
application <- application %>%
  mutate(income_bracket = cut(amt_income_total_a, 
                              breaks = breaks, labels = labels, include.lowest = TRUE),
         EMPLOYED_IN_YEARS = ifelse(days_employed_a == 365243, NA,
                                    round( -days_employed_a / 365.25,2)),
         AGE_IN_YEARS = floor(-days_birth_a / 365.25),
         AGE_BUCKET = cut(AGE_IN_YEARS, breaks = c(18, 26, 46, 65, Inf),
                          labels = c("18-25", "26-45", "46-64", "65+"), right = FALSE),
         MARRIED = case_when(name_family_status_a %in% c("Civil marriage","Married") ~ 1, 
                             TRUE ~ 0)
         )



table(application$occupation_type_a, application$income_bracket)
income_bracket <- combined(application, "income_bracket")

AGE_BUCKET <- combined(application, "AGE_BUCKET")
MARRIED <- combined(application, "MARRIED")
name_family_status <- combined(application, "name_family_status_a")

# results based on looking at the plots created for the categorical variables
# variables that can be dropped based on high correlation to other variables:


# variables that need min/max capping: amt_req_credit_bureau_year_a (cap at 9),
# amt_req_credit_bureau_qrt_a (cap at 4), amt_req_credit_bureau_mon_a (cap at 12),
# amt_req_credit_bureau_week_a (cap at 2), amt_req_credit_bureau_day_a (cap at 1),
# amt_req_credit_bureau_hour_a (cap at 1), def_60_cnt_social_circle_a (cap at 2),
# obs_60_cnt_social_circle_a (cap at 5), 

# variables to drop based on exploratory analysis
application_vars_drop <- c("flag_mobil_a")

x <- head(application)

# defaulters vs non-defaulters for continuous variables in a graph
combined_2 <- function(data, variable){
  
  graph <- data %>%
    filter(!is.na(!!sym(variable))) %>%
    mutate(target_a = factor(target_a)) %>%
    group_by(!!sym(variable), target_a) %>%
    summarize(n = n(), .groups = "drop") %>% # suppress warning message
    ggplot(aes(x = !!sym(variable), fill = target_a)) +
    geom_density(alpha = 0.5) +
    labs(y = "Density", title = "Number of Loans - Repaid vs Defaulted") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  table <- data %>%
    mutate(!!sym(variable) := if_else(is.na(!!sym(variable)), "missing", "not missing")) %>%
    group_by(!!sym(variable)) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(percentage_missing = round(n / sum(n), 2))
  
  table <- tableGrob(table)
  
  combined_2 <- grid.arrange(table, graph, nrow = 2)
}

# continuous variables: exploratory analysis
ext_source_1_a <- combined_2(application, "ext_source_1_a") # density function looks really different between the two!
ext_source_2_a <- combined_2(application, "ext_source_2_a") # looks different between the two
ext_source_3_a <- combined_2(application, "ext_source_3_a") # does not look different between the two 

days_employed <- combined_2(application, "EMPLOYED_IN_YEARS")
region_population_relative <- combined_2(application, "region_population_relative_a")
obs_30_cnt_social_circle <- combined_2(application, "obs_30_cnt_social_circle_a")
def_30_cnt_social_circle <- combined_2(application, "def_30_cnt_social_circle_a")
obs_60_cnt_social_circle <- combined_2(application, "obs_60_cnt_social_circle_a")
days_last_phone_change_a <- combined_2(application, "days_last_phone_change_a")


amt_req_credit_bureau_hour <- combined_2(application, "amt_req_credit_bureau_hour_a")
amt_req_credit_bureau_day_a <- combined_2(application, "amt_req_credit_bureau_day_a")
amt_req_credit_bureau_week_a <- combined_2(application, "amt_req_credit_bureau_week_a")
#amt_req_credit_bureau_qrt_a <- combined_2(application, "amt_req_credit_bureau_qrt_a") #this one looks weird, needs to be capped
amt_req_credit_bureau_year_a <- combined_2(application, "amt_req_credit_bureau_year_a")






######### binning, capping, and transforming variables #########
application <- application %>%
  mutate(cnt_children_capped_a = if_else(cnt_children_a >=5, "5+", as.factor(cnt_children_a)))