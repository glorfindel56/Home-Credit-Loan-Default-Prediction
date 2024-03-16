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

# count of defaulters vs non-defaulters for categorical variables in a graph and table
combined <- function(data, variable){
  variable <- enquo(variable)
  
  graph <- data %>%
    mutate(target_a = factor(target_a)) %>%
    group_by(!!variable, target_a) %>%
    summarize(n = n(), .groups = "drop") %>% # suppress warning message
    ggplot(aes(x = !!variable, y = n, fill = target_a)) +
    geom_bar(stat = "identity") +
    labs(y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  table <- data %>%
    group_by(!!variable, target_a) %>%
    summarize(n = n(), .groups = "drop") %>%
    pivot_wider(., names_from = target_a, values_from = n) %>%
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
    mutate(default_rate = round(.[ , c(3)] / rowSums(.[ , c(2, 3)]), 2))
  
  table <- tableGrob(table)
  
  combined <- grid.arrange(table, graph, nrow = 2)
}


# categorical variables: exploratory analysis
contract_type <- combined(application, name_contract_type_a) # mostly cash loans, higher default rate w cash loans
gender <- combined(application, code_gender_a) # women take out much more number of loans than men
# women have a lower default rate too
own_car <- combined(application, flag_own_car_a) # most clients do not own a car
own_real_estate <- combined(application, flag_own_realty_a) # most clients own a home
# interestingly, those that own real estate have a higher default rate

x <- head(application)


# graph <- function(variable){
#   home_credit_2 %>%
#     mutate(variable = cut(variable, breaks = 10),
#            target_a = factor(target_a)) %>%
#     group_by(paste0(variable, "_group"), target_a) %>%
#     summarize(n = n(), .groups = "drop") %>% # suppress warning message
#     ggplot(aes(x = paste0(variable, "_group"), y = n, fill = target_a)) +
#     geom_bar(stat = "identity") +
#     labs(title = "Bar Plot", y = "Count") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# }




######### binning and cleaning variables #########




######### creating new variables from existing variables #########





