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

# variables containing information about where a client lives




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

######### finding outliers #########
# using box-and-whisker plots for visualization






######### conducting exploratory data analysis #########




######### binning and cleaning variables #########




######### creating new variables from existing variables #########





