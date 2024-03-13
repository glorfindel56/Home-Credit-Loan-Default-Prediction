library(tidyverse)

# reading in the datasets from Home Credit
application <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/application_data.csv", 
                        col_names = TRUE)

head(application)