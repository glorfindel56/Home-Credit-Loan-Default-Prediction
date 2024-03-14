library(tidyverse)

# reading in the datasets from Home Credit
application <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/application_data.csv", 
                        col_names = TRUE)

bureau <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/bureau.csv", 
                   col_names = TRUE)

bureau_balance <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/bureau_balance.csv",
                           col_names = TRUE)

credit_card_balance <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/credit_card_balance.csv",
                                col_names = TRUE)

installments_payments <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/installments_payments.csv",
                                  col_names = TRUE)

pos_cash_balance <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/POS_CASH_balance.csv",
                             col_names = TRUE)

previous_application <- read_csv("/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/previous_application.csv",
                                 col_names = TRUE)

# converting column names to lower case for each dataset
colnames(application) <- str_to_lower(colnames(application))
colnames(bureau) <- str_to_lower(colnames(bureau))
colnames(bureau_balance) <- str_to_lower(colnames(bureau_balance))
colnames(credit_card_balance) <- str_to_lower(colnames(credit_card_balance))
colnames(installments_payments) <- str_to_lower(colnames(installments_payments))
colnames(pos_cash_balance) <- str_to_lower(colnames(pos_cash_balance))
colnames(previous_application) <- str_to_lower(colnames(previous_application))

# combining into one large dataset using common foreign keys on a few policies
head(application)

# viewing the data to do a simple sanity check
head(application)

str_to_lower(colnames(application))
