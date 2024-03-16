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

# renaming columns by appending dataset initials to the end of each column in each dataset
application <- application %>% rename_with(~ paste0(., "_a"), everything())
bureau <- bureau %>% rename_with(~ paste0(., "_b"), everything())
bureau_balance <- bureau_balance %>% rename_with(~ paste0(., "_bb"), everything())
credit_card_balance <- credit_card_balance %>% rename_with(~ paste0(., "_cbb"), everything())
installments_payments <- installments_payments %>% rename_with(~ paste0(., "_ip"), everything())
pos_cash_balance <- pos_cash_balance %>% rename_with(~ paste0(., "_pcb"), everything())
previous_application <- previous_application %>% rename_with(~ paste0(., "_pa"), everything())

############## combining Home Credit data on one policy number ##############
# does not include bureau data
# policy number = 100002

# joining application and previous_application
combined_small <- application %>%
  filter(sk_id_curr_a == 100002) %>%
  left_join(., previous_application, by = c("sk_id_curr_a" = "sk_id_curr_pa")) 

# joining installments_payments to combined_small
# there is a 1-to-N relationship between combined_small and installments_payments
# ex: sk_id_curr_a == 100002 has a 1-to-19 relationship (19 records in installments_payments)
# grabbing the latest information from installments_payments and merging it onto combined_small
# this is the best gauge on the customer's recent financial payments history
# also results in a 1-to-1 relationship between combined_small and installments_payments
i <- installments_payments %>% filter(sk_id_curr_ip == 100002)

installments_payments_2 <- installments_payments %>%
                           group_by(sk_id_curr_ip, sk_id_prev_ip) %>%
                           filter(num_instalment_version_ip == max(num_instalment_version_ip),
                                  num_instalment_number_ip == max(num_instalment_number_ip), 
                                  days_instalment_ip == max(days_instalment_ip),
                                  days_entry_payment_ip == max(days_entry_payment_ip),
                                  amt_payment_ip == max(amt_payment_ip))

combined_small_2 <- combined_small %>%
  left_join(., installments_payments_2, by = c("sk_id_prev_pa" = "sk_id_prev_ip"))

# joining pos_cash_balance to combined_small_2
# there is also a 1-to-N relationship between combined_small_2 and pos_cash_balance
# going to follow the same logic as above to get a 1-to-1 relationship
p <- pos_cash_balance %>% 
  filter(sk_id_curr_pcb == 100002)

pos_cash_balance_2 <- pos_cash_balance %>%
                      group_by(sk_id_curr_pcb, sk_id_prev_pcb) %>%
                      filter(months_balance_pcb == max(months_balance_pcb))

combined_small_3 <- combined_small_2 %>%
  left_join(., pos_cash_balance_2, by = c("sk_id_prev_pa" = "sk_id_prev_pcb"))

# joining credit_card to combined_small_3
# going to follow the same logic as above to get a 1-to-1 relationship
cc <- credit_card_balance %>%
  filter(sk_id_curr_cbb == 100002)

credit_card_balance_2 <- credit_card_balance %>%
  group_by(sk_id_curr_cbb, sk_id_prev_cbb) %>%
  filter(months_balance_cbb == max(months_balance_cbb))

combined_small_4 <- combined_small_3 %>%
  left_join(., credit_card_balance, by = c("sk_id_prev_pa" = "sk_id_prev_cbb"))


############## combining Home Credit data on ALL policy numbers ##############
combined <- application %>%
  left_join(., previous_application, by = c("sk_id_curr_a" = "sk_id_curr_pa")) %>%
  arrange(sk_id_curr_a)

combined_2 <- combined %>%
  left_join(., installments_payments_2, by = c("sk_id_prev_pa" = "sk_id_prev_ip")) %>%
  arrange(sk_id_curr_a)

combined_3 <- combined_2 %>%
  left_join(., pos_cash_balance_2, by = c("sk_id_prev_pa" = "sk_id_prev_pcb"))

combined_4 <- combined_3 %>%
  left_join(., credit_card_balance_2, by = c("sk_id_prev_pa" = "sk_id_prev_cbb"))

# debug why combined_2 doesn't have the same amount of rows as combined
# need to do data manipuldation on installments_payments
a <- combined %>% filter(sk_id_curr_a == 100213)
b <- combined_2 %>% filter(sk_id_curr_a == 100213) %>% distinct()
c <- installments_payments_2 %>% filter(sk_id_curr_ip == 100213)

d <- combined %>% group_by(sk_id_curr_a) %>% summarize(count = n())
e <- combined_2 %>% group_by(sk_id_curr_a) %>% summarize(count = n())
f <- d %>%
  left_join(., e, by = c("sk_id_curr_a"), suffix = c("c","c2")) %>%
  mutate(diff = countc2 - countc)

# debug why combined_4 doesn't have the same amount of rows as combined_3
# need to do data manipulation on credit_card_balance
a <- combined_3 %>% filter(sk_id_curr_a == 317063)
b <- combined_4 %>% filter(sk_id_curr_a == 317063)
c <- credit_card_balance %>% filter(sk_id_curr_cbb == 317063)

d <- combined_3 %>% group_by(sk_id_curr_a) %>% summarize(count = n())
e <- combined_4 %>% group_by(sk_id_curr_a) %>% summarize(count = n())
f <- d %>%
  left_join(., e, by = c("sk_id_curr_a"), suffix = c("c","c2")) %>%
  mutate(diff = countc2 - countc)

# going to forgo merging bureau and bureau_balance to the merged Home Credit data set
# bureau_balance doesn't offer any new information - already contained within bureau
# bureau contains information about application data from previous loans that clients applied for at other
# financial institutions
# however, this data only shows when an applicant got approved for a loan at another financial institution
# there is no information about why or whether an applicant got rejected for a loan application elsewhere
# there is also no information about whether the applicant defaulted on a loan from another financial institution
# combined_4 also contains information on bureau data, which is why the bureau dataset doesn't contain
# a lot of predictive power

# saving out the combined Home Credit dataset
combined_4_data <- saveRDS(combined_4, file = "/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/combined_data.rds")
combined_4_data_csv <- write_csv(combined_4, file = "/Users/joycehu/Library/CloudStorage/Box-Box/MGT 6203/combined_data.csv")
