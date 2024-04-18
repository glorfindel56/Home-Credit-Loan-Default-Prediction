library(dplyr)
library(caret)
library(kknn)
library(tidyverse)
library(tidyr)
library(kernlab)
library(ggplot2)
library(reshape2)


application_data <- read.csv(file = "/Users/kfung/Library/CloudStorage/Box-Box/MGT 6203/application_data.csv")
application_trimmed <- read.csv(file = "/Users/kfung/Library/CloudStorage/Box-Box/MGT 6203/application_trimmed.csv")
table(application_trimmed$CNT_CHILDREN,application_trimmed$NAME_FAMILY_STATUS)


application_trimmed <- application_trimmed %>%
  mutate(HAS_CHILDREN = ifelse(CNT_CHILDREN > 0, TRUE, FALSE))

unique(application_trimmed$NAME_EDUCATION_TYPE)

application_trimmed <- application_trimmed %>%
  mutate(EDUCATION_LEVEL = case_when(
    NAME_EDUCATION_TYPE %in% c("Higher education", "Academic degree") ~ "college_graduate",
    TRUE ~ "highschool graduate"
  ))