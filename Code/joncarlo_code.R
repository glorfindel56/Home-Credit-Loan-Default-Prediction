.libPaths("C:\\Users\\jonca\\Desktop\\School\\R")
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
#install.packages("psych")
#install.packages("psychTools")
library(psychTools)
library(psych)
set.seed(5678) #lucky seed-could be anything really
options(scipen=999) #turn off pesky scientific notation
options(max.print = 10000) #allow for more options to print
application_data <- read.csv("C:\\Users\\jonca\\Box\\MGT 6203\\application_data.csv") #307,511 obs and 122 vars

head(application_data) #presence of some NA in data
str(application_data) #determine data-type of each variable
#many binary indicator flag variaboes 0/1

describe(application_data, na.rm=TRUE) #get a feel for data ranges/averages

missing_percent<-colMeans(is.na(application_data))*100 #display % missing in each col
missing_percent



#too many variables to plot all missingness on one graph
#plot first 45 variables missingness
#dev.off()
par(mar = c(10, 5, 4, 2))
plot(missing_percent[1:45], type="o", pch=19,
     xlab="Variable Name",
     ylab="Percent Missingness",
     main="Percent Missing in First 45 Variables",
     col="blue",
     xlim=c(1,length(missing_percent[1:45])),
    ylim=c(0,100))
axis(1,at=1:length(missing_percent[1:45]),labels=names(missing_percent[1:45]),
     las=3,cex.axis=0.5)
abline(h=40,col="red",lty=2)
#plot variables 46-90 missingness
plot(missing_percent[46:90], type="o", pch=19,
     xlab="Variable Name",
     ylab="Percent Missingness",
     main="Percent Missing in Variables 46-90",
     col="blue",
     xlim=c(1,length(missing_percent[46:90])),
     ylim=c(0,100))
axis(1,at=1:length(missing_percent[46:90]),labels=names(missing_percent[46:90]),
     las=3,cex.axis=0.5)
abline(h=40,col="red",lty=2)
#plot variable 91 onwards missingness
plot(missing_percent[91:122], type="o", pch=19,
     xlab="Variable Name",
     ylab="Percent Missingness",
     main="Percent Missing in Variables 91 onwards",
     col="blue",
     xlim=c(1,length(missing_percent[91:122])),
     ylim=c(0,100))
axis(1,at=1:length(missing_percent[91:122]),labels=names(missing_percent[91:122]),
     las=3,cex.axis=0.5)
abline(h=40,col="red",lty=2)
#list variables missing over 40%, can also be seen in above graphs
missing_40percent<-names(missing_percent[missing_percent>=40])
missing_40percent

#capture the binary indicator variables starting with "FLAG"
flag_list<-grep("FLAG",names(application_data),value=TRUE)
flag_plus_target<-c("TARGET", flag_list)

#Using tetrachoric correlation as TARGET and all "FLAG" variables are binary indicators
flag_corr<-tetrachoric(na.omit(application_data[flag_plus_target]), na.rm=TRUE)
flag_corr
#every "FLAG" variable has a weak tetrachoric correlation value, very unlikely to be useful for modeling

droplist<-c(missing_40percent,flag_list) #drop vars with over 40% missingness and all FLAG vars
droplist<-names(application_data) %in% droplist
application_trimmed<-application_data[!droplist]
#the above code is time consuming to rerun, will save this file in box for now
saveRDS(application_trimmed, "C:\\Users\\jonca\\Box\\MGT 6203\\application_trimmed.rds")
write.csv(application_trimmed, "C:\\Users\\jonca\\Box\\MGT 6203\\application_trimmed.csv")
#time for some preliminary data analysis to get a better feel for our data
df<-application_trimmed
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
  mutate(HAS_CHILDREN = ifelse(CNT_CHILDREN > 0, 1, 0)) %>%
  mutate(EDUCATION_LEVEL = case_when(
    NAME_EDUCATION_TYPE %in% c("Higher education", "Academic degree") ~ "college_graduate",
    TRUE ~ "highschool graduate")) %>%
  mutate(AGE_IN_YEARS = floor(-DAYS_BIRTH / 365.25)) %>%
  mutate(AGE_BUCKET = cut(AGE_IN_YEARS, breaks = c(18, 26, 46, 65, Inf),
                          labels = c("18-25", "26-45", "46-64", "65+"),
                          right = FALSE)) %>%
  mutate(MARRIED = case_when(NAME_FAMILY_STATUS %in% c("Civil marriage","Married") ~ 1, TRUE ~ 0))

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

sum(is.na(df$DAYS_EMPLOYED))

sum(is.na(df$AMT_INCOME_TOTAL))
