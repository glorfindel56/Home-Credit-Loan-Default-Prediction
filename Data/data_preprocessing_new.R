library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(MASS)

data <- read_csv('/Users/calvinng/Library/CloudStorage/Box-Box/MGT 6203/application_imputed_cleaner.csv')
print(names(data))
print(head(data))
######### categorizing independent variables into categorical or continuous #########
# based on column descriptions given by Home Credit
# also based on intuition and studying the contents of the variable
# ex: zip code is a categorical variable even though it looks like a continuous variable
# the underlying packages in tidyverse might interpret it as continuous based on the first x entries
# up to modelers to inspect the data and ensure that the data is of the correct data type

# Function to categorize variables
categorize_variables <- function(data_path) {
  # Load the dataset
  data <- read_csv(data_path)
  
  # Initialize lists to store the names of categorical and continuous variables
  categorical_vars <- c()
  continuous_vars <- c()
  
  # Loop through each column to categorize it
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    
    # Determine if the column is numeric or character
    if (is.numeric(col_data)) {
      # If numeric, use heuristic to determine category
      if (length(unique(col_data)) < 10) {
        categorical_vars <- c(categorical_vars, col_name)
      } else {
        continuous_vars <- c(continuous_vars, col_name)
      }
    } else if (is.character(col_data)) {
      # Character columns are considered categorical
      categorical_vars <- c(categorical_vars, col_name)
    }
  }
  
  list(categorical = categorical_vars, continuous = continuous_vars)
}

# Adjust the path to your dataset here
data_path <- "/Users/calvinng/Library/CloudStorage/Box-Box/MGT 6203/application_imputed_cleaner.csv"
result <- categorize_variables(data_path)

# Now you can safely print and save the results
print(result$categorical)
print(result$continuous)

# Save the results to files as intended
# Note: Ensure the directory for saving files is writable and correct the path as needed
writeLines(c("Categorical Variables:", paste(result$categorical, collapse = ", ")), "categorical_variables.txt")
writeLines(c("Continuous Variables:", paste(result$continuous, collapse = ", ")), "continuous_variables.txt")
writeLines(c("Categorical Variables:", paste(result$categorical, collapse = ", "),
             "", "Continuous Variables:", paste(result$continuous, collapse = ", ")),
           "variable_categorization.txt")


cat_result <- result$categorical
con_result <- result$continuous

print(cat_result)
print(con_result)


######### finding variables that are highly correlated w each other #########
# categorical-categorical: finding if there are any correlations between the categorical independent variables
# categorical-continuous: correlations between the categorical and continuous independent variables
# continuous-continuous: correlations between the continuous independent variables
# categorical-target (dependent variable)
# continuous-target (dependent variable)

#Continuous-Continuous
# Assuming `data` is your dataset and `con_result` contains the continuous variables
# Ensure to exclude any placeholders or non-existent columns
con_result_cleaned <- con_result[con_result != "...1"]

# Calculate the correlation matrix
cor_matrix <- cor(data[, con_result_cleaned, drop = FALSE], use = "pairwise.complete.obs")

# Melt the correlation matrix for ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Plot the heatmap
ggplot(data = melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +
  coord_fixed() +
  labs(x = "", y = "", title = "Correlation for Continuous variables")


# Sorting the melted_cor_matrix in descending order by the correlation value
melted_cor_matrix_sorted <- melted_cor_matrix[order(-melted_cor_matrix$value), ]
print(melted_cor_matrix_sorted)
write.table(melted_cor_matrix_sorted, file = "sorted_melted_cor_matrix.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# categorical-categorical

# Function to calculate Cramer's V, based on chi-squared statistic
# Ensure the categorical variables are factors
data[cat_result] <- lapply(data[cat_result], factor)
# Function to calculate Cramer's V, based on chi-squared statistic
cramers_v <- function(chi_squared, n, min_dim) {
  phi_squared <- chi_squared / n
  return(sqrt(phi_squared / min(min_dim - 1, min_dim - 1)))
}

# Ensure the categorical variables are factors
data[cat_result] <- lapply(data[cat_result], factor)

# Initialize an empty list to store Cramér's V results
cramers_v_results <- list()

# Perform chi-square tests and calculate Cramér's V for all pairs of categorical variables
for(i in 1:length(cat_result)) {
  for(j in (i + 1):length(cat_result)) {
    var1 <- cat_result[i]
    var2 <- cat_result[j]
    
    # Perform the chi-square test
    test_result <- tryCatch(
      chisq.test(data[[var1]], data[[var2]]),
      error = function(e) {
        return(NULL) # Return NULL if the test fails (e.g., due to all values being the same)
      }
    )
    
    # Proceed if test_result is not NULL
    if (!is.null(test_result)) {
      # Calculate Cramer's V
      n <- sum(test_result$observed) # Total observations
      min_dim <- min(nrow(test_result$observed), ncol(test_result$observed)) # Smaller dimension of the table
      cv <- cramers_v(test_result$statistic, n, min_dim)
      
      # Store the result with variable names and Cramer's V value
      cramers_v_results[[paste(var1, "vs", var2)]] <- cv
    }
  }
}
results_text <- sapply(names(cramers_v_results), function(name) {
  sprintf("%s: Cramér's V = %f", name, cramers_v_results[[name]])
})

writeLines(results_text, "categorical_categorical.txt")
print(cramers_v_results)

# Assuming cramers_v_results is a named list with Cramer's V values
# Convert the named list to a data frame for sorting
cramers_v_df <- do.call(rbind, lapply(names(cramers_v_results), function(x) {
  data.frame(VarPair = x, CramersV = cramers_v_results[[x]], stringsAsFactors = FALSE)
}))
# Sort in descending order
cramers_v_df <- cramers_v_df[order(-cramers_v_df$CramersV),]
print(cramers_v_df)
writeLines(paste(cramers_v_df$VarPair, ": Cramér's V =", cramers_v_df$CramersV), "sorted_categorical_categorical.txt")

# categorical-continuous: correlations between the categorical and continuous independent variables
data[cat_result] <- lapply(data[cat_result], as.factor)
anova_results <- list()
#p-value threshold (0.05) is set to determine significance
for(cat_var in cat_result) {
  for(con_var in con_result_cleaned) {
    # Perform ANOVA
    anova_test <- aov(as.formula(paste(con_var, "~", cat_var)), data = data)
    # Extract the summary
    anova_summary <- summary(anova_test)
    # Get the p-value of the ANOVA test
    p_value <- anova_summary[[1]][["Pr(>F)"]][1]
    anova_results[[paste(cat_var, "vs", con_var)]] <- p_value
  }
}

print(anova_results)
results_df <- data.frame(Variable_Pair = names(anova_results),P_Value = unlist(anova_results))
results_df <- results_df[order(-results_df$P_Value),]
write.table(results_df, "anova_results_sorted.txt", row.names = FALSE, sep = "\t", quote = FALSE)


# Working on the high correlation variable
cor_matrix <- read.csv("/Users/calvinng/Desktop/GeoTech/MGT6203/Project/Team-89/sorted_melted_cor_matrix.txt", sep = "\t")

# Filter for rows where the correlation value is greater than 0.5 and less than 1.0
high_cor_relations <- subset(cor_matrix, value > 0.5 & value < 1.0)

# Extract the unique variable names involved in these high correlations
high_cor_var_names <- unique(c(high_cor_relations$Var1, high_cor_relations$Var2))

# Print the variable names
print(high_cor_var_names)


# Anything Higher than 0.5
# Select only the variables with high correlations for PCA
data_selected <- data[, c("OBS_60_CNT_SOCIAL_CIRCLE", "OBS_30_CNT_SOCIAL_CIRCLE", 
                          "AMT_GOODS_PRICE", "AMT_CREDIT", "EMPLOYED_IN_YEARS", 
                          "DAYS_EMPLOYED", "AMT_ANNUITY", "AGE_IN_YEARS", 
                          "AGE_IN_YEARS_NUM", "CREDIT_TO_ANNUITY_RATIO", 
                          "CREDIT_TO_INCOME_RATIO", "EXT_SOURCE_1")]

# Standardize the data: PCA is affected by scale so you need to scale the features in your data before applying PCA.
data_scaled <- scale(data_selected)

# Perform PCA
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

print(summary(pca_result))

# Plot the variance explained by each principal component
# Removed 'xlab' and 'ylab' parameters to avoid the error
plot(pca_result, type = "l", main = "Ebow Plot")

# PC1 explains 31.84% of the variance, PC2 explains an additional 28.57%, and so on. 
# PC1 and PC2 are almost 50%

# By PC3, captured 77.05% of the variance, and by PC9, you're at 99.869%
# ,indicating that most information in the data can be represented by the first nine components.
# he first few components (e.g., PC1 to PC9) capture the majority of the variance in the dataset, 
# which suggests you could potentially reduce the dimensionality of your data without losing significant information. 
# This can simplify further analyses and reduce computational costs.

# Variable Selection


# Include only the selected variables in the model
selected_vars <- c("OBS_60_CNT_SOCIAL_CIRCLE", "OBS_30_CNT_SOCIAL_CIRCLE", 
                   "AMT_GOODS_PRICE", "AMT_CREDIT", "EMPLOYED_IN_YEARS", 
                   "DAYS_EMPLOYED", "AMT_ANNUITY", "AGE_IN_YEARS", 
                   "AGE_IN_YEARS_NUM", "CREDIT_TO_ANNUITY_RATIO", 
                   "CREDIT_TO_INCOME_RATIO", "EXT_SOURCE_1")

formula_selected <- as.formula(paste("TARGET ~", paste(selected_vars, collapse = " + ")))

# Setup initial models for stepwise selection
full_model_selected <- glm(formula_selected, data = data, family = binomial)
null_model_selected <- glm(TARGET ~ 1, data = data, family = binomial)

# Perform stepwise regression
model_backward_selected <- stepAIC(full_model_selected, direction = "backward")
model_forward_selected <- stepAIC(null_model_selected, direction = "forward", scope = list(lower = null_model_selected, upper = full_model_selected))
model_both_selected <- stepAIC(null_model_selected, direction = "both", scope = list(lower = null_model_selected, upper = full_model_selected))

# Review the selected models
print("Backward")
summary(model_backward_selected)
print("Forward")
summary(model_forward_selected)
print("Stepwise")
summary(model_both_selected)

# good eff: OBS_30_CNT_SOCIAL_CIRCLE, AMT_CREDIT 
# bad eff: EXT_SOURCE_1, CREDIT_TO_ANNUITY_RATIO

library(caret)
library(pROC)


# Assuming 'data' is your dataset and 'model' is your fitted model
# Split data into training and test sets
set.seed(123) # for reproducibility
# Select only the variables with high correlations for PCA
data_selected <- data[, c("OBS_60_CNT_SOCIAL_CIRCLE", "OBS_30_CNT_SOCIAL_CIRCLE", 
                          "AMT_GOODS_PRICE", "AMT_CREDIT", "EMPLOYED_IN_YEARS", 
                          "DAYS_EMPLOYED", "AMT_ANNUITY", "AGE_IN_YEARS", 
                          "AGE_IN_YEARS_NUM", "CREDIT_TO_ANNUITY_RATIO", 
                          "CREDIT_TO_INCOME_RATIO", "EXT_SOURCE_1", "TARGET")]
index <- createDataPartition(data_selected$TARGET, p = 0.80, list = FALSE)
train_data <- data_selected[index, ]
test_data <- data_selected[-index, ]

# Fit model on training data
fit <- glm(TARGET ~ ., family = binomial, data = train_data)

# Predict on test data
predictions <- predict(fit, test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- confusionMatrix(factor(predicted_class), factor(test_data$TARGET))

# ROC Curve and AUC
roc_result <- roc(test_data$TARGET, predictions)
plot(roc_result, main = "ROC Curve")
auc(roc_result)

# Print out the confusion matrix and other metrics
print(conf_matrix)

