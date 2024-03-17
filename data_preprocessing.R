library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape2)

data <- read_csv('/Users/calvinng/Library/CloudStorage/Box-Box/MGT 6203/application_trimmed_kai_30_cols.csv')
print(names(data))
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
data_path <- "/Users/calvinng/Library/CloudStorage/Box-Box/MGT 6203/application_trimmed_kai_30_cols.csv"
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



