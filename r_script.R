# Install required packages if missing
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(readxl)) install.packages("readxl")
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(broom)) install.packages("broom")

# Load libraries
library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(broom)

# Step 1: Set working directory
setwd("/Users/ritachamiyan/Desktop/stat project")
#  Purpose: Sets the folder where the script will look for input files and save outputs.
# This avoids file-not-found errors.

# Step 2: Load Excel files
chronological <- read_excel("chronological_series.xlsx")
gini <- read_excel("Gini_coefficent.xlsx")
education <- read_excel("education_data.xlsx")
#  Purpose: Loads income data, GINI inequality data, and education enrollment data into R.

# Step 3: Clean column names
colnames(chronological) <- tolower(gsub(" ", "_", colnames(chronological)))
colnames(gini) <- tolower(gsub(" ", "_", colnames(gini)))
# Purpose: Makes sure column names are consistent and R-friendly.
# This prevents errors when merging and calling variables.

# Step 4: Merge datasets and remove missing values
merged <- left_join(chronological, gini, by = "year")
merged_clean <- na.omit(merged)
merged_edu_gini <- left_join(education, gini, by = "year") %>% na.omit()
#  Purpose: Combines datasets so we can study relationships.
# Removes missing rows to avoid errors in calculations.
#  Conclusion: We now have two clean datasets ready for analysis — income vs. inequality, and education vs. inequality.

# Step 5: Explore merged data
print(head(merged_clean))
summary(merged_clean)
print(head(merged_edu_gini))
summary(merged_edu_gini)
#  Purpose: Gives a first look at the data and basic statistics.
#  Conclusion: We can see the range of values, spot-check for odd numbers, and confirm that the merge worked.

# Step 6: Summary statistics
summary_stats <- merged_clean %>%
  summarise(
    mean_absolute = mean(absolute_value),
    sd_absolute = sd(absolute_value),
    min_absolute = min(absolute_value),
    max_absolute = max(absolute_value),
    mean_gini = mean(gini_coefficent_score),
    sd_gini = sd(gini_coefficent_score),
    min_gini = min(gini_coefficent_score),
    max_gini = max(gini_coefficent_score)
  )
print(summary_stats)
#  Purpose: Calculates the average, spread, minimum, and maximum of income and inequality.
#  Conclusion: This helps understand the scale of values — for example, GINI tends to stay between ~27–37, and income has wide variability.

# Step 6b: Boxplot for absolute income
ggplot(merged_clean, aes(y = absolute_value)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Absolute Value", y = "Absolute Value", x = "") +
  theme_minimal()
#  Purpose: Shows the median, variability, and outliers for income.
#  Conclusion: We can visually check if some years have extreme income or if the data is tightly clustered.

# Step 6c: Boxplot for GINI
ggplot(merged_clean, aes(y = gini_coefficent_score)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Boxplot of GINI Coefficient", y = "GINI Coefficient", x = "") +
  theme_minimal()
#  Purpose: Shows the spread and central tendency of inequality.
#  Conclusion: The GINI coefficient is fairly stable across years, without major outliers.

# Step 7: Histograms
ggplot(merged_clean, aes(x = absolute_value)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Absolute Value", x = "Absolute Value", y = "Count")

ggplot(merged_clean, aes(x = gini_coefficent_score)) +
  geom_histogram(bins = 10, fill = "pink", color = "black") +
  labs(title = "Histogram of GINI Coefficient", x = "GINI Coefficient", y = "Count")
#  Purpose: Displays how often different income and GINI levels appear.
#  Conclusion: This helps detect skewness, clustering, or unusual data shapes.

# Step 8: Time trends
ggplot(merged_clean, aes(x = year)) +
  geom_line(aes(y = absolute_value, color = "Absolute Value")) +
  geom_line(aes(y = gini_coefficent_score * 1000, color = "GINI Coefficient (scaled)")) +
  scale_y_continuous(name = "Absolute Value", sec.axis = sec_axis(~./1000, name = "GINI Coefficient")) +
  labs(title = "Trends Over Time", x = "Year") +
  scale_color_manual(values = c("skyblue", "pink")) +
  theme_minimal()
#  Purpose: Shows how income and inequality change year by year.
#  Conclusion: Income generally increases; GINI fluctuates. This suggests they don’t move in sync.

# Step 9: Correlation (income vs GINI)
cor_test <- cor.test(merged_clean$absolute_value, merged_clean$gini_coefficent_score)
print(cor_test)
#  Purpose: Measures the strength and direction of the relationship between income and inequality.
#  Conclusion: The correlation is weak and not significant → income levels alone do not predict inequality.

# Step 10: Linear regression (income predicts GINI)
model <- lm(gini_coefficent_score ~ absolute_value, data = merged_clean)
summary_model <- summary(model)
print(summary_model)
#  Purpose: Tests whether income can statistically explain changes in inequality.
#  Conclusion: The slope is near zero, p-value is high → income does not significantly explain GINI changes.

# Step 11: Regression plot
ggplot(merged_clean, aes(x = absolute_value, y = gini_coefficent_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "purple") +
  labs(title = "Regression: Absolute Value vs GINI Coefficient",
       x = "Absolute Value", y = "GINI Coefficient") +
  theme_minimal()
#  Purpose: Visualizes the relationship and regression fit.
#  Conclusion: The flat line shows no meaningful relationship between the variables.

# Step 12: Correlation of education vs GINI
correlation_secondary <- cor.test(merged_edu_gini$secondary_total, merged_edu_gini$gini_coefficent_score)
correlation_bachelor <- cor.test(merged_edu_gini$bachelor_total, merged_edu_gini$gini_coefficent_score)
correlation_master <- cor.test(merged_edu_gini$master_total, merged_edu_gini$gini_coefficent_score)
print(correlation_secondary)
print(correlation_bachelor)
print(correlation_master)
#  Purpose: Tests if enrollment rates (secondary, bachelor, master) are related to inequality.
#  Conclusion:
#   - Secondary: negative but not significant.
#   - Bachelor: positive but not significant.
#   - Master: positive, almost significant → more data needed.

# Step 13: Regression (education → GINI)
model <- lm(gini_coefficent_score ~ secondary_total + bachelor_total + master_total, data = merged_edu_gini)
summary_model <- summary(model)
print(summary_model)
#  Purpose: Checks which education levels (combined) explain inequality.
#  Conclusion: No level is a strong predictor, likely due to small sample size (only 3 years).

# Step 14: Scatterplots of education vs GINI
ggplot(merged_edu_gini, aes(x = secondary_total, y = gini_coefficent_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Secondary Enrollment vs GINI", x = "Secondary Enrollment (%)", y = "GINI Coefficient")

ggplot(merged_edu_gini, aes(x = bachelor_total, y = gini_coefficent_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Bachelor Enrollment vs GINI", x = "Bachelor Enrollment (%)", y = "GINI Coefficient")

ggplot(merged_edu_gini, aes(x = master_total, y = gini_coefficent_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Master Enrollment vs GINI", x = "Master Enrollment (%)", y = "GINI Coefficient")
#  Purpose: Visualizes the strength and direction of relationships.
#  Conclusion:
#   - Secondary: slight negative slope.
#   - Bachelor & Master: surprising positive slopes, but probably unreliable due to small dataset.
