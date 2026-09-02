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

setwd("/Users/ritachamiyan/Desktop")

# Load Excel files
chronological <- read_excel("chronological_series.xlsx")
gini <- read_excel("Gini_coefficent.xlsx")


# Step 2: Clean column names
colnames(chronological) <- tolower(gsub(" ", "_", colnames(chronological)))
colnames(gini) <- tolower(gsub(" ", "_", colnames(gini)))

# Step 3: Merge datasets on 'year'
merged <- left_join(chronological, gini, by = "year")
merged_clean <- na.omit(merged)

# Step 4: Explore the merged data
print(head(merged_clean))
summary(merged_clean)

# Step 5: Plot the trends
ggplot(merged_clean, aes(x = year)) +
  geom_line(aes(y = absolute_value, color = "Absolute Value")) +
  geom_line(aes(y = gini_coefficent_score * 1000, color = "GINI Coefficient (scaled)")) +  # scale GINI for visibility
  scale_y_continuous(
    name = "Absolute Value",
    sec.axis = sec_axis(~./1000, name = "GINI Coefficient")
  ) +
  labs(title = "Trends: Absolute Value vs GINI Coefficient", x = "Year") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# Step 6: Calculate correlation
cor_result <- cor(merged_clean$absolute_value, merged_clean$gini_coefficent_score, use = "complete.obs")
print(paste("Correlation between absolute value and GINI:", round(cor_result, 3)))

# Step 7: Run linear regression
model <- lm(gini_coefficent_score ~ absolute_value, data = merged_clean)
summary(model)

# Step 8: Plot regression
ggplot(merged_clean, aes(x = absolute_value, y = gini_coefficent_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Regression: Absolute Value vs GINI Coefficient",
       x = "Absolute Value",
       y = "GINI Coefficient") +
  theme_minimal()

# Step 9: Show regression summary table
tidy_model <- tidy(model)
print(tidy_model)
