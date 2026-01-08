library(tidyverse)
superstore <- read.csv("C:\\Users\\ramya\\OneDrive\\Documents\\new_bootcamp\\Week 8\\Day 2\\lab-r-dataframes\\dataset\\Sample - Superstore.csv")
superstore

sales_vector <- superstore["Sales"]
head(sales_vector)

subset_data <- superstore[1:15 , c("Order.ID", "Customer.Name", "Sales")]
subset_data


num_rows <- nrow(superstore)
num_rows


num_cols <- ncol(superstore)
num_cols

# Step 4

# Filter Profit > 100
profit_over_100 <- subset(superstore, Profit > 100)
profit_over_100
# Filter Category = "Furniture" & Sales > 500
furniture_high_sales_1 <- subset(superstore, Category == "Furniture" & Sales > 500)
furniture_high_sales_1
furniture_high_sales_2 <- superstore %>% filter(Category == "Furniture" & Sales > 500)
furniture_high_sales_2
# Filter Region = "West" & Quantity > 5
west_large_quantity <- subset(superstore, Region == "West" & Quantity > 5)
west_large_quantity

# Step 5

# Add Profit Margin column
superstore$Profit.Margin <- (superstore$Profit / superstore$Sales) * 100
superstore
superstore <- superstore %>% mutate(Profit.Margin = (Profit  * 100)/ Sales)
superstore

# Round Sales to 2 decimal places
superstore$Sales <- round(superstore$Sales, 2)
superstore
# Remove Postal Code column
superstore <- subset(superstore, select = -Postal.Code)
# OR using dplyr:
# library(dplyr)
# superstore <- select(superstore, -Postal.Code)

# Step 6

# Check for missing values
sum(is.na(superstore))  # TRUE if any missing value

# Remove rows with missing data
superstore_clean <- na.omit(superstore)

# Replace missing values in Sales with mean
# install.packages("zoo")  # if not installed
library(zoo)
superstore$Sales <- na.fill(superstore$Sales, mean(superstore$Sales, na.rm = TRUE))
superstore$Sales
# Step 7

# Load dplyr for easier data manipulation
library(dplyr)

# Group by Region and calculate total Sales and Profit
region_summary <- superstore %>%
  group_by(Region) %>%
  summarise(Total_Sales = sum(Sales),
            Total_Profit = sum(Profit))
region_summary

# Create Discount Level column
superstore <- superstore %>%
  mutate(Discount.Level = case_when(
    Discount >= 0 & Discount <= 0.2 ~ "Low",
    Discount > 0.2 & Discount <= 0.5 ~ "Medium",
    Discount > 0.5 & Discount <= 1 ~ "High"
  ))

# Sort dataframe by Sales descending
superstore_sorted <- superstore %>%
  arrange(desc(Sales))
