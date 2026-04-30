# Load libraries 
library(tidyverse)
library(dplyr)
library(ggplot2)

# Load the Online Retail dataset.  
shop_sphere <- read_csv("online_retail.csv", show_col_types = FALSE)
shop_sphere

# Inspect structure: str, glimpse, summary
str(shop_sphere)
glimpse(shop_sphere)
summary(shop_sphere)

# Check missing values
anyNA(shop_sphere)
colSums(is.na(shop_sphere))

# Clean dataset. Do the following: Remove rows where CustomerID is missing.
shop_sphere <- shop_sphere %>%
  filter(!is.na(CustomerID), !is.na(Description))
shop_sphere

# Remove rows where Quantity <= 0. Remove rows where UnitPrice <= 0.
shop_sphere <- shop_sphere %>%
  filter(Quantity > 0, UnitPrice > 0)
shop_sphere

# Create a new column: Revenue = Quantity * UnitPrice
shop_sphere <- shop_sphere %>%
  mutate(Revenue = Quantity * UnitPrice)
shop_sphere

# Create a dataset called: customer_summary. It should contain: CustomerID. Total Revenue (sum of Revenue per customer). Number of Orders (count of invoices per customer). Average Order Value
customer_summary <- shop_sphere %>%
  group_by(CustomerID) %>%
  summarise(
    total_revenue = sum(Revenue),
    number_of_orders = n_distinct(InvoiceNo),
    avg_order_value = mean(Revenue)
  ) %>%
  arrange(desc(total_revenue))
customer_summary

# Create a new column: customer_segment. Use rules like: High Value → top 25% revenue customers. Medium Value → middle 50%. Low Value → bottom 25%. (Hint: think about quantile())
customer_summary <- customer_summary %>%
  mutate(
    customer_segment = case_when(
      total_revenue >= quantile(total_revenue, 0.75) ~ "High Value",
      total_revenue >= quantile(total_revenue, 0.25) ~ "Medium Value",
      TRUE ~ "Low Value"
    )
  )
customer_summary

# Build 3 charts. Chart 1: Revenue by Customer Segment. X = segment, Y = total revenue
customer_summary %>%
  group_by(customer_segment) %>%
  summarise(total_revenue = sum(total_revenue)) %>%
  arrange(customer_segment) %>%
  ggplot(aes(x = customer_segment, y = total_revenue, fill = customer_segment)) +
  geom_col() +
  labs(
    title = "Revenue by Customer Segment",
    x = "Customer Segment",
    y = "Total Revenue"
  )

# Chart 2: Number of Customers per Segment: Count how many customers are in each segment
customer_summary %>%
  group_by(customer_segment) %>%
  summarise(number_of_customers = n()) %>%
  arrange(customer_segment) %>%
  ggplot(aes(x = customer_segment, y = number_of_customers, fill = customer_segment)) +
  geom_col() +
  labs(
    title = "Number of Customers Per Segment",
    x = "Customer Segment",
    y = "Number of Customers"
  )

# Chart 3: Top 10 Customers by Revenue: Horizontal bar chart. Show highest spending customers
shop_sphere %>%
  group_by(CustomerID) %>%
  summarise(total_revenue = sum(Revenue, na.rm = TRUE)) %>%
  slice_max(total_revenue, n = 10) %>%
  ggplot(aes(x = reorder(as.factor(CustomerID), total_revenue),
             y = total_revenue, 
             fill = as.factor(CustomerID))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Customers by Revenue",
    x = "Customer Identifier",
    y = "Top 10 Customers"
  )