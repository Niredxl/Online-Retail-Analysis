# Load necessary libraries
library(tidyverse)
library(arules)
library(arulesViz)

# ==========================================
# 1. Data Collection & Preprocessing
# ==========================================
# Load the dataset (Make sure to update the filename to match your new CSV)
retail_data <- read.csv("OnlineRetail.csv") 

# Clean the dataset by handling missing values and irrelevant data [cite: 23]
clean_data <- retail_data %>%
  # Remove leading/trailing whitespace from the Description column
  mutate(Description = str_trim(Description)) %>%
  # Handle missing values [cite: 59]
  drop_na(CustomerID, Description) %>%
  filter(Description != "") %>%
  # Filter invalid transactions[cite: 60]: keep positive quantities 
  # and remove cancelled orders (which start with 'C' in this specific dataset)
  filter(Quantity > 0) %>%
  filter(!grepl("^C", InvoiceNo)) %>%
  # Remove duplicate records [cite: 58]
  distinct()

# ==========================================
# 2. Data Transformation
# ==========================================
# Group products by InvoiceNo to create a list of items per transaction [cite: 62]
transaction_list <- split(clean_data$Description, clean_data$InvoiceNo)

# Convert into the transactional format suitable for association rule mining [cite: 24]
transactions <- as(transaction_list, "transactions")

# Let's double check your basket sizes - this should now show numbers greater than 1!
summary(size(transactions))

# ==========================================
# 3. Exploratory Data Analysis (EDA)
# ==========================================
# Visualize the top 15 most frequently purchased items [cite: 65]
itemFrequencyPlot(transactions, topN = 15, type = "absolute", 
                  main = "Top 15 Most Frequently Purchased Products",
                  col = "steelblue", ylab = "Item Frequency")

# ==========================================
# 4. Apriori Algorithm & Rule Generation
# ==========================================
# Apply the Apriori algorithm [cite: 26, 68]
# We keep minlen = 2 to ensure we find strong product combinations [cite: 30]
retail_rules <- apriori(transactions, 
                        parameter = list(supp = 0.01, conf = 0.5, minlen = 2, target = "rules"))

# Display the total number of rules generated
print(retail_rules)

# ==========================================
# 5. Rule Evaluation & Visualization
# ==========================================
# Evaluate rules using Support, Confidence, and Lift metrics [cite: 28, 72]
if (length(retail_rules) > 0) {
  
  # Sort rules by Lift to find the strongest associations
  strong_rules <- sort(retail_rules, by = "lift")
  
  # Inspect the top 10 rules in the console
  inspect(head(strong_rules, n = 10))
  
  # Visualize association rules using a scatter plot [cite: 29, 79]
  plot(retail_rules, measure = c("support", "confidence"), shading = "lift", 
       main = "Scatter Plot of Association Rules")
  
  # Visualize the product relationships using rule graphs [cite: 82]
  top_20_rules <- head(strong_rules, 20)
  plot(top_20_rules, method = "graph", engine = "htmlwidget", 
       main = "Network Graph of Top 20 Association Rules")
  
} else {
  print("0 rules found. Try lowering the 'supp' (e.g., 0.005) or 'conf' (e.g., 0.2) parameters.")
}