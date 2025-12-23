#===============================
# Car Dataset Analysis Project with Insights
#===============================

# Install tidyverse
install.packages("tidyverse")
library(tidyverse)

# Install readr
install.packages("readr")
library(readr)

# Install ggplot2
install.packages("ggplot2")
library(ggplot2)

# Install stringr
install.packages("stringr")
library(stringr)

# Install dplyr
install.packages("dplyr")
library(dplyr)

# Install corrplot
install.packages("corrplot")
library(corrplot)

# STEP 2 — Load Dataset
car_data <- read_csv("Cars_Dataset.csv")

# STEP 3 — Preview and Check Missing Data
head(car_data)
str(car_data)
colSums(is.na(car_data))
car_data <- na.omit(car_data)

# STEP 4 — Clean Numeric Columns
car_data$Price <- car_data$`Cars Prices` |>
  str_replace_all("\\$", "") |>
  str_replace_all(",", "") |>
  str_replace_all(" ", "") |>
  sapply(function(x) {
    if (grepl("-", x)) {
      mean(as.numeric(str_split(x, "-")[[1]]))
    } else {
      as.numeric(x)
    }
  })

summary(car_data$Price)



car_data$HP <- str_extract(car_data$HorsePower, "\\d+") %>% as.numeric()
car_data$Performance <- str_extract(car_data$`Performance(0 - 100 )KM/H`, "\\d+\\.?\\d*") %>%
  as.numeric()
car_data$TotalSpeed <- str_extract(car_data$`Total Speed`, "\\d+\\.?\\d*") %>%
  as.numeric()

#convert in one number and make it numeric
car_data$Seats_clean <- sapply(as.character(car_data$Seats), function(x) {
  
  x <- str_trim(x)  # Remove leading/trailing spaces
  if (is.na(x) || x == "") return(NA)
  
  nums <- as.numeric(str_extract_all(x, "\\d+")[[1]])
  
  if (str_detect(x, "\\+")) {
    # Sum numbers for plus-separated seats, e.g. "2+2" → 4
    return(sum(nums))
  }
  
  if (str_detect(x, "-")) {
    # Take max number for hyphen-separated range, e.g. "2-3" → 3
    return(max(nums))
  }
  
  # Single number case, e.g. "5"
  if (length(nums) == 1) {
    return(nums)
  }
  
  # If none of above, return NA
  return(NA)
})

# Optional: Sanity cap to remove unrealistic seat counts
car_data$Seats_clean[car_data$Seats_clean > 15] <- NA
car_data$Seats <- as.numeric(car_data$Seats)

# Clean Torque safely (take first number if range exists)
car_data$Torque <- str_extract(car_data$Torque, "\\d+\\.?\\d*") %>% as.numeric()


# STEP 5 — Categorize Cars by Price
car_data$Category <- ifelse(car_data$Price > 300000, "Luxury", "Normal")
car_data <- car_data[is.finite(car_data$Price), ]

View(car_data)
sort(unique(car_data$Seats_clean))


#===============================
# 1. DESCRIPTIVE ANALYSIS
#===============================

# 1a. Bar Chart – Count of Cars by Company Names
brand_count <- car_data %>% group_by(`Company Names`) %>% summarise(TotalCars = n()) %>% arrange(desc(TotalCars))
ggplot(brand_count, aes(x = reorder(`Company Names`, TotalCars), y = TotalCars, fill = `Company Names`)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  labs(title = "Count of Cars by Company", x = "Company", y = "Total Cars") + theme_minimal()
# Insight: Ferrari and Toyota appear most frequently in the dataset, showing brand prevalence.

# 1b. Histogram / Density – Distribution of HorsePower
ggplot(car_data, aes(x = HP)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = 50 * ..count..), color = "red", size = 1) +
  labs(title = "Distribution of HorsePower", x = "HorsePower (HP)", y = "Count") + theme_minimal()
# Insight: Most cars fall in the 200-700 HP range; supercars (700+) are less frequent.


# 1c. Histogram / Density – Distribution of Cars Prices
ggplot(car_data, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  scale_x_log10() +
  labs(
    title = "Distribution of Car Prices (Log Scale)",
    x = "Price (log10 scale)",
    y = "Count"
  ) +
  theme_minimal()

# Density
ggplot(car_data, aes(x = Price)) +
  geom_density(fill = "red", alpha = 0.4) +
  scale_x_log10() +
  theme_minimal()
# Insight: Majority of cars are under $300,000; luxury cars above $1M are rare outliers.

# 1d. Bar Chart – Count of Cars by Fuel Types
ggplot(car_data, aes(x = `Fuel Types`, fill = `Fuel Types`)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Count of Cars by Fuel Type", x = "Fuel Type", y = "Count") +
  theme_minimal() +
  coord_flip()
# Insight: Petrol cars dominate the dataset; hybrids are very few.

#===============================
# 2. COMPARISON ANALYSIS
#===============================

# 2a. Boxplot – Cars Prices by Company Names
ggplot(car_data, aes(x = reorder(`Company Names`, Price, median), y = Price, fill = `Company Names`)) +
  geom_boxplot(show.legend = FALSE) + coord_flip() +
  labs(title = "Car Prices by Company", x = "Company", y = "Price ($)") + theme_minimal()
# Insight: Brands like Ferrari and Lamborghini have much higher median prices; Toyota and Ford are lower.

# 2b. Boxplot – Performance(0-100 KM/H) by Engines
top15 <- car_data |>
  count(Engines, sort = TRUE) |>
  slice_head(n = 15) |>
  pull(Engines)

car_data |>
  filter(Engines %in% top15) |>
  ggplot(aes(x = reorder(Engines, Performance, FUN = median),
             y = Performance,
             fill = Engines)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "0–100 KM/H Performance (Top 15 Engines)",
    x = "Engine",
    y = "Performance (sec)"
  ) +
  theme_minimal(base_size = 12)
# Insight: V12 engines generally accelerate faster (lower 0-100 sec) than I4 or smaller engines.

# 2c. Bar Chart / Violin Plot – Average Total Speed by Seats
avg_speed <- car_data %>%
  group_by(Seats_clean) %>%   # use numeric cleaned column
  summarise(AverageSpeed = mean(TotalSpeed, na.rm = TRUE))

# Plot
ggplot(avg_speed, aes(x = Seats_clean, y = AverageSpeed, fill = as.factor(Seats_clean))) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = avg_speed$Seats_clean) +  # show actual seat numbers
  labs(title = "Average Total Speed by Seats",
       x = "Seats",
       y = "Average Speed (km/h)") +
  theme_minimal()

# Insight: Cars with 2 seats (sports cars) generally have higher top speeds than 4-5 seaters.

#===============================
# 3. RELATIONSHIPS / CORRELATION
#===============================

# 3a. Scatter Plot – HorsePower vs Total Speed
ggplot(car_data, aes(x = HP, y = TotalSpeed, color = Category)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "HorsePower vs Total Speed", x = "HorsePower (HP)", y = "Top Speed (km/h)") + theme_minimal()
# Insight: Higher HP generally leads to higher top speed, but exceptions exist.

# 3b. Scatter Plot – Cars Prices vs HorsePower
ggplot(car_data, aes(x = HP, y = Price, color = Category)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Car Prices vs HorsePower", x = "HorsePower (HP)", y = "Price ($)") + theme_minimal()
# Insight: Luxury cars have very high prices; HP partially explains price but brand also matters.

# 3c. Heatmap – Correlation between numeric columns
numeric_cols <- car_data %>% select(HP, TotalSpeed, Performance, Price, Torque)
cor_matrix <- cor(numeric_cols, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black", tl.cex = 0.8)
# Insight: Performance inversely correlates with 0-100 sec; Price correlates moderately with HP and Torque.


# Bubble Plot – HorsePower vs Car Prices with Company Names as color & Seats as bubble size
ggplot(car_data, aes(x = HP, y = Price, size = Seats, color = `Company Names`)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "HorsePower vs Car Prices (Bubble by Seats)",
    x = "HorsePower (HP)",
    y = "Price ($)"
  ) +
  theme_minimal() +
  scale_size(range = c(3, 10))

# Insight: Visualizes multi-dimensional relationship: luxury brands have higher HP, price, and fewer seats.


# Top 3 Luxury cars by Price
top_luxury <- car_data %>% 
  filter(Category == "Luxury") %>% 
  arrange(desc(Price)) %>% 
  select(`Company Names`, `Cars Names`, Price) %>% 
  head(3)

# Top 3 Normal cars by Price
top_normal <- car_data %>% 
  filter(Category == "Normal") %>% 
  arrange(desc(Price)) %>% 
  select(`Company Names`, `Cars Names`, Price) %>% 
  head(3)

top_luxury
top_normal


ggplot(car_data, aes(x = Category, fill = Category)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Number of Luxury vs Normal Cars", x = "Category", y = "Count") +
  theme_minimal()