
# ============================================================
# PROJECT: An Interactive Analysis of Social Media Usage
#          and Screen Time
# TOOL: RStudio / Posit Cloud
# ============================================================

# 1. INSTALL AND LOAD REQUIRED PACKAGES

# Run this installation command only if needed:
# install.packages("ggplot2")

library(ggplot2)

# 2. IMPORT DATASET

social_media <- read.csv(
  "social_media_usage_screen_time_sample.csv"
)

# Display first 6 records
head(social_media)

# Check dataset structure
str(social_media)

# Dataset dimensions
dim(social_media)

# 3. DATA CLEANING AND PREPROCESSING

# Check missing values
colSums(is.na(social_media))

# Count duplicate records
sum(duplicated(social_media))

# Remove duplicate records
social_media <- social_media[
  !duplicated(social_media), ]

# Remove rows with missing values
social_media <- na.omit(social_media)

# Convert categorical variables to factors
social_media$Gender <- as.factor(social_media$Gender)
social_media$Social_Media_Platform <-
  as.factor(social_media$Social_Media_Platform)
social_media$Usage_Purpose <-
  as.factor(social_media$Usage_Purpose)
social_media$Daily_Usage_Frequency <-
  as.factor(social_media$Daily_Usage_Frequency)
social_media$Productivity_Level <-
  as.factor(social_media$Productivity_Level)
social_media$Social_Media_Impact <-
  as.factor(social_media$Social_Media_Impact)

# Check cleaned dataset
str(social_media)
summary(social_media)

# 4. DESCRIPTIVE STATISTICS

# Average age
mean(social_media$Age)

# Average daily screen time
mean(social_media$Daily_Screen_Time_Hours)

# Average sleep hours
mean(social_media$Sleep_Hours)

# Minimum and maximum screen time
range(social_media$Daily_Screen_Time_Hours)

# Most-used social media platform
sort(
  table(social_media$Social_Media_Platform),
  decreasing = TRUE
)

# Usage purpose distribution
table(social_media$Usage_Purpose)

# Productivity level distribution
table(social_media$Productivity_Level)

# 5. DATA VISUALIZATION

# GRAPH 1: Social Media Platform Distribution

ggplot(social_media,
       aes(x = Social_Media_Platform,
           fill = Social_Media_Platform)) +
  geom_bar() +
  labs(
    title = "Social Media Platform Distribution",
    x = "Social Media Platform",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# GRAPH 2: Daily Screen Time Distribution

ggplot(social_media,
       aes(x = Daily_Screen_Time_Hours)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Daily Screen Time Distribution",
    x = "Screen Time (Hours)",
    y = "Number of Respondents"
  ) +
  theme_minimal()

# GRAPH 3: Screen Time and Sleep Hours

ggplot(social_media,
       aes(x = Daily_Screen_Time_Hours,
           y = Sleep_Hours)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Screen Time and Sleep",
    x = "Daily Screen Time (Hours)",
    y = "Sleep Hours"
  ) +
  theme_minimal()

# GRAPH 4: Productivity Level Distribution

ggplot(social_media,
       aes(x = Productivity_Level,
           fill = Productivity_Level)) +
  geom_bar() +
  labs(
    title = "Productivity Level of Respondents",
    x = "Productivity Level",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# GRAPH 5: Social Media Usage Purpose

ggplot(social_media,
       aes(x = Usage_Purpose,
           fill = Usage_Purpose)) +
  geom_bar() +
  labs(
    title = "Purpose of Social Media Usage",
    x = "Usage Purpose",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 6. CORRELATION ANALYSIS

correlation <- cor(
  social_media$Daily_Screen_Time_Hours,
  social_media$Sleep_Hours
)

print(correlation)

# 7. SAVE CLEANED DATASET

write.csv(
  social_media,
  "cleaned_social_media_usage.csv",
  row.names = FALSE
)

# 8. FINAL OUTPUT

cat("====================================\n")
cat("SOCIAL MEDIA ANALYSIS COMPLETED!\n")
cat("====================================\n")
cat("Total Respondents:", nrow(social_media), "\n")
cat("Average Screen Time:",
    round(mean(social_media$Daily_Screen_Time_Hours), 2),
    "hours\n")
cat("Average Sleep Hours:",
    round(mean(social_media$Sleep_Hours), 2),
    "hours\n")
cat("Correlation Between Screen Time and Sleep:",
    round(correlation, 3), "\n")
cat("Cleaned dataset successfully saved!\n")

