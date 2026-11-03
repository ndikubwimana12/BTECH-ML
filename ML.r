#Data Acquisition, Exploration, Visualisation
##                      + Linear Machine Learning Algorithms
## -----------------------------------------------------------------------------
## DATASETS USED:
##   - mtcars   (built into base R)  -> regression: predict fuel efficiency (mpg)
##   - iris     (built into base R)  -> classification preview: species by petal size
## PACKAGES USED: tidyverse, skimr, broom, car
## =============================================================================

library(tidyverse) # dplyr (data wrangling) + ggplot2 (visualisation)
library(skimr)    # one-line, beautiful data summaries
library(broom)       # turns model output into a tidy data frame
library(car)          # regression diagnostic plots
library(dplyr) # also used for (visualisation)
library(skimr)
library(ggplot2)


## -----------------------------------------------------------------------------
## PART A. DATA ACQUISITION
## -----------------------------------------------------------------------------
# In real projects, data comes from a CSV, a database, or an API. Today we
# use datasets already built into R so we can focus 100% on the ML concepts.
data(mtcars)   # 32 cars: fuel economy (mpg) and design/performance features
data(iris)     # 150 flowers: 4 measurements, 3 species

# Always look at the STRUCTURE of your data before doing anything else:
str(mtcars)
str(iris)
summary(iris)# summary statistics
summary(mtcars)# summary statistics
glimpse(iris)

## -----------------------------------------------------------------------------
## PART B. EXPLORATORY DATA ANALYSIS (EDA)
## -----------------------------------------------------------------------------
# skim() gives you: missing values, mean, sd, min, max, and a tiny histogram
# for every column -- in ONE line of code.
skim(mtcars)

# Summary statistics recap (links back to Day 1, Module 2 - Mean, Variance...):
mtcars %>%
  summarise(
    mean_mpg = mean(mpg),
    sd_mpg   = sd(mpg),          # standard deviation = sqrt(variance)
    var_mpg  = var(mpg),
    min_mpg  = min(mpg),
    max_mpg  = max(mpg)
  )

# Correlation matrix: which variables move together? (Day 1, Module 2 - Correlation)
cor(mtcars) %>% round(digits=2)
cor(iris[, 1:4]) %>% round(digits = 2) # iris

# VISUALISATION -----------------------------------------------------------
# 1. Distribution of the target variable (mpg)
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 10, fill = "#2E86AB", colour = "white") +
  labs(title = "Distribution of Fuel Efficiency (mpg)",
       x = "Miles per Gallon", y = "Count of Cars") +
  theme_bw()

# 2. Relationship between weight and mpg (is there a pattern? a straight line?)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 3, colour = "#2E86AB") +
  geom_smooth(method = "lm", se = TRUE, colour = "#F26430") +
  labs(title = "Car Weight vs Fuel Efficiency",
       subtitle = "The blue line 'geom_smooth' previews what linear regression will fit",
       x = "Weight (1000 lbs)", y = "Miles per Gallon") +
  theme_minimal()

# 3. Boxplot: does the number of cylinders affect mpg?
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot() +
  labs(title = "Fuel Efficiency by Number of Cylinders",
       x = "Cylinders", y = "Miles per Gallon") +
  theme_minimal() + theme(legend.position = "none")

# 4. Pairwise scatterplots for the iris dataset (classification preview)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(size = 2.5) +
  labs(title = "Iris Species Separate Cleanly by Petal Size",
       subtitle = "This is why petal measurements are great classifier features") +
  theme_minimal()
#............................................
ggplot(iris, aes(x = Species, fill = Species)) +
  geom_bar() +
  labs(title = "Distribution of Iris Species",caption = "source:ML Training",
  
       x = "Species", y = "Count") + 
  theme_minimal() +
  theme(legend.position = "none")
