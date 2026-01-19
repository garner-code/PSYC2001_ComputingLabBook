# Analysis Script for Social Media Data
# This script demonstrates data analysis following PSYC2001 coding conventions

# Check if packages are installed, if not install.
if(!require(here)) install.packages('here') # checks if a package is installed and installs it if required.
if(!require(tidyverse)) install.packages('tidyverse')

library(here) # loads in the specified package
library(tidyverse)

# Load the cleaned social media data
social_media <- read.csv(file = here("Data","PSYC2001_social-media-data-cleaned.csv")) # reads in CSV files

# View the structure of the data
str(social_media) # provides a summary of the data structure

# Get a summary of the data
summary(social_media)

# View the first few rows
head(social_media)

# Data wrangling: Create a likes variable (average of good and bad mood likes)
social_media_likes <- social_media %>% 
  mutate(likes = (bad_mood_likes + good_mood_likes) / 2) %>% # creates a new variable called likes which is the average
  select(id, urban, likes, followers, age) # selects only the specified columns from the data frame

# Check the result
head(social_media_likes)

# Convert urban to a factor with meaningful labels
social_media_likes <- social_media_likes %>% 
  mutate(urban = factor(urban, 
                       levels = c(1, 2), 
                       labels = c("urban", "rural"))) # converts urban variable to a factor with labels

# Data visualization: Prepare data for density plots
social_media_long <- social_media %>% 
  select(id, good_mood_likes, bad_mood_likes) %>% # choose columns
  pivot_longer(cols = ends_with("likes"), 
               names_to = "mood", 
               values_to = "likes") # take columns ending with "likes" and pivot to longform

# Create density plot comparing good vs bad mood likes
social_media_long %>% 
  ggplot(aes(x = likes, group = mood, fill = mood)) + # set canvas aesthetics
  geom_density(alpha = 0.5) + # use the data to draw a density plot and make it 50% transparent
  theme_classic() # themes can be provided to ggplot which give it a bunch of aesthetics to change

# Statistical analysis: Paired t-test for mood effect on likes
t.test(social_media$good_mood_likes, 
       social_media$bad_mood_likes, 
       paired = TRUE) # performs a paired t-test

# Create composite score for political attitude
social_media_attitude <- social_media %>% 
  mutate(polit_attitude = 0.25 * polit_informed + 
                         0.35 * polit_campaign + 
                         0.4 * polit_activism) # creates political attitude composite score

# Check the result
head(social_media_attitude)

# Visualize relationship between time on social media and political attitude
social_media_attitude %>% 
  ggplot(aes(x = polit_attitude, y = time_on_social)) + # set canvas aesthetics
  geom_point(alpha = 0.5) + # add points with transparency
  geom_smooth(method = "lm", se = TRUE) + # add linear regression line with confidence interval
  theme_classic() + # apply classic theme
  labs(x = "Political Attitude", 
       y = "Time on Social Media (hours/day)",
       title = "Relationship between Political Attitude and Social Media Use") # add labels

# Correlation analysis
cor.test(x = social_media_attitude$time_on_social, 
         y = social_media_attitude$polit_attitude) # performs Pearson correlation test

# Linear regression analysis
model <- lm(time_on_social ~ polit_attitude, 
           data = social_media_attitude) # fits a linear regression model

# View regression results
summary(model) # provides summary of the regression model

# Save the results to the Output folder
write.csv(social_media_attitude, 
          file = here("Output", "PSYC2001-social-media-attitude.csv"), 
          row.names = FALSE) # saves the data frame as a CSV file
