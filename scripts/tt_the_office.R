# This script pulls The Office transcripts via the schrute package and
# explores the show's ratings and dialogue

# Packages ----------------------------------------------------------------

library(tidytuesdayR)
library(schrute)
library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_minimal() + theme(panel.border = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank()))

# Load --------------------------------------------------------------------

df_transcripts <- theoffice

# Inspect -----------------------------------------------------------------

head(df_transcripts)
glimpse(df_transcripts)

# Find level of uniqueness
nrow(df_transcripts) # 55130
nrow(distinct(df_transcripts, season, episode)) # 186
nrow(distinct(df_transcripts, index)) # this is unique but just a row number
nrow(distinct(df_transcripts, season, episode, character, text_w_direction)) # 54421 - almost!
                                                                             # character could repeat lines

df_transcripts %>% 
  group_by(season, episode, character, text_w_direction) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1) #  Repeated lines (e.g., "Hey." "What?" etc.)

# Create separate ratings dataset -----------------------------------------

# Streamline and de-dupe ratings / basic episode info
df_ratings <- df_transcripts %>% 
  select(-c(index, character, text, text_w_direction))

# De-dup at season-episode level. Dropping complete duplicates should be sufficient
df_ratings <- distinct(df_ratings)

nrow(df_ratings) # 186
nrow(distinct(df_ratings, season, episode)) # 186

# How many episodes per season? -------------------------------------------

df_ratings %>% 
  ggplot(aes(x = season)) +
  geom_bar()

df_ratings %>% 
  group_by(season) %>% 
  summarize(n = n())

# How did the show's ratings vary over time? ------------------------------

nrow(distinct(df_ratings, air_date)) # 184 - double" show?

df_ratings %>% 
  group_by(air_date) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)

# Convert air_date to date type
count(df_ratings, air_date)

df_ratings <- df_ratings %>% 
  mutate(air_date = as_date(air_date))

# Plot
df_ratings %>% 
  ggplot(aes(x = air_date, y = imdb_rating)) +
  geom_point() + 
  geom_smooth(se = FALSE)

# Mark the seasons
df_ratings %>% 
  mutate(season = as.character(season)) %>% 
  ggplot(aes(x = air_date, y = imdb_rating, color = season)) +
  geom_point() +
  scale_color_brewer(type = 'qual', palette = 'Set1')

df_ratings %>% 
  group_by(season) %>% 
  mutate(season_start = min(air_date))

# TODO - do we need a zero-origin for y-axis?
df_ratings %>% 
  group_by(season) %>% 
  mutate(season_start = min(air_date)) %>% 
  ungroup() %>% 
  ggplot(aes(x = air_date, y = imdb_rating, color = as.character(season))) +
  geom_point() +
  guides(color = FALSE) +
  geom_vline(aes(xintercept = as.integer(season_start) - 10), size = 0.05) +
  labs(x = "Air Date",
       y = "IMDB Rating", 
       title = "The Office - Ratings by Episode") 
