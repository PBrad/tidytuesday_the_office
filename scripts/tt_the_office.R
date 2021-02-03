# This script pulls The Office transcripts via the schrute package and
# explores the show's ratings and dialogue

# Packages ----------------------------------------------------------------

library(tidytuesdayR)
library(schrute)
library(dplyr)
library(lubridate)
library(ggplot2)

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

# Create separate ratings dataset -----------------------------------------

# Streamline and de-dupe ratings / basic episode info
df_ratings <- df %>% 
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
