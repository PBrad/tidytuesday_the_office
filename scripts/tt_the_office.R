

# Packages ----------------------------------------------------------------

library(tidytuesdayR)
library(schrute)
library(dplyr)

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

