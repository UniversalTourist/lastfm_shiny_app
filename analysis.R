#
library(tidyverse)
library(lubridate)
#clean lastfm data
samcohen_lastfm_data <- read_csv("samcohen_lastfm_data.csv", 
                                 col_names = FALSE)

clean_data <- samcohen_lastfm_data  %>% 
  rename(
    artist = X1,
    album = X2,
    song = X3,
    stream_date_chr = X4
    ) %>% 
   mutate(stream_date = as.Date(stream_date_chr, format = "%d %B %Y"),
          stream_date_ts = as.POSIXct(stream_date_chr, format = "%d %B %Y %H:%M"),
          stream_hour = hour(stream_date_ts)) %>% 
   mutate(artist = case_when(artist %in% c("Florence and The Machine", "Florence + the Machine") ~ "Florence + The Machine",
                             TRUE ~ artist)) %>% 
   mutate(day_night = case_when(stream_hour < 12 ~ "Night to Noon",
                                stream_hour >= 12 ~ "Noon to Night")) %>% 
   mutate(weekday = wday(stream_date, label = TRUE),
          weekday_or_not = case_when(weekday %in% c("Sat", "Sun") ~ "Weekend",
                                     TRUE ~ "Weekday"))   
  #write_csv("clean_lastfm_data.csv")
