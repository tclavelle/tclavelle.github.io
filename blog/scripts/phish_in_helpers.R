library(tidyverse)
library(httr)
library(stringr)

# Get data from phish.in API for song info
song_res <- GET('http://phish.in/api/v1/songs.json?per_page=10000')
song_raw <- song_res %>% content %>% .$data
song_counts <- map_df((seq_along(song_raw)), function(x) {
  list(song         = song_raw[[x]]$title,
       song_id      = song_raw[[x]]$id,
       performances = song_raw[[x]]$tracks_count)
})

# Get data from phish.in API for tracks
res <- GET('http://phish.in/api/v1/tracks.json?per_page=10000')
raw <- res %>% content %>% .$data 
  
# Convert to data_frame
track_df <- map_df(seq_along(raw), function(x) {
  list(song       = raw[[x]]$title,
       song_id    = raw[[x]]$song_ids[1],
       track_id   = raw[[x]]$id,
       set        = raw[[x]]$set_name,
       length     = raw[[x]]$duration / 1000 / 60,
       mp3        = raw[[x]]$mp3,
       slug       = raw[[x]]$slug)
})
  
# Write function to get data for all performances of a desired song
song_info <- function(track_id) {
  track <- GET(paste0('http://phish.in/api/v1/songs/',track_id,'.json?per_page=1000'))
  raw <- track %>% content %>% .$data %>% .$tracks 
  
  # Convert to data_frame
  song_df <- map_df(seq_along(raw), function(x) {
    list(song       = raw[[x]]$title,
         track_id   = raw[[x]]$id,
         set        = raw[[x]]$set,
         date       = raw[[x]]$show_date,
         position   = raw[[x]]$position,
         length     = raw[[x]]$duration / 1000 / 60,
         mp3        = raw[[x]]$mp3,
         slug       = raw[[x]]$slug)
  })
  return(song_df)
}

# pull out all performances for every song with more than 100 performances
top_songs <- lapply(song_counts$song_id[song_counts$performances>300], song_info) %>%
  bind_rows()

# song summary (use to determine which jams to explore)
top_song_summary <- top_songs %>%
  group_by(song) %>%
  summarize(count = length(song),
            avg_length = mean(length, na.rm = T),
            sd_length  = sd(length, na.rm = T)) %>%
  filter(grepl('>', song) == F)

# calculate summary statistics for each song
top_songs %>%
  filter(song %in% c('You Enjoy Myself', 'Tweezer', 'Reba', 'Chalk Dust Torture', 'David Bowie',
                     "Suzy Greenberg", 'Runaway Jim', 'Possum', 'Harry Hood')) %>%
  left_join(top_song_summary) %>%
  ggplot(aes(x = length, fill = song)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~song, scales = 'free')
