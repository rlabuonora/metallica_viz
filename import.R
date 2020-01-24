library(spotifyr)
library(tidyverse)
library(lubridate)

# https://www.kaylinpavlik.com/song-distance/

METALLICA_ID <- "2ye2Wgw4gimLv2eAKyk1NB"
metallica_albums <- get_artist_albums(id=METALLICA_ID,
                                      include_groups = "album",
                                      limit = 50) %>% 
  select(album_name=name, album_id=id, release_date) %>% 
  slice(6, 11, 13, 17, 19, 21, 26, 30, 35, 39, 43)



get_album_tracks <- function(album_id, album_name) {
  # get track name, track id & album name (add all album.level data here)
  album <- get_album(id=album_id)
  tibble(track_name=album$tracks$items$name,
         track_id   = album$tracks$items$id,
         album_name = album_name)
}

get_track_info <- function(track_id, track_name, album_name) {
  features <- get_track_audio_features(id=track_id)
  mutate(features, 
         album_name = album_name,
         track_name = track_name) %>% 
    select(album_name, track_name, danceability:tempo, duration_ms)
}


metallica_tracks <- metallica_albums %>% 
  pmap(function(...) {
    current <- tibble(...)
    get_album_tracks(current$album_id, current$album_name)
  }) %>% 
  map(purrr::transpose) %>% 
  flatten %>% 
  map_df(function(x) {
    get_track_info(x$track_id, x$track_name, x$album_name)
  }) 

metallica_tracks_df <- metallica_tracks %>% 
  left_join(metallica_albums) %>% 
  mutate(album_name = str_remove(album_name, "\\(Remastered\\)")) %>% 
  mutate(track_name = str_remove(track_name, " - Remastered")) %>% 
  mutate(track_name = str_remove(track_name, "\\(Remastered\\)")) %>% 
  mutate(release_date = year(as_date(release_date))) %>% 
  group_by(album_name) %>% 
  mutate(track_num = row_number()) %>% 
  arrange(release_date, track_num)


saveRDS(metallica_tracks_df, file="metallica.rds")


