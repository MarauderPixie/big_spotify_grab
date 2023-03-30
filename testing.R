# library(dplyr)
library(tidyverse)
library(spotifyr)


# theme_set(hrbrthemes::theme_ft_rc()) # dark theme
theme_set(hrbrthemes::theme_ipsum_rc()) # light theme

###############
## get some from spotify
t50_artists <- get_my_top_artists_or_tracks(
  limit = 50, time_range = "short_term"
)

t50_tracks <- get_my_top_artists_or_tracks(
  type = "tracks", limit = 50,
  time_range = "short_term"
)

recent <- get_my_recently_played(limit = 50)

saved_albums <- get_my_saved_albums(limit = 50)
saved_tracks <- get_my_saved_tracks(limit = 50)


###############
## clean shit up
saved_tracks_tidy <- saved_tracks %>%
  mutate(
    artist    = map_chr(track.artists, ~pluck(., "name", first)),
    img_url_l = map_chr(track.album.images, ~pluck(., "url", first)),
    img_url_s = map_chr(track.album.images, ~pluck(., "url", last)),
    img_height_l = map_int(track.album.images, ~pluck(., "height", first)),
    img_height_s = map_int(track.album.images, ~pluck(., "height", last)),
    img_width_l  = map_int(track.album.images, ~pluck(., "width", first)),
    img_width_s  = map_int(track.album.images, ~pluck(., "width", last))
  ) %>%
  select(-track.href, -track.preview_url, -track.uri, -track.album.artists,
         -track.album.available_markets, -track.album.href, -track.album.images,
         -track.album.type, -track.album.uri, -track.artists)




genres_long <- unnest(t50_artists, cols = "genres") %>%
  mutate(
    genres = str_to_title(genres)
  ) %>%
  select(name, images, genres, id)

# pretty hefty operation, maybe slim it down somehow?
artist_features <- map_df(t50_artists$id, get_artist_audio_features, include_groups = c("album", "single"))
saveRDS(artist_features, "artist_features.rds") # juuuuust in case...


top10_genres <- artist_genres %>%
  count(genres, sort = TRUE) %>%
  head(10)

test_imgs <- artist_images %>%
  group_by(name) %>%
  filter(height == min(height))

x <- map(top10_genres$genres, function(gnr){
  genres_long$name[genres_long$genres == gnr]
})




top10_artist <- t50_artists %>%
  unnest(cols = "images") %>%
  select(name, popularity, url, height, width) %>%

  # mutate(
  #   size = case_when(
  #     height <= 240 ~ "img_s",
  #     height >= 480 ~ "img_l",
  #     TRUE ~ "img_m",
  #   )
  # ) %>%
  # select(name, popularity, size, url) %>%
  spread(size, url)




x <- t50_tracks %>%
  unnest(album.images) %>%
  filter(height == min(height)) %>%
  transmute(
    artists = artists,
    track_title = name,
    img_url = url) %>%
  unnest(artists)