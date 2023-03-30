library(DBI)
library(tidyverse)
library(spotifyr)
# library(dplyr)
# library(dbplyr)


db_params <- config::get(config = "spotgres")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = db_params$dbname,
  host = db_params$host,
  port = db_params$port,
  user = db_params$user,
  password = db_params$pass
)

dbListTables(con)


#################################
## let's try this
## 1. Create some tables
##    -> apparently postgres can't deal with nested dataframes
test_uuid <- ids::uuid(use_time = TRUE)

top50 <- get_my_top_artists_or_tracks(limit = 50) %>%
  mutate(
    uuid = test_uuid,
    user = "tobi", # input$user_name,
    time_range = "short_term" # input$time_range
  )

artist_genres <- unnest(top50, cols = "genres") %>%
  mutate(
    genres = str_to_title(genres)
  ) %>%
  select(uuid, name, genres, id)

artist_images <- top50 %>%
  select(uuid, name, id, images) %>%
  unnest(cols = "images")

top50_flat <- top50 %>%
  select(-genres, -images)


dbWriteTable(con, "test50_flat", top50_flat)
dbWriteTable(con, "test50_genres", artist_genres)
dbWriteTable(con, "test50_images", artist_images)

## 2. try out some queries
# sth about primary keys? I need to look into that
dbGetQuery(con, 'ALTER TABLE test50_genres ADD CONSTRAINT genres_pk PRIMARY KEY ("name")')

# dbClearResult must be called after dbSendQuery,
# DBI/RPostgres' dbGetQuery does this for me tho

genre_query <- dbSendQuery(
  con, "SELECT name, genres FROM test50_genres WHERE genres = 'Folk' or name = 'Algiers'"
)

genre_results <- dbFetch(genre_query)

dbClearResult(genre_query)

# alternatively?
genre_get_query <- dbGetQuery(
  con, "SELECT name, genres FROM test50_genres WHERE genres = 'Folk' or name = 'Algiers'"
)
# ...seems easier; like, less steps and all :o


## 3. add more data
top50 <- get_my_top_artists_or_tracks(
  time_range = "long_term", limit = 50
) %>%
  mutate(
    uuid = test_uuid,
    user = "tobi", # input$user_name,
    time_range = "long_term" # input$time_range
  )

artist_genres <- unnest(top50, cols = "genres") %>%
  mutate(
    genres = str_to_title(genres)
  ) %>%
  select(uuid, name, genres, id)

artist_images <- top50 %>%
  select(uuid, name, id, images) %>%
  unnest(cols = "images")

top50_flat <- top50 %>%
  select(-genres, -images)

dbWriteTable(con, "test50_flat", top50_flat, append = TRUE)
dbWriteTable(con, "test50_genres", artist_genres, append = TRUE)
dbWriteTable(con, "test50_images", artist_images, append = TRUE)



## 4. Close the connection
dbDisconnect(con)


##############################
## Why not query with dbpplyr?
db_flat <- tbl(con, "test50_flat")

pop_query <- db_flat %>%
  filter(popularity > 50)

# see query:
pop_query %>% show_query()

# and execute:
pop_query %>% collect()
