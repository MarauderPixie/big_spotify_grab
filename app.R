#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DBI)
library(dplyr)
library(tidyr)
library(shiny)
library(httr)
library(shinyjs)
library(spsComps)
library(spotifyr)

db_params  <- config::get(config = "spotgres")
app_params <- config::get()

options("httr_oauth_cache" = TRUE)
Sys.setenv(SPOTIFY_CLIENT_ID = app_params$id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_params$secret)



###################################
## oAuth prep

if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/"
} else {
  # deployed URL
  APP_URL <- "https://marauderpixie.shinyapps.io/spotify_history_catcher/"
}


app <- oauth_app(
  "spotifyr",
  key    = Sys.getenv("SPOTIFY_CLIENT_ID"),
  secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
  redirect_uri = APP_URL
)

## Scopes as of 29.03.2023:
# > scopes()
# [ 1] "ugc-image-upload"            "user-read-playback-state"    "user-modify-playback-state"  "user-read-currently-playing" "app-remote-control"
# [ 6] "streaming"                   "playlist-read-private"       "playlist-read-collaborative" "playlist-modify-private"     "playlist-modify-public"
# [11] "user-follow-modify"          "user-follow-read"            "user-read-playback-position" "user-top-read"               "user-read-recently-played"
# [16] "user-library-modify"         "user-library-read"           "user-read-email"             "user-read-private"

scopes_ltd <- paste("playlist-read-private", "playlist-read-collaborative", "user-follow-read",
                    "user-top-read", "user-read-recently-played", "user-library-read")

# very clunky for the time being:
api <- oauth_endpoint(
  authorize = "https://accounts.spotify.com/authorize",
  access = "https://accounts.spotify.com/api/token"
)

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  # cat("has_auth_code\n")
  return(!is.null(params$code))
}


uiFunc <- function(req) {
  # cat("Step 1 -- just a sec. Or 5.\n")
  Sys.sleep(5)
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    # cat(paste("parseQueryString #1:", parseQueryString(req$QUERY_STRING), "\n\n"))
    # cat("oauth -- generate URL\n")
    url <- oauth2.0_authorize_url(api, app, scope = scopes_ltd)
    # cat("oauth -- location.replace(...)\n")
    redirect <- sprintf("location.replace(\"%s\");", url)
    # cat("oauth -- ship script\n\n")
    Sys.sleep(2)
    tags$script(HTML(redirect))
  } else {
    # cat(paste("parseQueryString #2:", parseQueryString(req$QUERY_STRING), "\n\n"))
    # cat("validate-ish -- generate token?\n")
    #########################################
    ## adjustment by comment
    token <<- oauth2.0_token(
      app = app,
      endpoint = api,
      credentials = oauth2.0_access_token(
        api, app, parseQueryString(req$QUERY_STRING)$code
      ),
      cache = FALSE
    )
    # validate before launching UI
    # print(token)
    # cat("validate-ish -- GET request\n")
    resp <- GET('https://api.spotify.com/v1/tracks/2TpxZ7JUBn3uw46aR7qd6V',
                add_headers(Authorization = paste("Bearer", token$credentials$access_token)))
    stop_for_status(resp)
    ###############
    # cat("all done -- calling UI\n\n")
    ui
  }
}



################################################################
# Define UI
ui <- fluidPage(
  
  # theme & js
  theme = shinythemes::shinytheme("darkly"),
  useShinyjs(),
  
  # Application title
  titlePanel("API Call Prototype"),
  
  # Sidebar
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("time_range", label = h3("Zeitraum"),
                   choices = list("etwa 4 Wochen" = "short_term",
                                  "etwa 6 Monate" = "medium_term",
                                  "mehrere Jahre" = "long_term"),
                   selected = 1),
      hr(),
      actionButton("top50", label = "Get Top 50 Artists", icon = icon("spotify")),
      hr(),
      
      textInput("user_name", label = "User (optional)"),
      actionButton("donation", label = "Donate Data", icon = icon("spotify"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      # gallery(texts = test_imgs$name,
      #         hrefs = test_imgs$url,
      #         images = test_imgs$url,
      #         title = "Top 10",
      #         image_frame_size = 1,
      #         enlarge = TRUE),
      # uiOutput("houz"),
      fluidRow(htmlOutput("test_img")),
      
      hr(),
      fluidRow(
        column(4, tableOutput("top10_artists")),
        column(4, tableOutput("top10_genres")),
        column(4, tableOutput("top10_tracks"))
      ),
      
      hr(),
      spsTimeline(
        "timeline",
        up_labels = c("Top 100", "Saved Items", "Followed Artists", "Recently Played"),
        down_labels = c("", "", "", ""),
        icons = list(icon("spotify"), icon("spotify"), icon("spotify"), icon("spotify")),
        completes = c(FALSE, FALSE, FALSE, FALSE)
      )
    )
  )
)




##################################################################
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # cat("\n##########################################\n## JUST RUN THE SERVER FUNCTION ALREADY ##\n##########################################\n\n")
  
  ###############################
  ## from Hadley
  params <- parseQueryString(isolate(session$clientData$url_search))
  # cat(paste("\nparams$code:", params$code, "\n\n"))
  
  if (!has_auth_code(params)) {
    # cat("has_auth_code returning NULL\n")
    return()
  }
  ###############################
  
  
  
  #####
  ## futile attempts at authenticating via spotifyr
  # api   <- reactiveValues(auth = NA, tkn = NA, msg = NA)
  #
  # observeEvent(input$authorize, {
  #     spsComps::shinyCatch({
  #         api$auth <- get_spotify_authorization_code(scope = scopes_ldt)
  #         api$tkn  <- get_spotify_access_token()
  #         api$msg  <- "click happened"
  #     },
  #     blocking_level = "error", prefix = "auth-observation")
  #
  #     showNotification("This is a notification.")
  # })
  #####
  
  current_uuid <- ids::uuid(use_time = TRUE)
  
  front <- reactiveValues()
  pgres <- reactiveValues()
  
  ##### Sth Sth userland-ish ----
  observeEvent(input$top50, {
    front$top50_artists <- get_my_top_artists_or_tracks(
      time_range = input$time_range,
      limit = 50, authorization = token
    )
    
    front$top50_tracks <- get_my_top_artists_or_tracks(
      type = "tracks", time_range = input$time_range,
      limit = 50, authorization = token
    ) %>%
      unnest(album.images) %>%
      filter(height == min(height)) %>%
      transmute(
        # artists = artists,
        img_url = url,
        track_title = name
      )
    
    front$top50_genres <- front$top50_artists %>%
      tidyr::unnest(cols = "genres") %>%
      mutate(
        # uuid   = current_uuid,
        # user   = input$user_name,
        genres = stringr::str_to_title(genres)
      ) %>%
      count(genres, sort = TRUE)
    
    
    ## top10 actually
    front$t10g <- front$top50_genres %>%
      head(10)
    
    front$t10a <- front$top50_artists %>%
      select(name) %>%
      head(10)
    
    front$t10t <- front$top50_tracks %>%
      # mutate(
      #     img_url = paste0("<img src=\"",img_url, "\" height=\"30\" data-placement=\"right\"></img>")
      # ) %>%
      head(10)
    
    # testing of raw html:
    # front$test_img <- paste0("<img src='", front$top50_tracks$img_url[1], "'></img>")
    # cat(front$test_img, "\n")
  })
  
  ########################################################
  ## Get User Data and write to postgres db
  loader_replace <- addLoader$new("donation", type = "facebook")
  
  observeEvent(input$donation, {
    loader_replace$show()
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = db_params$dbname,
      host = db_params$host,
      port = db_params$port,
      user = db_params$user,
      password = db_params$pass
    )
    
    
    #####
    ## Step 1: Top 100 Artists & Tracks
    user_artists <- tibble()
    user_tracks  <- tibble()
    sml <- c("short_term", "medium_term", "long_term")
    
    for (rng in sml) {
      
      artists1 <- get_my_top_artists_or_tracks(
        type  = "artists",
        limit = 50, 
        time_range = rng, 
        authorization = token
      )
      artists2 <- get_my_top_artists_or_tracks(
        type   = "artists",
        limit  = 50, 
        ## currently (as of 19.04.2023), spotify doesn't return anything if offset is >=50
        offset = min(nrow(artists1), 49),
        time_range = rng, 
        authorization = token
      )
      
      tracks1 <- get_my_top_artists_or_tracks(
        time_range = rng, 
        type = "tracks",
        limit = 50, 
        authorization = token
      )
      tracks2 <- get_my_top_artists_or_tracks(
        time_range = rng, 
        type = "tracks",
        limit = 50, 
        offset = min(nrow(tracks1), 49),
        authorization = token
      )
      
      
      tmp_artists <- bind_rows(
        artists1, artists2
      ) %>%
        mutate(
          uuid = current_uuid,
          user = input$user_name,
          position = seq(n()),
          time_range = rng
        )
      
      tmp_tracks <- bind_rows(
        tracks1, tracks2
      ) %>%
        mutate(
          uuid = current_uuid,
          user = input$user_name,
          position = seq(n()),
          time_range = rng
        )
      
      user_artists <- bind_rows(user_artists, tmp_artists)
      user_tracks  <- bind_rows(user_tracks, tmp_tracks)
    }
    
    pgres$user_top100_artists <- user_artists %>% 
      select(uuid, user, time_range, position, id, uri)
    pgres$user_top100_tracks  <- user_tracks %>% 
      select(uuid, user, time_range, position, id, uri)
    
    updateSpsTimeline(session, "timeline", 1, down_label = "done")
    
    
    #####
    ## Step 2: Saved tracks & albums
    offset_albums <- 0
    offset_tracks <- 0
    tmp_albums <- tibble()
    tmp_tracks <- tibble()
    
    while(nrow(tmp_albums) == 50 | offset_albums == 0){
      tmp_albums <- get_my_saved_albums(limit = 50, 
                                        offset = offset_albums,
                                        authorization = token)
      if (offset_albums != 0){
        user_saved_albums <- bind_rows(user_saved_albums, tmp_albums)
      } else {
        user_saved_albums <- tmp_albums
      }
      offset_albums <- offset_albums + 50
    }
    
    while(nrow(tmp_tracks) == 50 | offset_tracks == 0){
      tmp_tracks <- get_my_saved_tracks(limit = 50, 
                                        offset = offset_tracks,
                                        authorization = token)
      if (offset_tracks != 0) {
        user_saved_tracks <- bind_rows(user_saved_tracks, tmp_tracks)
      } else {
        user_saved_tracks <- tmp_tracks
      }
      offset_tracks <- offset_tracks + 50
      
      if (offset_tracks == 0) {
        shinyCatch(message("This may take while"), position = "top-center")
      }
      if (offset_tracks == 2000) {
        shinyCatch(message(""), position = "top-center")
      }
      if (offset_tracks == 4000) {
        shinyCatch(message("Still running..."), position = "top-center")
      }
    }
    
    pgres$user_saved_albums <- user_saved_albums %>%
      mutate(
        uuid = current_uuid,
        user = input$user_name
      ) %>% 
      select(uuid, user, added_at, album.id, album.uri)
    pgres$user_saved_tracks <- user_saved_tracks %>%
      mutate(
        uuid = current_uuid,
        user = input$user_name
      ) %>% 
      select(uuid, user, added_at, track.id, track.uri)
    
    updateSpsTimeline(session, "timeline", 2, down_label = "done") # , up_label = "0000", down_label = "2")
    
    
    #####
    ## Step 3: Followed Artists
    tmp_followed <- tibble()
    
    while(nrow(tmp_followed) == 50 | nrow(tmp_followed) == 0){
      if (nrow(tmp_followed) != 0) {
        tmp_followed <- get_my_followed_artists(after = last(user_followed_artists$id),
                                                limit = 50, authorization = token)
        user_followed_artists <- bind_rows(user_followed_artists, tmp_followed)
      } else {
        tmp_followed <- get_my_followed_artists(limit = 50, authorization = token)
        user_followed_artists <- tmp_followed
      }
    }
    
    pgres$user_followed_artists <- user_followed_artists %>% 
      mutate(
        uuid = current_uuid,
        user = input$user_name
      ) %>% 
      select(uuid, user, id, uri)
    
    updateSpsTimeline(session, "timeline", 3, down_label = "done")
    
    
    #####
    ## Step 4: Last 100 Tracks Played
    recent_50_vorne <- get_my_recently_played(
      limit = 50, authorization = token
    )
    recent_50_hinten <- get_my_recently_played(
      limit = 50, authorization = token,
      after =last(recent_50_vorne$played_at)
    )
    
    pgres$user_recently_played <- bind_rows(
      recent_50_vorne, recent_50_hinten
    ) %>% 
      mutate(
        uuid = current_uuid,
        user = input$user_name
      ) %>% 
      select(uuid, user, played_at, track.id, track.uri, context.uri)
    
    updateSpsTimeline(session, "timeline", 4, down_label = "done")
    
    
    
    #####
    ## Postgres Things
    dbDisconnect(con)
    loader_replace$hide()
    
    updateActionButton(inputId = "donation", label = "All done, thank you!")
    disable("donation")
  })
  ########################################################
  
  output$top10_genres <- renderTable({
    front$t10g
  }, hover = TRUE)
  
  output$top10_artists <- renderTable({
    front$t10a
  }, hover = TRUE)
  
  output$top10_tracks <- renderTable({
    front$t10t
  }, hover = TRUE)
  
  # output$test_img <- renderUI({HTML(front$test_img)})
}

# Run the application
shinyApp(uiFunc, server)
