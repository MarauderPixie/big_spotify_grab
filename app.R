#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
## Chat GPT to the rescue?
# my_config <- httr::config(browserfunction = function(url) {
#     browseURL(url, browser = "firefox")
# })

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

scopes_ltd <- paste("playlist-read-private", "playlist-read-collaborative",
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
    cat("Step 1 -- just a sec. Or 5.\n")
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
                up_labels = c("Artists", "Tracks", "Audio Features"),
                down_labels = c("1.", "2.", "3."),
                icons = list(icon("spotify"), icon("spotify"), icon("spotify")),
                completes = c(FALSE, FALSE, FALSE)
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
        cat("open db connection\n")
        con <- dbConnect(
            RPostgres::Postgres(),
            dbname = db_params$dbname,
            host = db_params$host,
            port = db_params$port,
            user = db_params$user,
            password = db_params$pass
        )

        cat("call spotify\n")
        updateSpsTimeline(session, "timeline", 1, up_label = "0000", down_label = "1.")

        pgres$top50_artists <- get_my_top_artists_or_tracks(
            time_range = input$time_range,
            limit = 50, authorization = token
        )  %>%
            mutate(
                uuid = current_uuid,
                user = input$user_name,
                time_range = input$time_range
            )

        Sys.sleep(.5)
        updateSpsTimeline(session, "timeline", 2, up_label = "0000", down_label = "2.")

        cat("close db connection\n\n")
        dbDisconnect(con)
        Sys.sleep(3)
        updateSpsTimeline(session, "timeline", 3, up_label = "0000", down_label = "3.")
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
