


########################################################
## Image Shenanigans
observe({
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

  top10_genres <- artist_genres %>%
    count(genres, sort = TRUE) %>%
    head(10)

  x <- map(top10_genres$genres, function(gnr){
    artist_genres$name[artist_genres$genres == gnr]
  })

  n <- x[[1]]

  for (i in n) {
    print(i)
    local({
      my_i <- str_replace_all(i, " ", "")
      imagename = paste0("img", my_i)

      output[[imagename]] <-
        renderImage({
          list(src = test_imgs$url[test_imgs$name == i], # file.path('www', df_img$img_path[my_i]),
               width = "20%", height = "20%",
               alt = "ain't no image file there")
        }, deleteFile = FALSE)
    })
  }
})


output$houz <- renderUI({

  image_output_list <-
    lapply(n, function(i) {
      imagename = paste0("img", str_replace_all(i, " ", ""))
      print(imagename)
      imageOutput(imagename)
    })

  bla <- do.call(tagList, image_output_list)
  print(bla)
  return(bla)
})
########################################################