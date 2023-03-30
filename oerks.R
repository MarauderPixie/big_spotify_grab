x <- Sys.time()
test <- get_artist_audio_features(t50_artists$id[6], include_groups = c("album", "single"))
y <- Sys.time()
difftime(y, x)

map_df(t50_artists$id, function(i){
  pre  <- Sys.time()
  tmp  <- get_artist_audio_features(i, include_groups = c("album", "single"))
  post <- Sys.time()

  tibble(

  )
})

obs <- nrow(t50_artists)

df <- tibble(
  id   = seq(obs),
  pre  = seq(obs),
  post = seq(obs),
  features = seq(obs)
)

for (i in seq(obs)) {
  pre  <- Sys.time()
  tmp  <- get_artist_audio_features(t50_artists$id[i], include_groups = c("album", "single"))
  post <- Sys.time()

  df$id[i]    <- t50_artists$id[i]
  df$pre[i]   <- pre
  df$post[i]  <- post
  df$features[i] <- list(tmp)
  if (i != obs){
    cat("Status:", i, "of", obs, "--", paste0(round(i/obs*100), "%"), "\t\r")
  } else {
    cat("100% -- Done!")
  }
}
