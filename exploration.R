####
## this already goes quite a bit into analysis and feature engineering;
## there's somehing off in the scaling operation - resulting numbers
## are way out of range it seems

artist_features %>%
  gather(feature, value, 9:19) %>%
  ggplot(aes(value)) +
  facet_wrap(~feature, scales = "free") +
  geom_histogram(binwidth = .05)

artist_features %>%
  gather(feature, value, 9:19) %>%
  group_by(feature) %>%
  filter(between(value, quantile(value, .01), quantile(value, .99))) %>%
  ggplot(aes(value)) +
  facet_wrap(~feature, scales = "free") +
  geom_histogram(binwidth = .05)

artist_features %>%
  gather(feature, value, 9:19) %>%
  group_by(feature) %>%
  filter(between(value,
                 quantile(value, .01),
                 quantile(value, .99))) %>%
  ungroup() %>%
  spread(feature, value) %>%
  mutate(
    l_energy   = log(energy),
    l_loudness = log(loudness),
    l_valence  = log(valence),
    sq_sppech  = speechiness^2,
    sq_acoust  = acousticness^2,
    sq_isntrum = instrumentalness^2
  ) %>%
  gather(feature, value,
         starts_with("l_"),
         starts_with("sq_")) %>%
  ggplot(aes(value)) +
  facet_wrap(~feature, scales = "free") +
  geom_histogram(binwidth = .05)


artist_feat_long <- artist_features %>%
  gather(feature, value, 9:19) %>%
  group_by(feature) %>%
  filter(between(value,
                 quantile(value, .01),
                 quantile(value, .99))) %>%
  mutate(
    val_norm = (value - mean(value)) / sd(value)
  ) %>%
  ungroup() %>%
  select(artist_name, artist_id, feature, value, val_norm)


artist_feat_summary <- artist_feat_long %>%
  group_by(artist_name, artist_id, feature) %>%
  summarise(
    feat_mean   = mean(val_norm),
    feat_median = median(val_norm),
    feat_sd     = sd(val_norm),
    feat_q10    = quantile(val_norm, .1),
    feat_q90    = quantile(val_norm, .9),
    .groups     = "drop"
  )

ggplot(artist_feat_summary, aes(feat_mean, feat_median)) +
  geom_hline(yintercept = 0, linewidth = .1, color = "red") +
  geom_vline(xintercept = 0, linewidth = .1, color = "red") +
  facet_wrap(~feature) +
  geom_point(alpha = .7) +
  scale_x_continuous(limits = c(-3, 4)) +
  scale_y_continuous(limits = c(-3, 4))
