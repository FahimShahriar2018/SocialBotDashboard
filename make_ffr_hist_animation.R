# make_ffr_hist_animation.R ----
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(gifski)

# Load data (match the original histogram look: 0.00–1.00 range)
df <- read_excel("SocialBot.xlsx") %>%
  mutate(
    is_bot = factor(is_bot, levels = c(0, 1), labels = c("Human", "Bot")),
    follower_follow_rate = as.numeric(follower_follow_rate)
  ) %>%
  filter(!is.na(follower_follow_rate)) %>%
  mutate(follower_follow_rate = pmax(follower_follow_rate, 0)) %>%
  filter(follower_follow_rate <= 1)   # ✅ makes it look like your previous histogram (0–1)

# ---- Build bins to look like the old histogram (thin bins) ----
x_min <- 0
x_max <- 1
binwidth <- 0.025                   # ✅ thin bins similar to your old plot
breaks <- seq(x_min, x_max + binwidth, by = binwidth)

bin_df <- df %>%
  mutate(
    bin_left = cut(
      follower_follow_rate,
      breaks = breaks,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(is_bot, bin_left, name = "n") %>%
  complete(is_bot, bin_left, fill = list(n = 0)) %>%
  mutate(
    bin_start = as.numeric(gsub("\\[|\\(|,.*", "", as.character(bin_left))),
    bin_mid   = bin_start + binwidth / 2
  ) %>%
  arrange(is_bot, bin_mid)

# ---- CUMULATIVE reveal frames (previous bars NEVER disappear) ----
bin_df2 <- bin_df %>% mutate(bin_id = dense_rank(bin_mid))
n_bins <- max(bin_df2$bin_id)

anim_df <- tidyr::crossing(frame = 1:n_bins, bin_df2) %>%
  filter(bin_id <= frame) %>%
  arrange(frame, is_bot, bin_mid)

# ---- Plot (match your previous histogram style) ----
p <- ggplot(anim_df, aes(x = bin_mid, y = n, fill = is_bot, group = is_bot)) +
  geom_col(
    width = binwidth * 0.98,         # ✅ slight gap so bars don’t look “fat”
    position = "identity",           # overlay like your old plot
    alpha = 0.60
  ) +
  scale_fill_manual(values = c("Human" = "lightblue", "Bot" = "red")) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = sprintf("%.2f", c(0, 0.25, 0.50, 0.75, 1.00))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5)  # center title like your previous
  ) +
  labs(
    title = "Histogram of Follower/Following Rate by Account Type",
    x = "Follower/Following rate",
    y = "Count",
    fill = "Account type"
  ) +
  transition_manual(frame) +
  ease_aes("cubic-in-out")

anim <- animate(
  p,
  renderer = gifski_renderer(),
  width = 900,
  height = 450,
  fps = 20,
  duration = 6
)

if (!dir.exists("www")) dir.create("www")
anim_save("www/ffr_hist_animated.gif", animation = anim)

message("Saved: www/ffr_hist_animated.gif")
