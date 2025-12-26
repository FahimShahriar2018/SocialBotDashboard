library(readxl)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)

# Load data
df <- read_excel("SocialBot.xlsx") %>%
  mutate(
    is_bot = factor(is_bot, levels = c(0,1), labels = c("Human","Bot")),
    rank_group = cut(urank, breaks = 3, labels = c("Low","Medium","High"))
  )

# Create animation
p <- ggplot(df, aes(
  x = follower_follow_rate,
  y = ave_attitudes,
  color = is_bot
)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  transition_states(rank_group) +
  labs(
    title = "Rank group: {closest_state}",
    x = "Follower/Following Rate",
    y = "Average Likes (Attitudes)",
    color = "Type"
  )

anim <- animate(
  p,
  renderer = gifski_renderer(),
  width = 600,
  height = 400,
  duration = 4,
  fps = 15
)

# check/create www folder exists
if (!dir.exists("www")) dir.create("www")

# Save GIF
anim_save("www/animated_scatter.gif", animation = anim)



