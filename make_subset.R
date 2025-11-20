library(readxl)
library(dplyr)

# 1) Load the full SocialBot dataset
df <- read_excel("SocialBot.xlsx")

# 2) Keep only the columns needed for the AI graph
subset_df <- df %>%
  select(
    is_bot,
    follower_follow_rate,
    post_rate,
    ave_comment,
    ave_repost,
    ave_attitudes,
    ave_url,
    word_ave,
    ave_emotionnum
  )

# 3) Save to CSV
write.csv(subset_df, "socialbot_subset.csv", row.names = FALSE)
