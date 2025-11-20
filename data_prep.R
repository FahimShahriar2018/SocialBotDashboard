# data_prep.R ----
# Loads SocialBot.xlsx and prepares all data objects used in the app.

df <- read_excel("SocialBot.xlsx") %>%
  mutate(
    is_bot = factor(is_bot, levels = c(0, 1), labels = c("Human", "Bot")),
    rank_group = cut(urank, breaks = 3, labels = c("Low", "Medium", "High"))
  )

# Numeric-only data for correlation (drop factor is_bot)
num_df <- df %>%
  select(-is_bot) %>% 
  mutate(across(everything(), ~as.numeric(.)))

cor_mat <- cor(num_df, use = "pairwise.complete.obs")
cor_df  <- as.data.frame(as.table(cor_mat))
colnames(cor_df) <- c("Var1", "Var2", "Correlation")

# Summary for engagement bar chart
engagement_summary <- df %>%
  group_by(is_bot) %>%
  summarise(
    mean_comment   = mean(ave_comment, na.rm = TRUE),
    mean_repost    = mean(ave_repost, na.rm = TRUE),
    mean_attitudes = mean(ave_attitudes, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(metric,
                    "mean_comment"   = "Comments",
                    "mean_repost"    = "Reposts",
                    "mean_attitudes" = "Likes")
  )

# Long data for URL/textlength boxplots
url_text_long <- df %>%
  select(is_bot, ave_url, cvar_url, cvar_textlength) %>%
  pivot_longer(
    cols = c(ave_url, cvar_url, cvar_textlength),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = recode(measure,
                     "ave_url"         = "Average URL use",
                     "cvar_url"        = "URL variability",
                     "cvar_textlength" = "Text length variability")
  )
