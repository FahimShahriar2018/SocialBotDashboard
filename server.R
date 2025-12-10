# server.R ----
# Uses objects from data_prep.R: df, cor_df, engagement_summary, url_text_long

library(ggplot2)
library(plotly)

# IMPORTANT: load data & interactive module BEFORE defining server()
source("data_prep.R")
source("interactive_animation.R")

server <- function(input, output, session) {
  
  # ---- Overview ----
  
  output$data_summary <- renderPrint({
    list(
      n_rows = nrow(df),
      n_cols = ncol(df),
      class_distribution = table(df$is_bot)
    )
  })
  
  output$class_bar <- renderPlot({
    ggplot(df, aes(x = is_bot)) +
      geom_bar(fill = "steelblue") +
      labs(
        x = "Account type",
        y = "Count",
        title = "Number of Human vs Bot Accounts"
      ) +
      theme_minimal()
  })
  
  output$corr_heatmap <- renderPlotly({
    g <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "darkred",
        mid = "white",
        high = "darkblue",
        midpoint = 0
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      labs(
        x = "",
        y = "",
        title = "Correlation Heatmap of Numeric Features"
      )
    
    ggplotly(g, tooltip = c("x", "y", "fill"))
  })
  
  
  # SVG downloads for Overview plots
  output$download_class_bar <- downloadHandler(
    filename = function() "class_distribution.svg",
    content = function(file) {
      g <- ggplot(df, aes(x = is_bot)) +
        geom_bar(fill = "steelblue") +
        labs(
          x = "Account type",
          y = "Count",
          title = "Number of Human vs Bot Accounts"
        ) +
        theme_minimal()
      ggsave(file, plot = g, device = "svg", width = 6, height = 4)
    }
  )
  
  output$download_corr_heatmap <- downloadHandler(
    filename = function() "correlation_heatmap.svg",
    content = function(file) {
      g <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(
          low = "darkred",
          mid = "white",
          high = "darkblue",
          midpoint = 0
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) +
        labs(
          x = "",
          y = "",
          title = "Correlation Heatmap of Numeric Features"
        )
      ggsave(file, plot = g, device = "svg", width = 7, height = 5.5)
    }
  )
  
  # ---- Profile & Popularity ----
  
  output$box_follow_ratio <- renderPlot({
    ggplot(df, aes(x = is_bot, y = follower_follow_rate, fill = is_bot)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        x = "Account type",
        y = "Follower/Following rate",
        title = "Follower/Following Rate by Account Type"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$box_urank <- renderPlot({
    ggplot(df, aes(x = is_bot, y = urank, fill = is_bot)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        x = "Account type",
        y = "User rank (urank)",
        title = "User Rank by Account Type"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # SVG downloads for Profile & Popularity
  output$download_box_follow_ratio <- downloadHandler(
    filename = function() "follower_follow_ratio.svg",
    content = function(file) {
      g <- ggplot(df, aes(x = is_bot, y = follower_follow_rate, fill = is_bot)) +
        geom_boxplot(alpha = 0.7) +
        labs(
          x = "Account type",
          y = "Follower/Following rate",
          title = "Follower/Following Rate by Account Type"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      ggsave(file, plot = g, device = "svg", width = 6, height = 4)
    }
  )
  
  output$download_box_urank <- downloadHandler(
    filename = function() "urank_by_type.svg",
    content = function(file) {
      g <- ggplot(df, aes(x = is_bot, y = urank, fill = is_bot)) +
        geom_boxplot(alpha = 0.7) +
        labs(
          x = "Account type",
          y = "User rank (urank)",
          title = "User Rank by Account Type"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      ggsave(file, plot = g, device = "svg", width = 6, height = 4)
    }
  )
  
  # ---- Activity & Engagement ----
  
  output$postrate_violin <- renderPlot({
    ggplot(df, aes(x = is_bot, y = post_rate, fill = is_bot)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
      labs(
        x = "Account type",
        y = "Post rate",
        title = "Posting Rate Distribution by Account Type"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$engagement_bars <- renderPlot({
    ggplot(engagement_summary,
           aes(x = metric, y = value, fill = is_bot)) +
      geom_col(position = "dodge") +
      labs(
        x = "Engagement metric",
        y = "Mean value",
        fill = "Account type",
        title = "Average Engagement (Comments, Reposts, Likes)"
      ) +
      theme_minimal()
  })
  
  # SVG downloads for Activity & Engagement
  output$download_postrate_violin <- downloadHandler(
    filename = function() "posting_rate.svg",
    content = function(file) {
      g <- ggplot(df, aes(x = is_bot, y = post_rate, fill = is_bot)) +
        geom_violin(trim = FALSE, alpha = 0.7) +
        geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
        labs(
          x = "Account type",
          y = "Post rate",
          title = "Posting Rate Distribution by Account Type"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      ggsave(file, plot = g, device = "svg", width = 6, height = 4)
    }
  )
  
  output$download_engagement_bars <- downloadHandler(
    filename = function() "average_engagement.svg",
    content = function(file) {
      g <- ggplot(engagement_summary,
                  aes(x = metric, y = value, fill = is_bot)) +
        geom_col(position = "dodge") +
        labs(
          x = "Engagement metric",
          y = "Mean value",
          fill = "Account type",
          title = "Average Engagement (Comments, Reposts, Likes)"
        ) +
        theme_minimal()
      ggsave(file, plot = g, device = "svg", width = 6, height = 4)
    }
  )
  
  # Animated GIF (created earlier via gganimate)
  output$animated_scatter <- renderImage({
    filename <- file.path("www", "animated_scatter.gif")
    list(
      src = filename,
      contentType = "image/gif",
      width = "100%",
      height = "auto"
    )
  }, deleteFile = FALSE)
  
  # ---- Content & Timing ----
  
  output$url_textlength_box <- renderPlot({
    ggplot(url_text_long, aes(x = is_bot, y = value, fill = is_bot)) +
      geom_boxplot(alpha = 0.7) +
      facet_wrap(~ measure, scales = "free_y") +
      labs(
        x = "Account type",
        y = "Value",
        title = "URL Use & Text Length Variability"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$word_emotion_scatter <- renderPlot({
    ggplot(df, aes(x = word_ave, y = ave_emotionnum, color = is_bot)) +
      geom_point(alpha = 0.8) +
      labs(
        x = "Average words per post",
        y = "Average emotion tokens",
        color = "Account type",
        title = "Words vs Emotion Tokens by Account Type"
      ) +
      theme_minimal()
  })
  
  
  # SVG downloads for Content & Timing
  output$download_url_textlength_box <- downloadHandler(
    filename = function() "url_text_variability.svg",
    content = function(file) {
      g <- ggplot(url_text_long, aes(x = is_bot, y = value, fill = is_bot)) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap(~ measure, scales = "free_y") +
        labs(
          x = "Account type",
          y = "Value",
          title = "URL Use & Text Length Variability"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      ggsave(file, plot = g, device = "svg", width = 7, height = 5)
    }
  )
  
  output$download_word_emotion_scatter <- downloadHandler(
    filename = function() "words_vs_emotion.svg",
    content = function(file) {
      g <- ggplot(df, aes(x = word_ave, y = ave_emotionnum, color = is_bot)) +
        geom_point(alpha = 0.8) +
        labs(
          x = "Average words per post",
          y = "Average emotion tokens",
          color = "Account type",
          title = "Words vs Emotion Tokens by Account Type"
        ) +
        theme_minimal()
      ggsave(file, plot = g, device = "svg", width = 6, height = 4)
    }
  )
  
  # ---- Interactive Story (delegated) ----
  interactive_story_server(input, output, session, df)
  
  
  # ---- AI-generated Graph ----
  
  output$ai_graph <- renderImage({
    filename <- file.path("www", "ai_graph.png")
    list(
      src = filename,
      contentType = "image/png",
      width = "100%",
      height = "auto"
    )
  }, deleteFile = FALSE)
  
  # ---- Report Download ----
  
  output$download_report <- downloadHandler(
    filename = function() {
      "SocialBot_report.pdf"
    },
    content = function(file) {
      temp <- file.path(tempdir(), "SocialBot_report.pdf")
      rmarkdown::render("SocialBot_report.Rmd", output_file = temp)
      file.copy(temp, file, overwrite = TRUE)
    }
  )
}
