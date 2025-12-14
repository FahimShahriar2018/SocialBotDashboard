# server.R ----

library(ggplot2)
library(plotly)
library(dplyr)

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
  
  # NEW: Histogram of follower_follow_rate by account type
  output$ffr_hist <- renderPlot({
    ggplot(df, aes(x = follower_follow_rate, fill = is_bot)) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      scale_fill_manual(values = c("Human" = "skyblue", "Bot" = "red")) +
      theme_minimal(base_size = 12) +
      labs(
        title = "Histogram of Follower/Following Rate by Account Type",
        x = "Follower/Following rate",
        y = "Count",
        fill = "Account type"
      )
  })
  
  # SVG download for the histogram
  output$download_ffr_hist <- downloadHandler(
    filename = function() "hist_follower_follow_rate.svg",
    content = function(file) {
      g <- ggplot(df, aes(x = follower_follow_rate, fill = is_bot)) +
        geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
        scale_fill_manual(values = c("Human" = "skyblue", "Bot" = "red")) +
        theme_minimal(base_size = 12) +
        labs(
          title = "Histogram of Follower/Following Rate by Account Type",
          x = "Follower/Following rate",
          y = "Count",
          fill = "Account type"
        )
      ggsave(file, plot = g, device = "svg", width = 6, height = 4)
    }
  )
  
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


  # ---- PCA SECTION ----

  # ---- Pairwise PCA Plot 1: Follower_Follow_Rate vs cvar_url ----
  output$pca_pair1 <- renderPlot({
    ggplot(df, aes(x = follower_follow_rate, y = cvar_url, color = is_bot)) +
      geom_point(size = 2, alpha = 0.8) +
      theme_minimal(base_size = 12) +
      scale_color_manual(values = c("Human" = "steelblue", "Bot" = "red")) +
      labs(
        title = "Follower–Follow Rate vs cvar_url",
        x = "Follower–Follow Rate",
        y = "cvar_url",
        color = "User Type"
      )
  })

  output$download_pca_pair1 <- downloadHandler(
    filename = function() "PCA_pair_follower_vs_cvar_url.svg",
    content = function(file) {
      p <- ggplot(df, aes(x = follower_follow_rate, y = cvar_url, color = is_bot)) +
        geom_point(size = 2, alpha = 0.8) +
        theme_minimal(base_size = 12) +
        scale_color_manual(values = c("Human" = "steelblue", "Bot" = "red")) +
        labs(title = "Follower–Follow Rate vs cvar_url")
      ggsave(file, plot = p, device = "svg", width = 6, height = 4)
    }
  )


  # ---- Pairwise PCA Plot 2: Follower_Follow_Rate vs uRank ----
  output$pca_pair2 <- renderPlot({
    ggplot(df, aes(x = follower_follow_rate, y = urank, color = is_bot)) +
      geom_point(size = 2, alpha = 0.8) +
      theme_minimal(base_size = 12) +
      scale_color_manual(values = c("Human" = "steelblue", "Bot" = "red")) +
      labs(
        title = "Follower–Follow Rate vs uRank",
        x = "Follower–Follow Rate",
        y = "uRank",
        color = "User Type"
      )
  })

  output$download_pca_pair2 <- downloadHandler(
    filename = function() "PCA_pair_follower_vs_urank.svg",
    content = function(file) {
      p <- ggplot(df, aes(x = follower_follow_rate, y = urank, color = is_bot)) +
        geom_point(size = 2, alpha = 0.8) +
        theme_minimal(base_size = 12)
      ggsave(file, plot = p, device = "svg", width = 6, height = 4)
    }
  )


  # ---- Whole PCA (PC1 vs PC2) ----
  output$pca_whole <- renderPlot({
    
    # numeric columns
    numeric_df <- df %>% select(where(is.numeric))
    feature_cols <- setdiff(colnames(numeric_df), "is_bot")
    X <- numeric_df[, feature_cols, drop = FALSE]

    # remove zero variance
    zero_var <- sapply(X, function(v) sd(v, na.rm = TRUE) == 0)
    X <- X[, !zero_var, drop = FALSE]

    # remove incomplete rows
    good_rows <- complete.cases(X)
    X_complete <- X[good_rows, ]
    bot_labels <- df$is_bot[good_rows]

    # PCA
    pca_res <- prcomp(X_complete, center = TRUE, scale. = TRUE)
    pca_df <- as.data.frame(pca_res$x[, 1:2])
    pca_df$is_bot <- bot_labels

    ggplot(pca_df, aes(x = PC1, y = PC2, color = is_bot)) +
      geom_point(size = 2, alpha = 0.8) +
      theme_minimal(base_size = 12) +
      scale_color_manual(values = c("Human" = "steelblue", "Bot" = "red")) +
      labs(
        title = "PCA Scatter Plot (PC1 vs PC2)",
        x = "PC1",
        y = "PC2",
        color = "User Type"
      )
  })

  output$download_pca_whole <- downloadHandler(
    filename = function() "PCA_whole_scatter.svg",
    content = function(file) {
      numeric_df <- df %>% select(where(is.numeric))
      feature_cols <- setdiff(colnames(numeric_df), "is_bot")
      X <- numeric_df[, feature_cols, drop = FALSE]

      zero_var <- sapply(X, function(v) sd(v, na.rm = TRUE) == 0)
      X <- X[, !zero_var, drop = FALSE]

      good_rows <- complete.cases(X)
      X_complete <- X[good_rows, ]
      bot_labels <- df$is_bot[good_rows]

      pca_res <- prcomp(X_complete, center = TRUE, scale. = TRUE)
      pca_df <- as.data.frame(pca_res$x[, 1:2])
      pca_df$is_bot <- bot_labels

      p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = is_bot)) +
        geom_point(size = 2, alpha = 0.8) +
        theme_minimal(base_size = 12)

      ggsave(file, plot = p, device = "svg", width = 6, height = 4)
    }
  )

  
  # ---- Interactive Story (delegated to module) ----
  interactive_story_server(input, output, session, df)
  
  
  # ---- AI-generated Graph: new correlation heatmap ----
  
  output$ai_corr_heatmap <- renderPlot({
    # 1. numeric columns only (like Correlation_heatmap.R)
    numeric_df <- df %>%
      select(where(is.numeric))
    
    # 2. correlation matrix
    corr_mat <- cor(numeric_df, use = "complete.obs")
    
    # 3. long format (similar to melt)
    corr_long <- as.data.frame(as.table(corr_mat))
    colnames(corr_long) <- c("Var1", "Var2", "Correlation")
    
    # 4. heatmap
    ggplot(corr_long, aes(x = Var2, y = Var1, fill = Correlation)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "blue",
        high = "red",
        mid = "white",
        midpoint = 0,
        limits = c(-1, 1),
        name = "Correlation"
      ) +
      coord_fixed() +
      theme_minimal(base_size = 9) +
      theme(
        axis.text.x = element_text(
          angle = 45, hjust = 1, vjust = 1, size = 6
        ),
        axis.text.y = element_text(size = 6),
        panel.grid = element_blank()
      ) +
      labs(
        title = "Correlation Heatmap of Numeric Features",
        x = NULL,
        y = NULL
      )
  })

  # ---- Report Download ----
  
  output$download_report <- downloadHandler(
    filename = function() {
      "SocialBot_analysis_report.pdf"
    },
    content = function(file) {
      temp <- file.path(tempdir(), "SocialBot_analysis_report.pdf")
      rmarkdown::render("SocialBot_report.Rmd", output_file = temp)
      file.copy(temp, file, overwrite = TRUE)
    }
  )

  # ---- Download: Existing Project Report PDF ----
  output$download_project_report <- downloadHandler(
    filename = function() {
      "DV_SocialBot_project_report_group8.pdf"
    },
    content = function(file) {
      file.copy("DV_SocialBot_project_report_group8.pdf", file, overwrite = TRUE)
    }
  )

}
