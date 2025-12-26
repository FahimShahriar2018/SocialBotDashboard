# server.R ----

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

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
    ggplot(engagement_summary, aes(x = metric, y = value, fill = is_bot)) +
      geom_col(position = "dodge") +
      labs(
        x = "Engagement metric",
        y = "Mean value",
        fill = "Account type",
        title = "Average Engagement (Comments, Reposts, Likes)"
      ) +
      theme_minimal()
  })

  # Histogram
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

output$download_ffr_hist <- downloadHandler(
  filename = function() paste0("hist_follower_follow_rate_current_", Sys.Date(), ".svg"),
  content = function(file) {

    # the animated histogram style
    x_min <- 0
    x_max <- 1
    binwidth <- 0.025

    df2 <- df %>%
      mutate(follower_follow_rate = as.numeric(follower_follow_rate)) %>%
      filter(!is.na(follower_follow_rate)) %>%
      mutate(follower_follow_rate = pmax(follower_follow_rate, 0)) %>%
      filter(follower_follow_rate >= x_min, follower_follow_rate <= x_max)

    breaks <- seq(x_min, x_max + binwidth, by = binwidth)

    hist_df <- df2 %>%
      mutate(bin_left = cut(
        follower_follow_rate,
        breaks = breaks,
        include.lowest = TRUE,
        right = FALSE
      )) %>%
      count(is_bot, bin_left, name = "n") %>%
      tidyr::complete(is_bot, bin_left, fill = list(n = 0)) %>%
      mutate(
        bin_start = as.numeric(gsub("\\[|\\(|,.*", "", as.character(bin_left))),
        bin_mid   = bin_start + binwidth / 2
      ) %>%
      arrange(is_bot, bin_mid)

    g <- ggplot(hist_df, aes(x = bin_mid, y = n, fill = is_bot)) +
      geom_col(
        width = binwidth * 0.98,
        position = "identity",
        alpha = 0.60
      ) +
      scale_fill_manual(values = c("Human" = "lightblue", "Bot" = "red")) +
      coord_cartesian(xlim = c(x_min, x_max)) +
      scale_x_continuous(
        breaks = c(0, 0.25, 0.50, 0.75, 1.00),
        labels = sprintf("%.2f", c(0, 0.25, 0.50, 0.75, 1.00))
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(
        title = "Histogram of Follower/Following Rate by Account Type",
        x = "Follower/Following rate",
        y = "Count",
        fill = "Account type"
      )

    ggsave(file, plot = g, device = "svg", width = 7, height = 4.5)
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

  # ---- PCA Section ----

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

  output$pca_whole <- renderPlot({
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
    colnames(pca_df) <- c("PC1", "PC2")
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

  # ---- Interactive Story ----
  interactive_story_server(input, output, session, df)

  # ---- AI-generated Graph ----
  output$ai_corr_heatmap <- renderPlot({
    numeric_df <- df %>% select(where(is.numeric))
    corr_mat <- cor(numeric_df, use = "complete.obs")

    corr_long <- as.data.frame(as.table(corr_mat))
    colnames(corr_long) <- c("Var1", "Var2", "Correlation")

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
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        panel.grid = element_blank()
      ) +
      labs(
        title = "Correlation Heatmap of Numeric Features",
        x = NULL,
        y = NULL
      )
  })

  # ---- Download: Existing Project Report PDF ----
  output$download_project_report <- downloadHandler(
    filename = function() "DV_SocialBot_project_report_group8.pdf",
    content = function(file) {
      file.copy("DV_SocialBot_project_report_group8.pdf", file, overwrite = TRUE)
    }
  )

  # ------------------- Analysis Report Generation + Download -------------------

  report_file <- reactiveVal(NULL)

  observeEvent(input$build_report, {

    showModal(
      modalDialog(
        title = "Generating Analysis Report…",
        "Please wait. The PDF is being created.",
        tags$br(),
        tags$em("Working..."),
        footer = NULL,
        easyClose = FALSE
      )
    )

    tryCatch({
      tmpdir <- tempdir()
      out_pdf <- file.path(tmpdir, "SocialBot_analysis_report.pdf")

      rmarkdown::render(
        input = "SocialBot_report.Rmd",
        output_file = out_pdf,
        quiet = TRUE
      )

      report_file(out_pdf)

      removeModal()
      showNotification("Report generated! Now click 'Download Analysis Report (PDF)'.",
                       type = "message", duration = 5)

    }, error = function(e) {
      removeModal()
      showNotification(paste("Report generation failed:", e$message),
                       type = "error", duration = 8)
    })
  })

  output$download_report <- downloadHandler(
    filename = function() "SocialBot_analysis_report.pdf",
    content = function(file) {
      req(report_file())
      file.copy(report_file(), file, overwrite = TRUE)
    }
  )

}
