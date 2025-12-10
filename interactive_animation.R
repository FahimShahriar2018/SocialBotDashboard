# interactive_animation.R ----
# All Interactive Story logic in one place

interactive_story_server <- function(input, output, session, df) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(shiny)
  
  # ---- Helpers / model ----
  
  numeric_features <- c(
    "follower_follow_rate",
    "post_rate",
    "ave_attitudes",
    "word_ave",
    "ave_emotionnum",
    "urank"
  )
  
  # Binary flag for the model
  df_model <- df %>%
    mutate(is_bot_flag = ifelse(is_bot == "Bot", 1L, 0L))
  
  model_logit <- glm(
    is_bot_flag ~ follower_follow_rate + post_rate +
      ave_attitudes + word_ave + ave_emotionnum,
    data = df_model,
    family = binomial()
  )
  
  coefs <- summary(model_logit)$coef
  feat_imp <- data.frame(
    term = rownames(coefs)[-1],
    importance = abs(coefs[-1, "Estimate"])
  )
  
  # ---- 1. Animated FFR Story ----
  
  output$interactive_story_plot <- renderPlotly({
    df_plot <- df %>%
      mutate(
        account_type = factor(as.character(is_bot),
                              levels = c("Human", "Bot")),
        rank_group = ifelse(
          is.na(rank_group),
          as.character(cut(
            urank,
            breaks = 3,
            labels = c("Low", "Medium", "High")
          )),
          as.character(rank_group)
        )
      ) %>%
      tidyr::crossing(ffr_step = c(1, 2, 3)) %>%
      mutate(
        follower_rate_step = factor(
          paste0("FFR = ", ffr_step),
          levels = c("FFR = 1", "FFR = 2", "FFR = 3")
        ),
        follower_rate_anim = follower_follow_rate * ffr_step
      )
    
    plot_ly(
      data  = df_plot,
      x     = ~follower_rate_anim,
      y     = ~ave_attitudes,
      frame = ~follower_rate_step,
      color = ~account_type,
      colors = c("Human" = "green", "Bot" = "red"),
      symbol = ~rank_group,
      symbols = c("circle", "square", "diamond"),
      type  = "scatter",
      mode  = "markers",
      hoverinfo = "text",
      text = ~paste(
        "Account type:", account_type,
        "<br>Follower/Following:", round(follower_follow_rate, 2),
        "<br>Scenario FFR:", round(follower_rate_anim, 2),
        "<br>Avg likes:", round(ave_attitudes, 2),
        "<br>Rank group:", rank_group,
        "<br>FFR step:", follower_rate_step
      )
    ) %>%
      layout(
        title = "Animated Story: Engagement vs Follower/Following",
        xaxis = list(title = "Follower/Following rate (scenario)"),
        yaxis = list(title = "Average likes (ave_attitudes)"),
        legend = list(title = list(text = "Type / Rank"))
      ) %>%
      animation_slider(currentvalue = list(prefix = "Follower/Following step: ")) %>%
      animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
  })
  
  
  # ---- 2. Decision Boundary Explorer ----
  
  output$decision_boundary_plot <- renderPlotly({
    req(input$db_x, input$db_y)
    xvar <- input$db_x
    yvar <- input$db_y
    
    x_seq <- seq(min(df[[xvar]], na.rm = TRUE),
                 max(df[[xvar]], na.rm = TRUE),
                 length.out = 80)
    y_seq <- seq(min(df[[yvar]], na.rm = TRUE),
                 max(df[[yvar]], na.rm = TRUE),
                 length.out = 80)
    
    grid <- expand.grid(x = x_seq, y = y_seq)
    names(grid) <- c(xvar, yvar)
    
    other_vars <- setdiff(
      c("follower_follow_rate", "post_rate",
        "ave_attitudes", "word_ave", "ave_emotionnum"),
      c(xvar, yvar)
    )
    for (v in other_vars) {
      grid[[v]] <- mean(df[[v]], na.rm = TRUE)
    }
    
    grid$prob_bot <- predict(model_logit, newdata = grid, type = "response")
    
    df2 <- df %>%
      mutate(
        account_type = factor(as.character(is_bot),
                              levels = c("Human", "Bot"))
      )
    
    g <- ggplot() +
      geom_raster(
        data = grid,
        aes_string(x = xvar, y = yvar, fill = "prob_bot"),
        alpha = 0.6
      ) +
      scale_fill_gradientn(
        colours = c("white", "yellow", "red"),
        limits = c(0, 1),
        name = "P(Bot)"
      ) +
      geom_point(
        data = df2,
        aes_string(x = xvar, y = yvar, color = "account_type"),
        alpha = 0.8,
        size = 1.8
      ) +
      scale_color_manual(
        values = c("Human" = "green", "Bot" = "red"),
        name = "Account type"
      ) +
      theme_minimal() +
      labs(
        title = "Decision Boundary Explorer",
        x = xvar,
        y = yvar
      )
    
    ggplotly(g, tooltip = c("x", "y", "fill", "colour"))
  })
  
  
  # ---- 3. Clustering Explorer ----
  
  output$cluster_plot <- renderPlotly({
    req(input$cl_x, input$cl_y, input$k_clusters)
    xvar <- input$cl_x
    yvar <- input$cl_y
    
    df_na <- df[, c(xvar, yvar)]
    ok    <- stats::complete.cases(df_na)
    
    if (sum(ok) < input$k_clusters) {
      return(NULL)
    }
    
    km <- kmeans(scale(df_na[ok, , drop = FALSE]), centers = input$k_clusters)
    
    df_cl <- df[ok, ]
    df_cl$cluster <- factor(km$cluster)
    df_cl$account_type <- factor(as.character(df_cl$is_bot),
                                 levels = c("Human", "Bot"))
    
    plot_ly(
      x     = df_cl[[xvar]],
      y     = df_cl[[yvar]],
      type  = "scatter",
      mode  = "markers",
      color = df_cl$cluster,
      symbol = df_cl$account_type,
      symbols = c("circle", "x"),
      hoverinfo = "text",
      text = paste(
        "Cluster:", df_cl$cluster,
        "<br>Account type:", df_cl$account_type,
        "<br>", xvar, ": ", round(df_cl[[xvar]], 2),
        "<br>", yvar, ": ", round(df_cl[[yvar]], 2)
      )
    ) %>%
      layout(
        title = "Clustering Explorer (k-means)",
        xaxis = list(title = xvar),
        yaxis = list(title = yvar)
      )
  })
  
  
  # ---- 4. Bot Detection Sandbox: animation state ----
  
  auto_play <- reactiveVal(FALSE)    # playing or paused
  auto_idx  <- reactiveVal(1L)       # which account index
  
  # Reset index whenever we start playing
  observeEvent(input$sb_play_pause, {
    auto_play(!auto_play())
    if (!isTRUE(auto_play())) {
      return()  # just paused
    }
    auto_idx(1L)  # just turned ON -> restart
  })
  
  # Animation loop: step through accounts, updating sliders
  observe({
    req(nrow(df) > 0)
    req(input$sb_speed)
    
    # always schedule next tick
    invalidateLater(input$sb_speed, session)
    
    # if paused, do nothing on this tick
    if (!isTRUE(auto_play())) {
      return()
    }
    
    i <- auto_idx()
    if (i > nrow(df)) {
      i <- 1L
    }
    
    row <- df[i, ]
    
    updateSliderInput(session, "sb_ffr",
                      value = if (!is.na(row$follower_follow_rate))
                                row$follower_follow_rate else input$sb_ffr)
    updateSliderInput(session, "sb_post",
                      value = if (!is.na(row$post_rate))
                                row$post_rate else input$sb_post)
    updateSliderInput(session, "sb_likes",
                      value = if (!is.na(row$ave_attitudes))
                                row$ave_attitudes else input$sb_likes)
    updateSliderInput(session, "sb_words",
                      value = if (!is.na(row$word_ave))
                                row$word_ave else input$sb_words)
    updateSliderInput(session, "sb_emotion",
                      value = if (!is.na(row$ave_emotionnum))
                                row$ave_emotionnum else input$sb_emotion)
    
    auto_idx(i + 1L)
  })
  
  
  # Text: show probability
  output$sandbox_prob <- renderText({
    scenario <- data.frame(
      follower_follow_rate = input$sb_ffr,
      post_rate            = input$sb_post,
      ave_attitudes        = input$sb_likes,
      word_ave             = input$sb_words,
      ave_emotionnum       = input$sb_emotion
    )
    
    p_bot <- predict(model_logit, newdata = scenario, type = "response")
    paste0("Estimated Bot Probability: ", sprintf("%.1f%%", 100 * p_bot))
  })
  
  
  # Plot: Parallel Coordinates of all accounts + scenario line
  output$sandbox_point_plot <- renderPlotly({
    # scenario values from sliders
    scenario <- data.frame(
      follower_follow_rate = input$sb_ffr,
      post_rate            = input$sb_post,
      ave_attitudes        = input$sb_likes,
      word_ave             = input$sb_words,
      ave_emotionnum       = input$sb_emotion
    )
    p_bot <- predict(model_logit, newdata = scenario, type = "response")
    
    # all accounts with numeric color for Human/Bot
    df_pc <- df %>%
      mutate(
        account_type = factor(as.character(is_bot),
                              levels = c("Human", "Bot")),
        color_val = ifelse(account_type == "Bot", 1, 0)
      )
    
    # dimensions for real accounts
    dims <- list(
      list(
        range  = range(df_pc$follower_follow_rate, na.rm = TRUE),
        label  = "Follower/Following rate",
        values = df_pc$follower_follow_rate
      ),
      list(
        range  = range(df_pc$post_rate, na.rm = TRUE),
        label  = "Post rate",
        values = df_pc$post_rate
      ),
      list(
        range  = range(df_pc$ave_attitudes, na.rm = TRUE),
        label  = "Average likes",
        values = df_pc$ave_attitudes
      ),
      list(
        range  = range(df_pc$word_ave, na.rm = TRUE),
        label  = "Avg words per post",
        values = df_pc$word_ave
      ),
      list(
        range  = range(df_pc$ave_emotionnum, na.rm = TRUE),
        label  = "Avg emotion tokens",
        values = df_pc$ave_emotionnum
      )
    )
    
    # dimensions for scenario line
    dims_scenario <- list(
      list(
        range  = range(df_pc$follower_follow_rate, na.rm = TRUE),
        label  = "Follower/Following rate",
        values = scenario$follower_follow_rate
      ),
      list(
        range  = range(df_pc$post_rate, na.rm = TRUE),
        label  = "Post rate",
        values = scenario$post_rate
      ),
      list(
        range  = range(df_pc$ave_attitudes, na.rm = TRUE),
        label  = "Average likes",
        values = scenario$ave_attitudes
      ),
      list(
        range  = range(df_pc$word_ave, na.rm = TRUE),
        label  = "Avg words per post",
        values = scenario$word_ave
      ),
      list(
        range  = range(df_pc$ave_emotionnum, na.rm = TRUE),
        label  = "Avg emotion tokens",
        values = scenario$ave_emotionnum
      )
    )
    
    # base: all accounts, colored by Human/Bot
    p <- plot_ly(
      type = "parcoords",
      line = list(
        color      = df_pc$color_val,
        colorscale = list(
          list(0, "green"),
          list(1, "red")
        ),
        showscale = FALSE
      ),
      dimensions = dims
    )
    
    # scenario line: thick black
    p <- p %>%
      add_trace(
        type = "parcoords",
        line = list(
          color = "black",
          width = 7
        ),
        dimensions = dims_scenario,
        name = paste0(
          "Scenario (P(Bot) = ",
          sprintf("%.1f%%", 100 * p_bot), ")"
        ),
        inherit = FALSE
      )
    
    p %>%
      layout(
        title = "Bot Detection Sandbox: Parallel Coordinates",
        margin = list(l = 40, r = 40, t = 60, b = 40)
      )
  })
  
  
  # ---- 5. Feature Importance Viewer ----
  
  output$feature_importance_plot <- renderPlotly({
    plot_ly(
      data = feat_imp,
      x    = ~reorder(term, importance),
      y    = ~importance,
      type = "bar"
    ) %>%
      layout(
        title = "Feature Importance (Logistic Regression)",
        xaxis = list(title = "Feature"),
        yaxis = list(title = "|Coefficient|"),
        margin = list(b = 100)
      )
  })
  
  invisible(NULL)
}
