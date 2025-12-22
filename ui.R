# ui.R ----
library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)

# numeric features for interactive story controls
numeric_features <- c(
  "follower_follow_rate",
  "post_rate",
  "ave_attitudes",
  "word_ave",
  "ave_emotionnum",
  "urank"
)

ui <- dashboardPage(

  dashboardHeader(title = "SocialBot Dashboard"),

  # ---------------- LEFT SIDEBAR ----------------
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Profile & Popularity", tabName = "profile", icon = icon("user")),
      menuItem("Activity & Engagement", tabName = "activity", icon = icon("bolt")),
      menuItem("Content & Timing", tabName = "content", icon = icon("clock")),
      menuItem("Principal Component Analysis (PCA)", tabName = "pca", icon = icon("chart-line")),
      menuItem("Interactive Story", tabName = "story", icon = icon("film")),
      menuItem("AI-generated Graph", tabName = "ai", icon = icon("robot")),
      menuItem("Report / Download", tabName = "report", icon = icon("download"))
    )
  ),

  # ---------------- MAIN BODY ----------------
  dashboardBody(

    tags$head(tags$style(HTML("
      .section-card {
        background: #ffffff;
        border-radius: 12px;
        padding: 18px 22px;
        margin-bottom: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
      }
      .section-title {
        font-size: 18px;
        font-weight: 600;
        margin-bottom: 10px;
        color: #2c3e50;
      }
      .section-subtitle {
        font-size: 13px;
        color: #7f8c8d;
        margin-bottom: 12px;
      }
    "))),


    tabItems(

      # ------------- OVERVIEW -------------
      tabItem(tabName = "overview",
        fluidRow(
          column(
            4,
            div(class = "section-card",
              h4("Dataset Overview", class = "section-title"),
              verbatimTextOutput("data_summary"),
              hr(),
              tags$small("Note: is_bot is labelled as 'Human' or 'Bot'.")
            )
          ),
          column(
            8,
            div(class = "section-card",
              h4("Class Distribution", class = "section-title"),
              plotOutput("class_bar", height = "260px"),
              downloadButton("download_class_bar", "Download SVG")
            )
            # div(class = "section-card",
            #   h4("Feature Correlations", class = "section-title"),
            #   plotlyOutput("corr_heatmap", height = "420px"),
            #   downloadButton("download_corr_heatmap", "Download SVG")
            # )
          )
        )
      ),

      # ------------- PROFILE & POPULARITY -------------
      tabItem(tabName = "profile",
        fluidRow(
          column(
            6,
            div(class = "section-card",
              h4("Follower / Following Ratio", class = "section-title"),
              plotOutput("box_follow_ratio", height = "320px"),
              downloadButton("download_box_follow_ratio", "Download SVG")
            )
          ),
          column(
            6,
            div(class = "section-card",
              h4("User Rank (urank)", class = "section-title"),
              plotOutput("box_urank", height = "320px"),
              downloadButton("download_box_urank", "Download SVG")
            )
          )
        )
      ),

      # ------------- ACTIVITY & ENGAGEMENT -------------
      tabItem(tabName = "activity",
                fluidRow(
          column(
            12,
            div(class = "section-card",
              h4("Histogram: Follower/Following Rate by Account Type", class = "section-title"),
              # plotOutput("ffr_hist", height = "320px"),
              tags$img( src = "ffr_hist_animated.gif", style = "width:100%; height:320px; object-fit:contain;" ),
              downloadButton("download_ffr_hist", "Download SVG")
            )
          )
        ),
        fluidRow(
          column(
            6,
            div(class = "section-card",
              h4("Posting Rate", class = "section-title"),
              plotOutput("postrate_violin", height = "320px"),
              downloadButton("download_postrate_violin", "Download SVG")
            )
          ),
          column(
            6,
            div(class = "section-card",
              h4("Average Engagement", class = "section-title"),
              plotOutput("engagement_bars", height = "320px"),
              downloadButton("download_engagement_bars", "Download SVG")
            )
          )
        )
      ),

      # ------------- CONTENT & TIMING -------------
      tabItem(tabName = "content",
        fluidRow(
          column(
            6,
            div(class = "section-card",
              h4("URL & Text Length Behaviour"),
              plotOutput("url_textlength_box", height = "340px"),
              downloadButton("download_url_textlength_box", "Download SVG")
            )
          ),
          column(
            6,
            div(class = "section-card",
              h4("Words vs Emotion Tokens"),
              plotOutput("word_emotion_scatter", height = "340px"),
              downloadButton("download_word_emotion_scatter", "Download SVG")
            )
          )
        )
      ),

      # ------------- PCA TAB -------------
      tabItem(tabName = "pca",
        fluidRow(
          column(
            6,
            div(class = "section-card",
              h4("Follower–Follow Rate vs cvar_url"),
              plotOutput("pca_pair1", height = "340px"),
              downloadButton("download_pca_pair1", "Download SVG")
            )
          ),
          column(
            6,
            div(class = "section-card",
              h4("Follower–Follow Rate vs uRank"),
              plotOutput("pca_pair2", height = "340px"),
              downloadButton("download_pca_pair2", "Download SVG")
            )
          )
        ),
        fluidRow(
          column(
            12,
            div(class = "section-card",
              h4("PCA Scatter Plot (PC1 vs PC2)"),
              plotOutput("pca_whole", height = "420px"),
              downloadButton("download_pca_whole", "Download SVG")
            )
          )
        )
      ),

      # ------------- INTERACTIVE STORY -------------
      tabItem(tabName = "story",
        div(class = "section-card",
          h4("Interactive Story Lab", class = "section-title"),
          tabsetPanel(
            # 1) Animated Story
            tabPanel(
              "Animated Story",
              fluidRow(
                column(
                  8,
                  plotlyOutput("interactive_story_plot", height = "430px")
                ),
                column(
                  4,
                  h5("How to read this story"),
                  tags$ol(
                    tags$li("Each point is an account."),
                    tags$li("X-axis: Scenario follower/following rate."),
                    tags$li("Y-axis: Average likes per post."),
                    tags$li("Color: account type (Human = green, Bot = red)."),
                    tags$li("Animation step changes the follower/following scenario.")
                  )
                )
              )
            ),
            # 2) Decision Boundary
            tabPanel(
              "Decision Boundary",
              fluidRow(
                column(
                  4,
                  h5("Controls"),
                  selectInput(
                    "db_x", "X-axis feature",
                    choices = numeric_features,
                    selected = "follower_follow_rate"
                  ),
                  selectInput(
                    "db_y", "Y-axis feature",
                    choices = numeric_features,
                    selected = "ave_attitudes"
                  ),
                  tags$small("Background colour shows predicted P(Bot). Points are actual accounts.")
                ),
                column(
                  8,
                  plotlyOutput("decision_boundary_plot", height = "430px")
                )
              )
            ),
            # 3) Clustering Explorer
            tabPanel(
              "Clustering Explorer",
              fluidRow(
                column(
                  4,
                  h5("Controls"),
                  selectInput(
                    "cl_x", "X feature",
                    choices = numeric_features,
                    selected = "follower_follow_rate"
                  ),
                  selectInput(
                    "cl_y", "Y feature",
                    choices = numeric_features,
                    selected = "ave_attitudes"
                  ),
                  sliderInput(
                    "k_clusters", "Number of clusters (k)",
                    min = 2, max = 6, value = 3, step = 1
                  ),
                  tags$small("Colour = cluster, Symbol = Human/Bot.")
                ),
                column(
                  8,
                  plotlyOutput("cluster_plot", height = "430px")
                )
              )
            ),
            # 4) Detection Sandbox (Parallel Coordinates + Animation)
            tabPanel(
              "Detection Sandbox",
              fluidRow(
                column(
                  4,
                  h5("Adjust a hypothetical account"),
                  sliderInput("sb_ffr",    "Follower/Following rate", min = 0,  max = 10,  value = 1.5, step = 0.1),
                  sliderInput("sb_post",   "Post rate",               min = 0,  max = 20,  value = 2,   step = 0.1),
                  sliderInput("sb_likes",  "Average likes",           min = 0,  max = 100, value = 10,  step = 1),
                  sliderInput("sb_words",  "Avg words per post",      min = 0,  max = 200, value = 30,  step = 1),
                  sliderInput("sb_emotion","Avg emotion tokens",      min = 0,  max = 50,  value = 5,   step = 0.5),
                  hr(),
                  actionButton("sb_play_pause", "▶ Play / ⏸ Pause animation"),
                  sliderInput("sb_speed", "Animation speed (ms per step)",
                              min = 200, max = 2000, value = 800, step = 100),
                  hr(),
                  strong(textOutput("sandbox_prob"))
                ),
                column(
                  8,
                  plotlyOutput("sandbox_point_plot", height = "430px"),
                  tags$br(),
                  tags$small(
                    "Parallel Coordinates Plot: each line is an account. ",
                    "Vertical axes are Follower/Following rate, Post rate, Average likes, ",
                    "Avg words per post, and Avg emotion tokens. Green = Human, Red = Bot, ",
                    "and the thick black line shows your hypothetical (or animated) account."
                  )
                )
              )
            ),
            # 5) Feature Importance
            tabPanel(
              "Feature Importance",
              fluidRow(
                column(
                  12,
                  plotlyOutput("feature_importance_plot", height = "430px"),
                  tags$small(
                    "Bars show the absolute size of logistic regression coefficients ",
                    "(stronger = more important for distinguishing bots from humans)."
                  )
                )
              )
            )
          )
        )
      ),

      # ------------- AI GRAPH -------------
      tabItem(tabName = "ai",
        div(class = "section-card",
          h4("AI-generated Correlation Heatmap", class = "section-title"),
          plotOutput("ai_corr_heatmap", height = "420px")
        )
      ),

      # ------------- REPORT / DOWNLOAD -------------
      tabItem(tabName = "report",
        fluidRow(
          column(
            6,
            div(class = "section-card",
              h4("Report Download Options"),
              actionButton("build_report", "Generate Analysis Report (PDF)"),
              downloadButton("download_report", "Download Analysis Report (PDF)"),
              br(), br(),
              downloadButton("download_project_report", "Download Project Report (PDF)")
            )
          ),
          # -------- RIGHT: INSTRUCTIONS --------
          column(
            6,
            div(class = "section-card",
              h4("Instructions"),

              tags$ol(
                tags$li(
                  strong("Generate Analysis Report (PDF): "),
                  "Click this button first to create the analysis report dynamically based on the current dataset. ",
                  "A progress dialog will appear while the report is being generated."
                ),
                tags$li(
                  strong("Download Analysis Report (PDF): "),
                  "After generation is complete, click this button to download the generated analysis report."
                ),
                tags$li(
                  strong("Download Project Report (PDF): "),
                  "Downloads the final static project report submitted for evaluation. ",
                  "This file is pre-written and does not change."
                )
              ),

              tags$hr(),

              tags$p(
                strong("Note: "),
                "The analysis report includes dataset overview, visualisations, correlation analysis, PCA results, ",
                "and summary insights comparing bot and human accounts."
              )
            )
          )
        ),
        fluidRow(
          column(
            6,
            div(class = "section-card",
              h4("Summary"),
              tags$ul(
                tags$li("Overview tab provides a summary of the dataset and basic class statistics."),
                tags$li("Profile & Popularity tab addresses RQ1 by comparing follower–following ratios, profile completeness, and user rank between humans and bots."),
                tags$li("Activity & Engagement tab answers RQ2 by visualising posting frequency and engagement metrics such as comments, reposts, and likes."),
                tags$li("Content & Timing tab relates to RQ3 and RQ4 by examining URL use, text length, emotion signals, and temporal posting behaviour."),
                tags$li("Interactive Story tab provides multiple exploratory visual tools to understand how behaviour shifts across follower-following rate levels and other factors."),
                tags$li("AI Graph tab presents an AI-generated correlation heatmap summarising global variable relationships."),
                tags$li("PCA tab shows dimensionality-reduction visualisations to observe clustering patterns between bots and humans.")
              )
            )
          )
        )
      )

    ) # end tabItems
  ) # end dashboardBody
)
