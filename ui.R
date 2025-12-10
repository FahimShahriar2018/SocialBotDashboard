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
            ),
            div(class = "section-card",
              h4("Feature Correlations", class = "section-title"),
              plotlyOutput("corr_heatmap", height = "420px"),
              downloadButton("download_corr_heatmap", "Download SVG")
            )
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
        ),
        fluidRow(
          column(
            12,
            div(class = "section-card",
              h4("Animated: Engagement vs Follower/Following", class = "section-title"),
              imageOutput("animated_scatter", height = "260px")
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
          h4("AI-generated Correlation Heatmap"),
          imageOutput("ai_graph", height = "320px")
        )
      ),

      # ------------- REPORT / DOWNLOAD -------------
      tabItem(tabName = "report",
        fluidRow(
          column(
            6,
            div(class = "section-card",
              h4("Download Project Report"),
              downloadButton("download_report", "Download Report (PDF)")
            )
          ),
          column(
            6,
            div(class = "section-card",
              h4("User Guide"),
              tags$ul(
                tags$li("Use Overview to see dataset summary."),
                tags$li("Profile tab shows account ranking metrics."),
                tags$li("Activity & Engagement shows posting and user activity."),
                tags$li("Content tab shows text and URL behavior."),
                tags$li("Interactive Story tab provides multiple tools to explore bot vs human behaviour."),
                tags$li("AI Graph tab shows an AI-generated visual summary.")
              )
            )
          )
        )
      )

    ) # end tabItems
  ) # end dashboardBody
)
