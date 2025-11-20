# ui.R ----
# Defines the Shiny UI as object `ui`

ui <- navbarPage(
  title = "SocialBot Dashboard",
  id = "main_nav",
  theme = shinytheme("flatly"),  # modern theme
  
  header = tagList(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #f5f7fb;
        }
        .navbar-default {
          box-shadow: 0 2px 6px rgba(0,0,0,0.08);
        }
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
        .plot-wrapper {
          margin-top: 8px;
          margin-bottom: 8px;
        }
        .download-row {
          margin-top: 6px;
        }
      "))
    )
  ),
  
  ## ----------------- Overview -----------------
  tabPanel(
    "Overview",
    fluidRow(
      column(
        4,
        div(
          class = "section-card",
          h4("Dataset Overview", class = "section-title"),
          p("Quick summary of the SocialBot dataset.", class = "section-subtitle"),
          verbatimTextOutput("data_summary"),
          hr(),
          tags$small("Note: is_bot = 0 (Human), 1 (Bot)")
        )
      ),
      column(
        8,
        div(
          class = "section-card",
          h4("Class Distribution", class = "section-title"),
          p("Number of human vs bot accounts.", class = "section-subtitle"),
          div(class = "plot-wrapper", plotOutput("class_bar", height = "260px")),
          div(
            class = "download-row",
            downloadButton("download_class_bar", "Download SVG")
          )
        ),
        div(
          class = "section-card",
          h4("Feature Correlations", class = "section-title"),
          p("Correlation heatmap of numeric features.", class = "section-subtitle"),
          div(class = "plot-wrapper", plotlyOutput("corr_heatmap", height = "420px")),
          div(
            class = "download-row",
            downloadButton("download_corr_heatmap", "Download SVG")
          )
        )
      )
    )
  ),
  
  ## ----------------- Profile & Popularity -----------------
  tabPanel(
    "Profile & Popularity",
    fluidRow(
      column(
        6,
        div(
          class = "section-card",
          h4("Follower / Following Ratio", class = "section-title"),
          p("Distribution of follower/following rate by account type.", class = "section-subtitle"),
          div(class = "plot-wrapper", plotOutput("box_follow_ratio", height = "320px")),
          div(
            class = "download-row",
            downloadButton("download_box_follow_ratio", "Download SVG")
          )
        )
      ),
      column(
        6,
        div(
          class = "section-card",
          h4("User Rank (urank)", class = "section-title"),
          p("Comparison of user rank between humans and bots.", class = "section-subtitle"),
          div(class = "plot-wrapper", plotOutput("box_urank", height = "320px")),
          div(
            class = "download-row",
            downloadButton("download_box_urank", "Download SVG")
          )
        )
      )
    )
  ),
  
  ## ----------------- Activity & Engagement -----------------
  tabPanel(
    "Activity & Engagement",
    fluidRow(
      column(
        6,
        div(
          class = "section-card",
          h4("Posting Rate", class = "section-title"),
          p("How frequently different account types post.", class = "section-subtitle"),
          div(class = "plot-wrapper", plotOutput("postrate_violin", height = "320px")),
          div(
            class = "download-row",
            downloadButton("download_postrate_violin", "Download SVG")
          )
        )
      ),
      column(
        6,
        div(
          class = "section-card",
          h4("Average Engagement", class = "section-title"),
          p("Mean comments, reposts and likes per account type.", class = "section-subtitle"),
          div(class = "plot-wrapper", plotOutput("engagement_bars", height = "320px")),
          div(
            class = "download-row",
            downloadButton("download_engagement_bars", "Download SVG")
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(
          class = "section-card",
          h4("Animated: Engagement vs Follower/Following", class = "section-title"),
          p("Animated view across three rank groups (Low / Medium / High).", class = "section-subtitle"),
          div(
            style = "text-align:center;",
            imageOutput("animated_scatter", width = "70%", height = "260px")
          )
        )
      )
    )
  ),
  
  ## ----------------- Content & Timing -----------------
  tabPanel(
    "Content & Timing",
    fluidRow(
      column(
        6,
        div(
          class = "section-card",
          h4("URL & Text Length Behaviour", class = "section-title"),
          p("How URL use and text length variability differ between bots and humans.", class = "section-subtitle"),
          div(class = "plot-wrapper", plotOutput("url_textlength_box", height = "340px")),
          div(
            class = "download-row",
            downloadButton("download_url_textlength_box", "Download SVG")
          )
        )
      ),
      column(
        6,
        div(
          class = "section-card",
          h4("Words vs Emotion Tokens", class = "section-title"),
          p("Relationship between text length and emotion usage.", class = "section-subtitle"),
          div(class = "plot-wrapper", plotOutput("word_emotion_scatter", height = "340px")),
          div(
            class = "download-row",
            downloadButton("download_word_emotion_scatter", "Download SVG")
          )
        )
      )
    )
  ),
  
  ## ----------------- Interactive Story -----------------
  tabPanel(
    "Interactive Story",
    fluidRow(
      column(
        6,
        div(
          class = "section-card",
          h4("Story: Engagement vs Follower/Following", class = "section-title"),
          p("Use the play button or drag the slider to move through user rank groups (Low, Medium, High). Watch how bots and humans differ in likes for different follower/following ratios.", 
            class = "section-subtitle"),
          plotlyOutput("interactive_story_plot", height = "420px")
        )
      ),
      column(
        6,
        div(
          class = "section-card",
          h4("How to read this story", class = "section-title"),
          tags$ol(
            tags$li("Each point is an account (Human or Bot)."),
            tags$li("X-axis: follower/following rate."),
            tags$li("Y-axis: average likes (ave_attitudes)."),
            tags$li("Color: account type (Human vs Bot)."),
            tags$li("Animation frame: rank_group (Low / Medium / High user rank).")
          ),
          p("As you move from Low → Medium → High rank, you can describe how engagement changes for humans vs bots. This is your narrative for the story section.", 
            class = "section-subtitle")
        )
      )
    )
  ),

  ## ----------------- AI-generated Graph -----------------
  tabPanel(
    "AI-generated Graph",
    fluidRow(
      column(
        12,
        div(
          class = "section-card",
          h4("AI-generated Correlation Heatmap", class = "section-title"),
          p("This heatmap was generated by an AI tool from a subset of the SocialBot dataset.", class = "section-subtitle"),
          div(
            style = "text-align:center;",
            imageOutput("ai_graph", width = "75%", height = "320px")
          )
        )
      )
    )
  ),
  
  ## ----------------- Report / Download -----------------
  tabPanel(
    "Report / Download",
    fluidRow(
      column(
        6,
        div(
          class = "section-card",
          h4("Download Project Report", class = "section-title"),
          p("Click below to download the project report/manual as a PDF.", class = "section-subtitle"),
          downloadButton("download_report", "Download Report (PDF)")
        )
      ),
      column(
        6,
        div(
          class = "section-card",
          h4("How to Use this Dashboard", class = "section-title"),
          tags$ul(
            tags$li("Use the Overview tab to see dataset and feature correlations."),
            tags$li("Profile & Popularity shows structural differences between accounts."),
            tags$li("Activity & Engagement explores posting and interaction patterns."),
            tags$li("Content & Timing shows text and timing behaviour."),
            tags$li("AI-generated Graph documents your AI-created visualization.")
          )
        )
      )
    )
  )
)
