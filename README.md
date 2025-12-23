# SocialBotDashboard
## Bot Detection Data Visualization (R/Shiny)

An interactive R/Shiny dashboard and analysis report for exploring **bot vs human** behavior patterns in social media accounts using the **SocialBot.xlsx** dataset. The project includes multiple visualization tabs, SVG downloads of plots, and a one-click **Analysis Report (PDF)** generator.

---

## ✨ Features

- **Overview**
  - Dataset summary (rows/columns, class distribution)
  - Bar chart: Human vs Bot count
  - Correlation heatmap of numeric features (Plotly)

- **Profile & Popularity**
  - Boxplots for follower/following rate and user rank

- **Activity & Engagement**
  - Posting-rate distribution (violin + boxplot)
  - Average engagement (grouped bars)
  - **Follower/Following Rate histogram** (updated styling)
  - Optional animated histogram using **gganimate** (saved as GIF)

- **Content & Timing**
  - Boxplots for URL & text variability
  - Scatter plot: words vs emotion tokens

- **PCA**
  - Pairwise scatter plots
  - PCA scatter plot (PC1 vs PC2)

- **Report / Download**
  - Download SVG for key plots
  - Generate and download **Analysis Report (PDF)** from RMarkdown

---

## 📁 Repository Structure

```text
.
├── app.R                     # Shiny launcher
├── ui.R                      # Shiny UI
├── server.R                  # Shiny server logic
├── data_prep.R               # data loading + preprocessing + derived dataframes
├── interactive_animation.R   # Plotly-based interactive story module
├── SocialBot.xlsx            # dataset (place in project root)
├── SocialBot_report.Rmd      # analysis report template (PDF)
├── make_subset.R
├── DV_SocialBot_project_report_group8.pdf
├── socialbot_subset.csv
├── SocialBot.xlsx
├── www/
│   ├── ffr_hist_animated.gif # gganimate export
│   └── ...                   # other static assets
└── scripts/
    └── make_ffr_hist_animation.R  # create animated GIF
```
## Requirements
- R (recommended: R 4.x)
- Packages:
  - shiny
  - ggplot2
  - plotly
  - dplyr
  - tidyr
  - readxl
  - rmarkdown
  - knitr
  - (optional for animation) gganimate, gifski

