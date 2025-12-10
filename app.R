# app.R ----
library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rmarkdown)
library(plotly)


# Source separate components
source("data_prep.R")  # creates df, cor_df, engagement_summary, url_text_long
source("ui.R")         # creates ui
source("server.R")     # creates server function

# Run the app
shinyApp(ui = ui, server = server)