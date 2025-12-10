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
source("data_prep.R")  
source("ui.R")         
source("server.R")     

# Run the app
shinyApp(ui = ui, server = server)