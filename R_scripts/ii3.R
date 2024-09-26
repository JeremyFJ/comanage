# Load required libraries
library(shiny)
library(shinyMobile)
library(shinyTime)
library(shinymanager)
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(tidyr)
library(exifr)
library(DT)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(digest)

# Define global variables
current_date <<- Sys.Date()
tunisian_cities <<- read.csv("./www/data/port_positions.csv")
gear_list <<- data.frame(gear = c("Purse Seines", "Trawlers", "Drifting Longlines",
                              "Set Longlines", "Tuna Purse Seines",
                              "Set Gillnets", "Fixed Gear", "Dredge Fishing",
                              "Other"))

# Source external scripts
source("tabs.R")       # This will load all UI components
source("functions.R")  # This will load all necessary functions
source("server_logic.R") # This will load the server functions

# Define the UI
ui <- f7Page(
    title = "Fishermen's App",
        tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
    ),
    tabsUI()  # This will call the function from tabs.R to create the UI
)

# Define the server logic
server <- function(input, output, session) {
    appServer(input, output, session)  # This will call the function from functions.R for server logic
}

# Run the app
shinyApp(ui = ui, server = server)
