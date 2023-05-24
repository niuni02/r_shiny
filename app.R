# Creating a R Shiny web app
# Author: Niuni Amarasinghe

# ***** LIBRARIES *****
install.packages("httpuv")
library(shiny)
install.packages("shinydashboard")
library(shinydashboard)
library(plotly) # for widgets
library(tidyverse)
install.packages("DataExplorer")
library(DataExplorer)
library(corrplot)
library(sf)
library(tmap)
install.packages("shinyWidgets")
library(shinyWidgets)

# LOAD IN DATASETS
raw_data <- read.csv("raw_data/deprivation_and_mobility.csv")
# using the mutate() function to calculate percentages from raw data
london <- mutate(raw_data, percentage_not_deprived = (Households_not_deprived/Deprivation*100),
                 percentage_deprived = (100 - percentage_not_deprived),
                 percentage_home = (Work_from_home/Total_methods_travel)*100,
                 percentage_public = (Public_transport/Total_methods_travel)*100,
                 percentage_car = (Car_or_van/Total_methods_travel)*100,
                 percentage_other = (Other_travel/Total_methods_travel)*100)
# using the select() function to keep only relevant data
london <- select(london, "GEO_CODE", "GEO_LABEL", "percentage_deprived", "percentage_home", "percentage_public", "percentage_car", "percentage_other")
View(london)
correlation_tests <- select(london, percentage_deprived, percentage_home, percentage_public, percentage_car, percentage_other) 
matrix <- cor(correlation_tests) # Produce a matrix holding correlations
general_health_scores <- read_csv("raw_data/general_health_scores.csv", 
                                  col_types = cols(very_bad_health = col_integer(), 
                                                   total_health = col_integer(), very_good_health = col_integer(), 
                                                   good_health = col_integer(), fair_health = col_integer(), 
                                                   bad_health = col_integer()))
View(general_health_scores)
# load in the London MSOAs shapefile
london_MSOA_shp <- st_read("raw_data/boundaries/MSOA_2011_London_gen_MHW.shp")
# select relevant columns
colnames(london_MSOA_shp)
london_MSOA_shp <- select(london_MSOA_shp, "MSOA11CD", "LAD11CD", "LAD11NM", "geometry")
View(london_MSOA_shp)

# read in London Boroughs shapefile
london_boroughs_shp <- st_read("raw_data/boundaries/London_Borough_Excluding_MHW.shp")
# select relevant columns
colnames(london_boroughs_shp)
london_boroughs_shp <- select(london_boroughs_shp, "NAME", "GSS_CODE", "geometry")
View(london_boroughs_shp)

health_sdf <- merge(general_health_scores, london_MSOA_shp,
                              by.x = "GEO_CODE", by.y = "MSOA11CD")
health_sdf <- st_sf(health_sdf)
View(health_sdf)


# ***** USER INTERFACE *****
# Define User Interface (UI) for application that draws a histogram
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Deprivation, Mobility and Health in R Shiny"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("London", tabName = "Mobility", icon = icon("car")),
      menuItem("Health scores", tabName = "HealthScores", icon = icon("heartbeat"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Mobility", 
              box(plotOutput("corrplot"), width = 15),
              box(
                width = 4,
                selectInput("mobilityFeatures", "Method of transport:",
                            c("percentage_home", "percentage_public", "percentage_car", "percentage_other"))
              ),
              dataTableOutput("mobilityTable")
      ),
      tabItem("HealthScores",
              box(plotOutput("health"), width = 15),
              box(
                width = 4,
                selectInput("healthFeatures", "Type of health score:",
                            c("very_bad_health", "bad_health", "very_good_health", "good_health", "fair_health"))
              ))
    )
  )
)


# ***** SERVER *****
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$corrplot <- renderPlot({
      plot(london$percentage_deprived, london[[input$mobilityFeatures]],
           xlab = "Percentage of deprived households",
           ylab = "Method of transport")
    })
    
    output$mobilityTable <- renderDataTable(london)
    
    output$health <- renderPlot({
  qtm(health_sdf, fill= input$healthFeatures, fill.palette = "Pastel1", border = "white", fill.title = "Total count") + 
    tm_shape(london_boroughs_shp) +
    tm_borders(col = "black") +
    tm_layout(
      title = "Distribution of health scores",
      bg.color = "grey85",
      legend.outside = FALSE,
      legend.title.size = 0.75,
      legend.title.fontface = 2) +
    tm_compass(position = c("RIGHT", "TOP"), size = 2, type = "rose") +
    tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("LEFT", "BOTTOM"), size = 0.5)
})}

# ***** RUN WEB APP *****
# Run the application 
shinyApp(ui = ui, server = server)

