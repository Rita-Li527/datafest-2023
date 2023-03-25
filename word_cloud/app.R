library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(rgdal)
library (raster)
library(dplyr)
library(sf)
library(tidyverse)
library(plotly)
library(lubridate)


#load data
books <- list("A Mid Summer Night's Dream" = "summer",
               "The Merchant of Venice" = "merchant",
               "Romeo and Juliet" = "romeo")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  #this is for semantic dashboard
  #theme="slate",
  #this only for shiny dashboard
  skin = "purple",
  dashboardHeader(title = "Word Cloud"),
  dashboardSidebar(#add sidebar menu
    sidebarMenu(
      menuItem("Word Cloud", tabName = "word_cloud", icon = icon("tree")),
      menuItem("Other Plots", tabName = "other_plots", icon = icon("car"))
    )),
  dashboardBody(#Add tab Item
    tabItems(
      tabItem("word_cloud",
              fluidRow(
                # drop down list(add interactivity):set ID, add lable feature, a vector with possible choices
                box(
                  selectInput("selection", "Choose a book:",
                              choices = books),
                  textOutput("descrp"),
                  width = 4
                ),
                #action button
                box(
                  actionButton("update", "Change"),
                  ),
                  width = 4
                ),
              box(sliderInput("max",
                              "Maximum Number of Words:",
                              min = 1,  max = 300,  value = 100))),
      tabItem("other_plots",
              fluidPage(h1("Cars"),
                        # add data table item
                        dataTableOutput("carstable")
              ))
     
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #render the correlation plot with the iris dataset
  output$correlation_plot <- renderPlot({
    #make the dropdown list the input into the plot function
    plot(iris$Sepal.Length,iris[[input$features]],
         xlab = "Sepal length", ylab = "Feature")
  })
  #render the car table, just put in the data name
  output$carstable <- renderDataTable(mtcars)
}

# Run the application 
shinyApp(ui = ui, server = server)

