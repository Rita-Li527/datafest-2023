library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(rgdal)
library (raster)
library(dplyr)
library(sf)
library(tidyverse)
library(lubridate)
library(wordcloud)
library(tm)
library(memoise)


#load data


client<- read_csv("word_all_client_questions.csv")

lawyer<- read_csv("word_all_attorney_respond.csv")

#get the choice for the drop down list


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
      menuItem("Other Plots", tabName = "other_plots", icon = icon("car")),
      menuItem("Description", tabName = "Description", icon = icon("bookmark"))
    )),
  dashboardBody(#Add tab Item
    tabItems(
      tabItem("word_cloud",
              fluidRow(
                # drop down list(add interactivity):set ID, add lable feature, a vector with possible choices
               
              box(
                selectInput("selection", "Choose a state:",
                            choices =c(unique(client$StateAbbr))),
                width = 8,
                sliderInput(
                  "max",
                  "Maximum Number of Words:",
                  min = 1,
                  max = 300,
                  value = 100
                ),
                sliderInput(
                  "freq",
                  "Minimum Frequency:",
                  min = 1,
                  max = 50,
                  value = 15
                )
              ),
            box(title=textOutput('title_client'),plotOutput("word_cloud_plot_client"), width= 6),
            box(title=textOutput('title_lawyer'),plotOutput("word_cloud_plot_lawyer"), width= 6)
     
      )), 
      tabItem("other_plots",
              fluidPage(h1("Cars"),
                        # add data table item
                        dataTableOutput("carstable")
              )),
      tabItem("Description",
              h2("Research Question", style = "font-family: 'times'; font-si20pt"),
           
              tags$li("What is the difference between the language used by the client and the lawyer? ", style = "color:blue"), 
              tags$li("How can we help the lawyer use words that are more similar to those used by the client to facilitate more efficient communication?", style = "color:blue")
     
    ))
))

# Define server logic required to draw a histogram
server <- function(input, output) {

client_state<-reactive({client %>%
  filter(StateAbbr== input$selection)})

lawyer_state<-reactive({lawyer %>%
    filter(StateAbbr== input$selection)})



output$word_cloud_plot_client <- renderPlot({
  wordcloud(
    client_state()$word,
    # column of words
    client_state()$n,
    # column of frequencies
    scale = c(5, 0.2),
    # range of font sizes of words
    min.freq = input$freq,
    max.words = input$max,
    # show the 200 most frequent words
    # random.order=FALSE,             # position the most popular words first
    colors = brewer.pal(8, "Dark2")
  )
})
output$word_cloud_plot_lawyer <- renderPlot({
  wordcloud(
    lawyer_state()$word,
    # column of words
    lawyer_state()$n,
    # column of frequencies
    scale = c(5, 0.2),
    # range of font sizes of words
    min.freq = input$freq,
    max.words = input$max,
    # show the 200 most frequent words
    # random.order=FALSE,             # position the most popular words first
    colors = brewer.pal(8, "Dark2")
  )
})
output$title_lawyer <- renderText({"Lawyers' Word Cloud"})

output$title_client <- renderText({"Clients' Word Cloud"})
}

# Run the application 
shinyApp(ui = ui, server = server)

