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

full <- read_csv("frequency_differ_words.csv")

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
                width = 4,
        
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
            box(plotOutput("frequency_plot"), width= 8),
            box(title=textOutput('title_client'),plotOutput("word_cloud_plot_client"), width= 6),
            box(title=textOutput('title_lawyer'),plotOutput("word_cloud_plot_lawyer"), width= 6)
     
      )), 
      tabItem("Description",
              h2("Research Question", style = "font-family: 'times'; font-si20pt"),
           
              tags$li("What is the difference between the language used by the client and the lawyer? ", style = "color:blue"), 
              tags$li("How can we help the lawyer use words that are more similar to those used by the client to facilitate more efficient communication?", style = "color:blue"),
              h2("Plot Analysis & Key Findings", style = "font-family: 'times'; font-si20pt"),
              p("Using word cloud visualizations, we can observe that there are variations in the emphasis and needs of clients across different states. Additionally, 
              the language used by lawyers tends to be more general than the language used by their clients. For example, in Alaska, clients may refer to specific family members such as their daughters or sons, 
              while lawyers prefer to use more generic terms such as 'parent,' 'family,' or 'child'.", style = "font-family: 'times'; font-si22pt"),
              
              p("In addition to word cloud visualizations, we have incorporated a comparative frequency 
              chart into our Shiny app. The blue color indicates words that are frequently used by 
              lawyers but are seldom used by clients. On the other hand, the red color 
                represents words that are commonly used by clients but are rarely used by lawyers.",
                style = "font-family: 'times'; font-si22pt"),
              p("This summary provides lawyers with valuable insights as they can prioritize understanding 
              the blue-colored words, which may be technical terms that the general public is 
              not familiar with. If lawyers use such terms, 
                they may need to explain them to ensure clients comprehend their meaning.",
                style = "font-family: 'times'; font-si22pt"),
              p("Conversely, the red-colored words showcase the language habits of clients, 
                which lawyers can incorporate more frequently to make clients feel better understood.",
                style = "font-family: 'times'; font-si22pt")
     
    ))
))

# Define server logic required to draw a histogram
server <- function(input, output) {

client_state<-reactive({client %>%
  filter(StateAbbr== input$selection)})

lawyer_state<-reactive({lawyer %>%
    filter(StateAbbr== input$selection)})

frency_state<-reactive({full %>%
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
output$frequency_plot <- renderPlot({
  frency_state() %>% mutate(word = fct_reorder(word, diff)) %>% 
    ggplot(aes(diff, word, fill = diff > 0)) +
    geom_col() +
    labs(x = "number of occurrences",
         y = "",
         title = "Differences in the Most Frequently Used Words Between Clients & Lawyers",
         fill = "More frequently \nused by Attorney")+
    theme_minimal()
})
}

# Run the application 
shinyApp(ui = ui, server = server)

