
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(dplyr)
library(stringr)
library(shiny)
library(tm)
library(wordcloud)


#Define UI
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Select Book Type
    sidebarPanel(
      selectInput("book", "Choose a book:", 
                  c("Metamorphosis", "Beowulf", "Faust"))
      
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

server <- shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    meta <-
      readLines("data/metamorphosis.txt", encoding="UTF-8") %>%
      as.character()
    
    beowulf <- 
      readLines("data/beowulf.txt", encoding="UTF-8") %>%
      as.character()
    
    faust <- 
      readLines("data/faust.txt", encoding="UTF-8") %>%
      as.character() %>% 
      str_replace_all("_Faust._", "") %>% 
      str_replace_all("_Margarete._", "") %>% 
      str_replace_all("__Mephistopheles.__", "") %>% 
      str_replace_all("Mephistophel", "")
    
    meta <- meta %>%
      tolower() %>%
      removeNumbers() %>%
      removePunctuation() %>%
      removeWords(stopwords("english")) %>%
      stemDocument() %>%
      stripWhitespace()
    
    faust <- faust %>%
      tolower() %>%
      removeNumbers() %>%
      removePunctuation() %>%
      removeWords(stopwords("german")) %>%
      stemDocument() %>%
      stripWhitespace()
    
    beowulf <- beowulf %>% 
      tolower() %>%
      removeNumbers() %>%
      removePunctuation() %>%
      removeWords(stopwords("english")) %>%
      stemDocument() %>%
      stripWhitespace()
    
    text <- input$book
    
    if(text == "Metamorphosis"){
      value <- meta
    } else if (text == "Beowulf"){
      value <- beowulf
    } else if (text == "Faust"){
      value <- faust
    } 
    
    
    word_cloud <- VectorSource(value) %>% 
      Corpus()
    wordcloud(value, scale=c(3,1), max.words = 80
              , random.order=FALSE,
              rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Set1"))
    
  })
})

shinyApp(ui = ui, server = server)