library(shiny)
library(tidyverse)

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel(
    div(class = "title", "Sampling Distributions of Statistics")
  ),
  
  fluidRow(column(4,
          selectInput("distr", "Distribution", 
                       choices = c("Left-Skewed", "Right-Skewed", "Symmetric"),
                       selected = "Symmetric"
                       )
           ),
           
           column(4,
           selectInput("stat", "Statistic", 
                       choices = c("Mean", "Median", "Range", "Standard deviation"),
                       selected = "Mean"
                       )
           ),
           
           column(4,
                  numericInput("n", "Sample Size", value = 30,
                              min = 1, max = 100, step = 1
                              )
                  )
  ),
  
  fluidRow(
    column(6,
      plotOutput("population"),
    ),
    column(6,
      plotOutput("sampling")
    )
  )
)

server <- function(input, output){
  
  popdata <- reactive({
    if(input$distr == "Symmetric"){
     data.frame(dat=rnorm(1000, 10, 2))
    } else if(input$distr == "Left-Skewed"){
      data.frame(dat=rbeta(1000, 10, .5))
    } else if(input$distr == "Right-Skewed"){
      data.frame(dat=rexp(1000,10))
    } 
  })
  
  samples <- reactive({
    
    # Right-Skewed Sample Data
    if(input$stat == "Mean" && input$distr == "Right-Skewed"){
      data.frame(dat=sapply(1:1000, function(i) mean(rexp(input$n,10))))
    } else if(input$stat == "Median" && input$distr == "Right-Skewed"){
      data.frame(dat=sapply(1:1000, function(i) median(rexp(input$n,10))))
    } else if(input$stat == "Range" && input$distr == "Right-Skewed"){
      data.frame(dat=sapply(1:1000, function(i) {x<-rexp(input$n,10); max(x)-min(x)}))
    } else if(input$stat == "Standard deviation" && input$distr == "Right-Skewed"){
      data.frame(dat=sapply(1:1000, function(i) sd(rexp(input$n,10))))
    }
    

    # Symmetric Sample Data
    else if(input$stat == "Mean" && input$distr == "Symmetric"){
      data.frame(dat=sapply(1:1000, function(i) mean(rnorm(input$n, 10, 2))))
    } else if(input$stat == "Median" && input$distr == "Symmetric"){
      data.frame(dat=sapply(1:1000, function(i) median(rnorm(input$n, 10, 2))))
    } else if(input$stat == "Range" && input$distr == "Symmetric"){
      data.frame(dat=sapply(1:1000, function(i) {x<-rnorm(input$n, 10, 2); max(x)-min(x)}))
    } else if(input$stat == "Standard deviation" && input$distr == "Symmetric"){
      data.frame(dat=sapply(1:1000, function(i) sd(rnorm(input$n, 10, 2))))
    }    
    
    # Left-Skewed Sample Data
    else if(input$stat == "Mean" && input$distr == "Left-Skewed"){
      data.frame(dat=sapply(1:1000, function(i) mean(rbeta(input$n, 10, .5))))
    } else if(input$stat == "Median" && input$distr == "Left-Skewed"){
      data.frame(dat=sapply(1:1000, function(i) median(rbeta(input$n, 10, .5))))
    } else if(input$stat == "Range" && input$distr == "Left-Skewed"){
      data.frame(dat=sapply(1:1000, function(i) {x<-rexp(input$n,10); max(x)-min(x)}))
    } else if(input$stat == "Standard deviation" && input$distr == "Left-Skewed"){
      data.frame(dat=sapply(1:1000, function(i) sd(rbeta(input$n, 10, .5))))
    }    
    
   
  })
  
  output$population <- renderPlot({
    ggplot(popdata(), aes(x = dat)) +
      geom_histogram(bins = 10, 
                     color = "black", 
                     fill = "lightblue") +
      theme_light() +
      labs(title = "Sample from the Original Population",
           x = "x",
           y = "Frequency") +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
      )
  })
  
  output$sampling <- renderPlot({
    ggplot(samples(), aes(x = dat)) +
      geom_histogram(bins = 10, 
                     color = "black", 
                     fill = "lightblue") +
      theme_light() +
      labs(title = "Sampling Distribution of the Statistic",
           x = "Statistic",
           y = "Frequency") +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
      )
  })
}

shinyApp(ui = ui, server = server)