########## App for the estimation of effect magnitudes ##########

library(shiny)
library(bslib)
library(rsconnect)
library(googledrive)
library(googlesheets4)
library(tidyverse)

## Googlesheets Connection Setup ###############################################

gs4_auth(path = "key.json")

custom_theme <- bs_theme(
  font_scale = .8
)

# Define UI for application that draws a histogram
ui <- page_fixed(
  theme = custom_theme,
    # Layout columns 
  layout_columns(
    card(
      card_header("Formative assessment group vs. regular group", class = "bg-dark"),
      card_body(
        card(
          # Center the first plot
          div(class = "d-flex justify-content-center",
              shinycssloaders::withSpinner(
                plotOutput("plot1", 
                           width = "300px",
                           height = "300px"),
                color = "#8cd000"
              )
          ),
          textOutput("overlap")
        ),
        layout_columns(
          actionButton("smaller_plot1", icon("minus")),
          actionButton("larger_plot1", icon("plus"))
        )
      )
    ),
  ))

# Define server logic required to draw a histogram
distribution_normal <- function(n,
                                mean = 0,
                                sd = 1,
                                random = FALSE,
                                ...){
  if (random) {
    stats::rnorm(n, mean, sd)
  }
  else {
    stats::qnorm(stats::ppoints(n), mean, sd, ...)
  }
}

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    Firstgraders=distribution_normal(300, 45.4, 2.05)     
    Thirdgraders=distribution_normal(300, 45.4, 2.05) +
      max(0.5*input$larger_plot1 - 0.5*input$smaller_plot1, 0)
    
    # First distribution
    par(mfrow = c(2,1), mar=c(2,1,1.5,1))
    hist(Firstgraders, 
         breaks=30, 
         xlim=c(min(c(Firstgraders, Thirdgraders)),max(c(Firstgraders, Thirdgraders))), 
         ylab = "",
         xlab = "",
         col=rgb(1,0,0,0.5), 
         main="Regular group",
        xaxt = "n" )
    
    # Second with add=T to plot on top
    hist(Thirdgraders, 
         breaks=30, 
         xlim=c(min(c(Firstgraders, Thirdgraders)),max(c(Firstgraders, Thirdgraders))), 
         ylab = "",
         xlab = "",
         col=rgb(0,0,1,0.5),
         main="Formative assessment group",
         xaxt = "n")
  })
  
  cohend <- reactive({
    (mean(distribution_normal(300, 45.4, 2.05)) - 
       mean(distribution_normal(300, 45.4, 2.05) +
              max(1*input$larger_plot1 - 1*input$smaller_plot1, 0)))/20 #each input changes cohen's d for 0,05
  })
  
  ## URL Variable fetching #####################################################
  url_vars <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  ## Usage Logging #############################################################
  observeEvent(cohend(), {
    sheet_append("1j-Dh0VrNSKBVenbMllVr6EASX3O9_DX_op0s95VXFpw",
                 tibble(PROLIFIC_PID = ifelse(is.null(url_vars()$PROLIFIC_PID), 
                                              "code is missing", #to keep ncol constant
                                              url_vars()$PROLIFIC_PID), # Person identifier from URL
                        task_name = "ES_estimation2",
                        task_version = "main",
                        cohend = cohend(),
                        time = Sys.time(),
                        timezone = Sys.timezone()),
                 sheet = 3)
    
    
  })
}   

# Run the application 
shinyApp(ui = ui, server = server)
