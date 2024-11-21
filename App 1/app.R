########## app for the training phase ##########

library(rsconnect)
library(shiny)
library(bslib)

custom_theme <- bs_theme(
  font_scale = .8
)

# Define UI for application that draws a histogram
ui <- page_fixed(
  theme = custom_theme,
  card(markdown("Use the buttons to **generate plots showing plausible data for the height** of: 
                  
                  * 1st vs. 3nd graders
                  * 1st vs. 6th graders
                  
                **Hint**: You can use the buttons several times. When you are done, please press continue.")),
  
  # Layout columns for side-by-side distribution cards
  layout_columns(
    card(
      card_header("1st vs. 3rd graders", class = "bg-dark"),
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
          actionButton("smaller_plot1", icon("scale-unbalanced-flip")),
          actionButton("larger_plot1", icon("scale-unbalanced"))
        )
      )
    ),
    card(
      card_header("1st vs. 6th graders", class = "bg-dark"),
      card_body(
        card(
          # Center the second plot
          div(class = "d-flex justify-content-center",
              shinycssloaders::withSpinner(
                plotOutput("plot2", 
                           width = "300px",
                           height = "300px"),
                color = "#8cd000"
              )
          ),
          textOutput("overlap")
        ),
        layout_columns(
          actionButton("smaller_plot2", icon("scale-unbalanced-flip")),
          actionButton("larger_plot2", icon("scale-unbalanced"))
        )
      )
    )
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

server <- function(input, output) {
  
  # Plot 1: First vs Third Graders
  output$plot1 <- renderPlot({
    Firstgraders=distribution_normal(300, 45.4, 2.05)     
    Thirdgraders=distribution_normal(300, 45.4, 2.05) +
      max(1*input$larger_plot1 - 1*input$smaller_plot1, 0)
    
    # First distribution
    par(mfrow = c(2,1), mar=c(2,1,1.5,1))
    hist(Firstgraders, 
         breaks=30, 
         xlim=c(min(c(Firstgraders, Thirdgraders)),max(c(Firstgraders, Thirdgraders))), 
         ylab = "",
         xlab = "",
         col=rgb(1,0,0,0.5), 
         main="1st graders",
         xaxt = "n" )
    
    # Second with add=T to plot on top
    hist(Thirdgraders, 
         breaks=30, 
         xlim=c(min(c(Firstgraders, Thirdgraders)),max(c(Firstgraders, Thirdgraders))), 
         ylab = "",
         xlab = "",
         col=rgb(0,0,1,0.5),
         main="3rd graders",
         xaxt = "n" )
  })
  
  # Cohen's d for First vs Third Graders
  cohend_1_3 <- reactive({
    Firstgraders <- distribution_normal(300, 45.4, 2.05)
    Thirdgraders <- distribution_normal(300, 45.4, 2.05) +
      max(1 * input$larger_plot1 - 1 * input$smaller_plot1, 0)
    (mean(Firstgraders) - mean(Thirdgraders)) / 4.1
  })
  
  # Plot 2: First vs Sixth Graders
  output$plot2 <- renderPlot({
    Firstgraders=distribution_normal(300, 45.4, 2.05)     
    Sixthgraders=distribution_normal(300, 45.4, 2.05) +
      max(0.5*input$larger_plot2 - 0.5*input$smaller_plot2, 0)
    
    # Third distribution
    par(mfrow = c(2,1), mar=c(2,1,1.5,1))
    hist(Firstgraders, 
         breaks=30, 
         xlim=c(min(c(Firstgraders, Sixthgraders)),max(c(Firstgraders, Sixthgraders))), 
         ylab = "",
         xlab = "",
         col=rgb(1,0,0,0.5), 
         main="1st graders",
         xaxt = "n" )
    
    # Fourth with add=T to plot on top
    hist(Sixthgraders, 
         breaks=30, 
         xlim=c(min(c(Firstgraders, Sixthgraders)),max(c(Firstgraders, Sixthgraders))), 
         ylab = "",
         col=rgb(0,0,1,0.5),
         main="6th graders",
         xaxt = "n" )
  })
  
  # Cohen's d for First vs Sixth Graders
  cohend_1_6 <- reactive({
    Firstgraders <- distribution_normal(300, 45.4, 2.05)
    Sixthgraders <- distribution_normal(300, 45.4, 2.05) +
      max(1 * input$larger_plot2 - 1 * input$smaller_plot2, 0)
    (mean(Firstgraders) - mean(Sixthgraders)) / 4.1
  })
  
  
}   

# Run the application 
shinyApp(ui = ui, server = server)
