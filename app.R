library(shiny)
library(bslib)

custom_theme <- bs_theme(
  font_scale = .8
)

# Define UI for application that draws a histogram
ui <- page_fixed(
  theme = custom_theme,
  card(markdown("Use the buttons below to **generate plots showing plausible data for the height** of 
                  
                  * 1st vs. 2nd graders
                  * 1st vs. 4th graders
                  * 1st vs. 7th graders")),
  
  # Layout columns for side-by-side distribution cards
  layout_columns(
    card(
      card_header("1st vs. 2nd graders", class = "bg-dark"),
      card_body(
          card(shinycssloaders::withSpinner(
               plotOutput("plot1", 
                         width = "300px",
                         height = "300px"
                         ),
               color = "#8cd000"),
               textOutput("overlap")
               ),
        layout_columns(
          actionButton("smaller_plot1",  icon("scale-balanced")),
          actionButton("larger_plot1",  icon("scale-unbalanced"))
          )
      )
    ),
        card(
          card_header("1st vs. 4th graders", class = "bg-dark"),
          card_body(
            card(shinycssloaders::withSpinner(
              plotOutput("plot2", 
                         width = "300px",
                         height = "300px"
              ),
              color = "#8cd000"),
              textOutput("overlap")
            ),
            layout_columns(
              actionButton("smaller_plot2",  icon("scale-balanced")),
              actionButton("larger_plot2",  icon("scale-unbalanced")),
            )
          )
        ),
            card(
              card_header("1st vs. 7th graders", class = "bg-dark"),
              card_body(
                card(shinycssloaders::withSpinner(
                  plotOutput("plot3", 
                             width = "300px",
                             height = "300px"
                  ),
                  color = "#8cd000"),
                  textOutput("overlap")
                ),
                layout_columns(
                  actionButton("smaller_plot3",  icon("scale-balanced")),
                  actionButton("larger_plot3",  icon("scale-unbalanced"))),
      )))
)

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
  
  output$plot1 <- renderPlot({
    Firstgraders=distribution_normal(300, 45.4, 2.05)     
    Secondgraders=distribution_normal(300, 45.4, 2.05) +
      max(1*input$larger_plot1 - 1*input$smaller_plot1, 0)
    
    # First distribution
    par(mfrow = c(2,1), mar=c(4,4,2,1))
    hist(Firstgraders, 
         breaks=30, 
         xlim=c(min(c(Firstgraders, Secondgraders)),max(c(Firstgraders, Secondgraders))), 
         ylab = "",
         xlab = "[inches]",
         col=rgb(1,0,0,0.5), 
         main="1st graders" )
    
    # Second with add=T to plot on top
    hist(Secondgraders, 
         breaks=30, 
         xlim=c(min(c(Firstgraders, Secondgraders)),max(c(Firstgraders, Secondgraders))), 
         ylab = "",
         xlab = "[inches]",
         col=rgb(0,0,1,0.5),
         main="2nd graders" )
  })
  
  cohend <- reactive({
    (mean(distribution_normal(300, 45.4, 2.05)) - 
       mean(distribution_normal(300, 45.4, 2.05) +
              max(1*input$larger_plot1 - 1*input$smaller_plot1, 0)))/4
  })
  
  output$plot2 <- renderPlot({
    Erstklässler=distribution_normal(300, 45.4, 2.05)     
    Viertklässler=distribution_normal(300, 45.4, 2.05) +
      max(1*input$larger_plot2 - 1*input$smaller_plot2, 0)
    
    # Third distribution
    par(mfrow = c(2,1), mar=c(2,1,1.5,1))
    hist(Erstklässler, 
         breaks=30, 
         xlim=c(min(c(Erstklässler, Viertklässler)),max(c(Erstklässler, Viertklässler))), 
         ylab = "",
         xlab = "",
         col=rgb(1,0,0,0.5), 
         main="1st graders")
    
    # Fourth with add=T to plot on top
    hist(Viertklässler, 
         breaks=30, 
         xlim=c(min(c(Erstklässler, Viertklässler)),max(c(Erstklässler, Viertklässler))), 
         ylab = "",
         col=rgb(0,0,1,0.5),
         main="4th graders")
  })
  
  cohend <- reactive({
    (mean(distribution_normal(300, 45.4, 2.05)) - 
       mean(distribution_normal(300, 45.4, 2.05) +
              max(1*input$larger_plot2 - 1*input$smaller_plot2, 0)))/15
  })
  
  output$plot3 <- renderPlot({
    Erstklässler=distribution_normal(300, 45.4, 2.05)     
    Siebtklässler=distribution_normal(300, 45.4, 2.05) +
      max(1*input$larger_plot3 - 1*input$smaller_plot3, 0)
  
  # Fifth distribution
  par(mfrow = c(2,1), mar=c(2,1,1.5,1))
  hist(Erstklässler, 
       breaks=30, 
       xlim=c(min(c(Erstklässler, Siebtklässler)),max(c(Erstklässler, Siebtklässler))), 
       ylab = "",
       xlab = "",
       col=rgb(1,0,0,0.5), 
       main="1st graders")
  
  # Sixth with add=T to plot on top
  hist(Siebtklässler, 
       breaks=30, 
       xlim=c(min(c(Erstklässler, Siebtklässler)),max(c(Erstklässler, Siebtklässler))), 
       ylab = "",
       col=rgb(0,0,1,0.5),
       main="7th graders")
})

cohend <- reactive({
  (mean(distribution_normal(300, 45.4, 2.05)) - 
     mean(distribution_normal(300, 45.4, 2.05) +
            max(1*input$larger_plot3 - 1*input$smaller_plot3, 0)))/15
})  
  
  
  output$overlap <- renderText({
    paste0("Aktuelle Überlappung = ", round(2*pnorm(-abs(cohend())/2), 2)*100, "%")
  })
}   


# Run the application 
shinyApp(ui = ui, server = server)
