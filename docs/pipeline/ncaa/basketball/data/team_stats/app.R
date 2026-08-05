library(shiny)

team_Duke_2026 <- read.csv("team_Duke_2026.csv")

# Only offer columns that are actually numeric for the histogram
numeric_cols <- names(team_Duke_2026)[sapply(team_Duke_2026, is.numeric)]

ui <- fluidPage(
  titlePanel("Duke 2026 Team Stats — Histogram"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a stat:",
                  choices = numeric_cols,
                  selected = "team_score"),
      sliderInput("bins", "Number of bins:",
                  min = 5, max = 30, value = 15)
    ),
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    x <- team_Duke_2026[[input$variable]]
    hist(x,
         breaks = input$bins,
         main = paste("Distribution of", input$variable),
         xlab = input$variable,
         col = "steelblue",
         border = "white")
  })
}

shinyApp(ui = ui, server = server)
