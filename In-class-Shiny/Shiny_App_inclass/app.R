pacman::p_load(shiny, tidyverse)


data <- read_csv("Data/Exam_data.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Pupils examination Result Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Subject:",
                  choices = c("English" = "ENGLISH",
                              "Science" = "SCIENCE",
                              "Maths" = "MATHS"),
                  selected = "ENGLISH"),
      sliderInput(inputId = "bins",
                  label = "Number of bins",
                  min =5,
                  max = 20,
                  value = 10)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(data, 
           aes_string(x = input$variable)) +
           geom_histogram(bins = input$bins,
                          color="black",
                          fill='lightblue')
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
