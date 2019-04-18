library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investing scenarios"),
   
   #
   fluidRow(
      column(4,
        sliderInput("initial",
                  "Initial Amount",
                   min = 1,
                   max = 100000,
                   value = 1000,
                   step = 500),
        sliderInput("return",
                   "Return Rate (in %)",
                   min = 0, 
                   max = 20, 
                   value = 5,
                   step = .1)
      ),
      column(4,
        sliderInput("years",
                   "Years",
                   min = 0, 
                   max = 50, 
                   value = 20,
                   step = 1),
        sliderInput("contrib",
                    "AnnualContribution",
                     min = 0, 
                     max = 50000, 
                     value = 2000,
                     step = 500)
      ), 
      column(4,
        sliderInput("growth",
                   "Growth Rate (in %)",
                    min = 0, 
                    max = 20, 
                    value = 2,
                    step = .1),
        selectInput("facet", "Facet?", c("No", "Yes"))
      )
   ),
   #
   hr(),
   width = 16,
   h4("Timelines"),
   plotOutput("plot"),
   h4("Balances"),
   tableOutput("table")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$facet == "No") {
      no_contrib <- rep(0, input$years + 1)
      fixed_contrib <- rep(0, input$years + 1)
      growing_contrib <- rep(0, input$years + 1)
      for (i in 0:input$years) {
        no_contrib[i + 1] = input$initial * (1 + input$return * .01)^i
        fixed_contrib[i + 1] = no_contrib[i + 1] + input$contrib * ((((1 + input$return * .01)^i) - 1) / (input$return * .01))
        growing_contrib[i + 1] = no_contrib[i + 1] + input$contrib * ((((1 + input$return * .01)^i) - ((1 + input$growth * .01)^i)) / (input$return * .01 - input$growth * .01))
      }
      modalities <- data.frame("no_contrib" = no_contrib, "fixed_contrib" = fixed_contrib, "growing_contrib" = growing_contrib)
      ggplot(modalities) +
        geom_line(aes(x = 0:input$years, y = modalities$no_contrib, color = "no_contrib")) +
        geom_line(aes(x = 0:input$years, y = modalities$fixed_contrib, color = "fixed_contrib")) +
        geom_line(aes(x = 0:input$years, y = modalities$growing_contrib, color = "growing_contrib")) +
        labs(title = "Three modes of investing", x = "year", y = "value", color = "value") 
    }
    else {
      no_contrib <- rep(0, input$years + 1)
      fixed_contrib <- rep(0, input$years + 1)
      growing_contrib <- rep(0, input$years + 1)
      for (i in 0:input$years) {
        no_contrib[i + 1] = input$initial * (1 + input$return * .01)^i
        fixed_contrib[i + 1] = no_contrib[i + 1] + input$contrib * ((((1 + input$return * .01)^i) - 1) / (input$return * .01))
        growing_contrib[i + 1] = no_contrib[i + 1] + input$contrib * ((((1 + input$return * .01)^i) - ((1 + input$growth * .01)^i)) / (input$return * .01 - input$growth * .01))
      }
      modalities <- data.frame("contrib_type" = c(rep("no_contrib", input$years + 1), rep("fixed_contrib", input$years + 1), rep("growing_contrib", input$years + 1)), "contrib_balance" = c(no_contrib, fixed_contrib, growing_contrib))
      ggplot(modalities, aes(x = 0:((3 * (input$years + 1)) - 1) , y = modalities$contrib_balance, color = modalities$contrib_type, fill = modalities$contrib_type)) +
        geom_line() +
        geom_area() +
        facet_wrap(~ modalities$contrib_type)
    }
  })
  
  output$table <- renderTable({
    no_contrib <- rep(0, input$years + 1)
    fixed_contrib <- rep(0, input$years + 1)
    growing_contrib <- rep(0, input$years + 1)
    for (i in 0:input$years) {
      no_contrib[i + 1] = input$initial * (1 + input$return * .01)^i
      fixed_contrib[i + 1] = no_contrib[i + 1] + input$contrib * ((((1 + input$return * .01)^i) - 1) / (input$return * .01))
      growing_contrib[i + 1] = no_contrib[i + 1] + input$contrib * ((((1 + input$return * .01)^i) - ((1 + input$growth * .01)^i)) / (input$return * .01 - input$growth * .01))
    }
    modalities <- data.frame("year" = 0:input$years,"no_contrib" = no_contrib, "fixed_contrib" = fixed_contrib, "growing_contrib" = growing_contrib)
    modalities
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
