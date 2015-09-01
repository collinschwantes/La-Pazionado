library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("Spacelab"),
                fluidRow(
                  column(6,dataTableOutput(outputId = "table")),
                  column(6,p(textOutput("para")))
  )
)

server <- function(input, output){
  
  df <- as.data.frame(matrix(0, ncol = 15, nrow = 20))
  
  output$table <- renderDataTable({df})
  
  output$para <- renderText({
    text <- rep(x = "Hello World",1000)
  })
}
shinyApp(ui = ui,server = server)





