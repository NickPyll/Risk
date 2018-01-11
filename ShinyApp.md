# Risk Shiny App
Shiny App to calculate probability of Risk visctory

```
# install.packages('shiny')
# install.packages('shinythemes')
# install.packages('rsconnect')
 
library(shiny)
library(shinythemes)
library(rsconnect)

ui <- fluidPage(
  
  theme = shinytheme("cyborg"),
  
  headerPanel('Should red attack blue?'),
  
  sidebarPanel(
    numericInput('red.qty', 'How many red troops?', 9, 1, 30),
    numericInput('blue.qty', 'How many blue troops?', 3, 1, 30),
    verbatimTextOutput('text')
  ),
  mainPanel(
    plotOutput('redplot'),
    plotOutput('blueplot')
  )
)

server <- function(input, output, session) {
  
  results <- reactive({
    
    x.max <- max(input$red.qty, input$blue.qty)
    
    red.list <- list()
    blue.list <- list()
    
    red.wins <- 0
    
    for (i in 1:1000){
      
      red.sample <- head(sort(sample.int(6, min(input$red.qty - 1, 3)), decreasing = TRUE), min(input$red.qty - 1, input$blue.qty, 2))
      blue.sample <- sort(sample.int(6, min(input$red.qty - 1, input$blue.qty, 2)), decreasing = TRUE)
      red.new <- input$red.qty - sum(red.sample == blue.sample | red.sample < blue.sample, na.rm = TRUE)
      blue.new <- input$blue.qty - sum(red.sample > blue.sample, na.rm = TRUE)
      
      repeat{
        
        if (blue.new == 0){
          red.list <- c(red.list, red.new)
          blue.list <- c(blue.list, blue.new)
          red.wins <- red.wins + 1
          break
        } # end first if
        if (red.new < 2){
          red.list <- c(red.list, red.new)
          blue.list <- c(blue.list, blue.new)
          break
          
        } # end second if
        
        red.sample <- head(sort(sample.int(6, min(red.new - 1, 3)), decreasing = TRUE), min(red.new - 1, blue.new, 2))
        blue.sample <- sort(sample.int(6, min(red.new - 1, blue.new, 2)), decreasing = TRUE)
        red.new <- red.new - sum(red.sample == blue.sample | red.sample < blue.sample, na.rm = TRUE)
        blue.new <- blue.new - sum(red.sample > blue.sample, na.rm = TRUE)
        
      } # end repeat
    } # end for
    red.wins <- red.wins / 10
    results <- list('redlist' = red.list)
    results <- append(results, list('bluelist' = blue.list))
    results <- append(results, red.wins)
    results <- append(results, x.max)
    results

  })
  
  output$redplot <- renderPlot({
    hist(as.numeric(as.character(unlist(results()[1]))),
         prob = TRUE,
         breaks = seq(0, as.numeric(as.character(results()[4])), 1),
         main = 'Number of Red Troops Remaining',
         xlab = ''
    )
    lines(density(as.numeric(as.character(unlist(results()[1])))),
          col = 'red')
  })
  output$blueplot <- renderPlot({
    hist(as.numeric(as.character(unlist(results()[2]))),
         prob = TRUE,
         breaks = seq(0, as.numeric(as.character(results()[4])), 1),
         main = 'Number of Blue Troops Remaining',
         xlab = ''
    )
    lines(density(as.numeric(as.character(unlist(results()[2])))),
          col = 'blue')
  })
  output$text <- renderText(paste('Red Wins ', results()[3], '% of the time.'))
  
}  
shinyApp(ui = ui, server = server)
```
