#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Stat 133 Workout 2"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      inputPanel(
        sliderInput("init",
                    "Initial Amount",
                    step = 500,
                    min = 0,
                    sep = ',',
                    max = 100000,
                    value = 1000,
                    pre = '$'),
        sliderInput("return",
                    "Return Rate (in %)",
                    step = 0.1,
                    min = 0,
                    sep = ',',
                    max = 20,
                    value = 5, 
                    post = '%'),
        sliderInput("years",
                    "Years",
                    step = 1,
                    min = 0,
                    max = 50,
                    value = 20),
        sliderInput("annual",
                    "Annual Contribution",
                    step = 500,
                    min = 0,
                    max = 50000,
                    sep = ',',
                    value = 2000, 
                    pre = '$'),
        sliderInput("growth",
                    "Growth Rate (in %)",
                    step = 0.1,
                    min = 0,
                    max = 20,
                    value = 2, 
                    post = '%'),
        selectInput("facet",
                    "Facet",
                    choices = c("No", "Yes"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         tableOutput("df")
      )
   )
)

future_value <- function(pv, r, t) {
  fv <- pv * (1 + r)^t
  return(fv)
}
annuity <- function(C, r, t) {
  fva <- C * (((1+r)^t - 1) / r)
  return(fva)
}
growing_annuity <- function(C, r, g, t) {
  fvga <- C * (((1+r)^t - (1+g)^t) / (r-g))
  return(fvga)
}
plot_data <- function(init, return, years, annual, growth) {
  mode1 <- rep(0, years + 1)
  mode2 <- rep(0, years + 1)
  mode3 <- rep(0, years + 1)
  
  for(i in 0:years) {
    mode1[i+1] <- future_value(init, return/100, i)
    mode2[i+1] <- future_value(init, return/100, i) + annuity(annual, return/100, i)
    mode3[i+1] <- future_value(init, return/100, i) + growing_annuity(annual, return/100, growth/100, i)
             
  }
  my_table <- list(year = 0:years, no_contrib = mode1, fixed_contrib = mode2, growing_contrib = mode3)
  return(my_table)
}
non_facet_plot <- function(init, return, years, annual, growth) {
  my_table <- plot_data(init, return, years, annual, growth)
  par(bg = 'light gray')
  plot(my_table$year, my_table$no_contrib, type = "l", xlim = c(0, years), ylim = c(0, max(my_table$growing_contrib)), xlab = "Year", ylab = "Value ($)", col="red")
  title(main = "Timelines")
  lines(my_table$year, my_table$fixed_contrib, col="green")
  lines(my_table$year, my_table$growing_contrib, col="blue")
  legend(0, max(my_table$growing_contrib), legend = c("no contribution", "fixed contribution", "growing contribution"), fill = c("red", "green", "blue"))
}
facet_data <- function(init, return, years, annual, growth) {
  a <- c(rep("No Contribution", years), rep("Fixed Contribution", years), rep("Growing Contribution", years))
  #color <- c(rep("red", years), rep("green", years), rep("blue", years))
  b <- rep(1:years, 3)
  
  l = list(type=a, year=b)
  
  for(t in 1:years) {
    fv <- future_value(init, return/100, t)
    fc <- fv + annuity(annual, return/100, t)
    fva <- fv + growing_annuity(annual, return/100, growth/100, t)
    l$c[t] = fv
    l$c[t+years] = fc
    l$c[t+years+years] = fva
  }
  print(l)
  return(l)
}
facet_plot <- function(init, return, years, annual, growth) {
  df <- data.frame(facet_data(init, return, years, annual, growth))
  color_list <- c(rep("red", years), rep("green", years), rep("blue", years))
  color_list <- factor(color_list)
  df$type <- factor(df$type, levels = c("No Contribution", "Fixed Contribution", "Growing Contribution") )
  ggplot(df)  + geom_line(aes(year, c, color=type)) + geom_area(aes(year, c, fill = type)) + ylim(0, max(df$c)) + facet_wrap(.~type) + ggtitle("Timelines") + labs(x = "year", y = "balance")
}


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$df <- renderTable({
    plot_data(input$init, input$return, input$years, input$annual, input$growth)
  })
   
   output$distPlot <- renderPlot({
     if (input$facet == "No") {
       non_facet_plot(input$init, input$return, input$years, input$annual, input$growth)
     } else {
       facet_plot(input$init, input$return, input$years, input$annual, input$growth)
     }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

