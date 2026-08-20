library(shiny)
library(TwoSampleMR)
library(ggplot2)

data <- readRDS("data/dat.rds")

ui <- fluidPage(
  titlePanel("教育程度 → 阿尔兹海默症：MR 诊断面板"),
#  sliderInput("pthresh","select the range of p-value",max = -2, min = -10, step =  0.5, pre = "1e", value = -8),
  plotOutput("scatter"),
  plotOutput("forest"),
  plotOutput("leave"),
  plotOutput("funnel")
)

server <- function(input, output) {
  output$scatter <- renderPlot({
    mr_scatter_plot(result, data)
  })
  
  output$forest <- renderPlot({
    mr_forest_plot(mr_singlesnp(data))
  })
  
  output$leave <- renderPlot({
    mr_leaveoneout_plot(mr_leaveoneout(data))
  })
  
  output$funnel <- renderPlot({
    mr_funnel_plot(mr_singlesnp(data))
  })
}

shinyApp(ui, server)
