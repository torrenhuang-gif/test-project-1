library(shiny)
library(TwoSampleMR)
library(ggplot2)

dat <- readRDS("data/dat.rds")   # 启动时读一次，全局可用

ui <- fluidPage(
  titlePanel("教育程度 → 阿尔兹海默症：MR 诊断面板"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("methods", "估计方法",
                         choices = c("IVW"            = "mr_ivw",
                                     "MR Egger"       = "mr_egger_regression",
                                     "Weighted median"= "mr_weighted_median",
                                     "Weighted mode"  = "mr_weighted_mode"),
                         selected = c("mr_ivw", "mr_weighted_median")),
      sliderInput("pthresh", "工具筛选 p 值阈值",
                  min = -10, max = -5, value = -8, step = 0.5,
                  pre = "1e")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("散点图",       plotOutput("scatter", height = "500px")),
        tabPanel("森林图(单SNP)", plotOutput("forest",  height = "600px")),
        tabPanel("漏斗图",       plotOutput("funnel",  height = "500px")),
        tabPanel("Leave-one-out", plotOutput("loo",    height = "600px")),
        tabPanel("统计量", verbatimTextOutput("stats"))
      )
    )
  )
)

server <- function(input, output) {
  
  # 所有“重算”都基于本地 168 行数据 → 瞬时完成
  dat_sub <- reactive({
    subset(dat, pval.exposure < 10^input$pthresh)
  })
  
  res <- reactive({
    mr(dat_sub(), method_list = input$methods)
  })
  
  output$scatter <- renderPlot({
    mr_scatter_plot(res(), dat_sub())[[1]]   # 注意：返回的是 list，取 [[1]]
  })
  
  output$forest <- renderPlot({
    mr_forest_plot(mr_singlesnp(dat_sub()))[[1]]
  })
  
  output$funnel <- renderPlot({
    mr_funnel_plot(mr_singlesnp(dat_sub()))[[1]]
  })
  
  output$loo <- renderPlot({
    mr_leaveoneout_plot(mr_leaveoneout(dat_sub()))[[1]]
  })
  
  output$stats <- renderPrint({
    cat("=== 异质性 ===\n");  print(mr_heterogeneity(dat_sub()))
    cat("\n=== Egger 截距 ===\n"); print(mr_pleiotropy_test(dat_sub()))
    cat("\n=== 主结果 ===\n");  print(res())
  })
}

shinyApp(ui, server)
