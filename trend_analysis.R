tagList(
  fluidRow(
    column(width=12,includeMarkdown('trend_analysis.md'))
  ),
  fluidRow(
    column(width=3,numericInput("trendorder","Degree Polynomial",1,min=1,max=25)),
    column(width=2,radioButtons(inputId = "fcaston",
                                label = "Forecast",
                                choices = list("On"=1,"Off"=2),
                                selected = 2)),
    column(width=5,selectInput("trendtype",label="Select forecast type",
                               choices = list("Extrapolation" = 1,"ARIMA"=2),
                               selected = 1)),
    column(width=2,height=14,tags$b("New data"),br(),actionButton("trendsim","Fetch"))
  ),
  p("Select the degree of the polynomial for linear regression."),
  br(),  
  fluidRow(
    column(width=12,plotlyOutput("trendline",height="300px",width="100%"))
  ),
  br()
)
