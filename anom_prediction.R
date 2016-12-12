tagList(
  fluidRow(
    column(width=12,includeMarkdown('anom_prediction.md'))
  ),
  fluidRow(
    column(width=5,selectInput("trendkind",label="Select Trend type",
                               choices = list("Linear" = 1,"ANIMA"=2),
                               selected = 1)),
    column(width=3,numericInput("trenddegree","Degree Polynomial",1,min=1,max=25)),
    column(width=4,height=14,tags$b("Forecast"),br(),actionButton("forecast","Forecast"))
  ),
  p("Select the degree of the polynomial for linear regression."),
  br(),  
  fluidRow(
    column(width=12,plotOutput("trendforecast",height="300px",width="100%"))
  ),
  br()
)
