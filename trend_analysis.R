tagList(
  fluidRow(
    column(width=12,includeMarkdown('trend_analysis.md'))
  ),
  fluidRow(
    column(width=5,selectInput("trendtype",label="Select Trend type",
                choices = list("Linear" = 1),
                selected = 1)),
    column(width=3,numericInput("trendorder","Degree Polynomial",1))
  ),
#  fluidRow(
#    column(width=4,selectInput("ranges_known",label="Operating Range Known?",
#               choices = list("Yes" = 1, "No" = 2), selected = 2)),
#    column(width=4,uiOutput("minoprange")),
#    column(width=4,uiOutput("maxoprange"))
#  ),
  p("*If the normal operating range is known, adjust the value using the sliders to the left. Otherwise, values are set automatically using a range within two standard deviations of the mean."),
  br(),  
  fluidRow(
    column(width=12,plotlyOutput("trendline",height="300px",width="100%"))
  ),

#  actionButton("startsim","Start"),
#  actionButton("stopsim","Stop!"),
#  plotlyOutput("timeseries",height="300px",width="100%"),
  br()

)
