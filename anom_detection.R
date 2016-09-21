tagList(
  fluidRow(
    column(width=12,includeMarkdown('anom_detection.md'))
  ),
  fluidRow(
    column(width=4,selectInput("ranges_known",label="Operating Range Known?",
               choices = list("Yes" = 1, "No" = 2), selected = 2)),
    column(width=4,uiOutput("minoprange")),
    column(width=4,uiOutput("maxoprange"))
  ),
  p("*If the normal operating range is known, adjust the value using the sliders to the left. Otherwise, values are set automatically using a range within two standard deviations of the mean."),
  br(),
  actionButton("startsim","Start"),
  #actionButton("stopsim","Stop!"),
  plotlyOutput("timeseries",height="300px",width="100%"),
  br()

)
