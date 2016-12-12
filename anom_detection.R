tagList(
  fluidRow(
    column(width=12,includeMarkdown('anom_detection.md'))
  ),
  fluidRow(
    column(width=4,selectInput("ranges_known",label="Operating Range Known?",
               choices = list("Yes" = 1, "No" = 2), selected = 2)),
    column(width=3,uiOutput("minoprange")),
    column(width=3,uiOutput("maxoprange")),
    column(width=2,tags$b("New data"),actionButton("startsim","Fetch"))
  ),
  p("*If the normal operating range is known, adjust the value using the sliders to the left. Otherwise, values are set automatically using a range within two standard deviations of the mean."),
  br(),
  #actionButton("startsim","Start"),
  #actionButton("stopsim","Stop!"),
  fluidRow(
    column(width=8,plotlyOutput("timeseries",height="300px",width="100%")),
    column(width=4,DT::dataTableOutput('tabledata'))
  ),
  br()

)
