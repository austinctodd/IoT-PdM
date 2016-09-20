tagList(
  fluidRow(
    column(width=12,includeMarkdown('anom_detection.md'))
  ),
  fluidRow(
    column(width=6,selectInput("ranges_known",label="Normal Operating Range Known?", 
                               choices = list("Yes" = 1, "No" = 2), selected = 2)),
    #        column(width=6,sliderInput("slider2", "Operating range (C)", min = 0, max = 100, value = c(12, 35)))
    column(width=6,uiOutput("opranges"))
  ),
  p("*If the normal operating range is known, adjust the value using the sliders to the left. Otherwise, values are set automatically using a range within two standard deviations of the mean."),
  br(),
  actionButton("startsim","Start"),
  actionButton("stopsim","Stop!"),
  plotOutput("testplot",height="300px",width="100%"),
  br()
)
