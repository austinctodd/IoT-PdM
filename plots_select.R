tagList(
  br(),
  fluidRow(
    column(width=4,'Select the variables you wish to plot and click "Update Plot"'),
    column(width=5,selectInput("plotvars",label=NULL, choices=colnames(data), selected = "temperature", multiple = TRUE)),
    column(width=3,actionButton("makeplot", " Update Plot", icon("line-chart"), 
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  ),
  br(),
  fluidRow(
    dygraphOutput("dygraph")
  ),
  br(),
  fluidRow(
    "To zoom in on a selected part of the trip, either use the levers to adjust the window at the bottom of the figure, 
     or click and drag the mouse across the desired zoom region. Reset the zoom to the entire trip length 
     by double-clicking in the plot window. "
  ),br(),
  br()
)
