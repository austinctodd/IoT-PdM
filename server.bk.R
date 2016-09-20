library(reshape2)
library(magrittr)
library(saves)
library(shiny)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(dygraphs)
library(xts)

shinyServer(function(input, output, session) {

  devicedata <- reactive({
    if ( is.null(input$deviceid))
      return()
    data[(data$deviceid==input$deviceid),]
  })
  
  #---------------------------------------------------
  # Add functionality for plotting data using dygraph
  #---------------------------------------------------

  # Reactive data for updated time series
  pltdata <- eventReactive(input$makeplot,{
    t <- devicedata()[,c('devicetimestamp',input$plotvars)]
    if (input$plotvars=='temperature')
      t$temperature[t$temperature == 0] <- NaN
    t <- t[complete.cases(t[,c('devicetimestamp',input$plotvars)]),]
    t[,1] <- as.POSIXct(as.POSIXlt(t[,1],format="%Y-%m-%dT%H:%M:%S"))
    xts(t[,-1],order.by=t[,1])
  })
  
  output$dygraph <- renderDygraph({
    dygraph(pltdata()) %>%
      dyHighlight(highlightCircleSize = 5) %>%
      dyRangeSelector() %>%
      dyLegend(width = 600) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
  })
  
  output$from <- renderText({
    if (!is.null(input$dygraph_date_window))
      strftime(input$dygraph_date_window[[1]], "%H:%M:%S")      
  })
  
  output$to <- renderText({
    if (!is.null(input$dygraph_date_window))
      strftime(input$dygraph_date_window[[2]], "%H:%M:%S")
  })  
  
  output$plot_hoverinfo <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
})


