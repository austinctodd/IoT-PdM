library(markdown)
library(dygraphs)
library(stringr)
library(plyr)
library(dplyr)
library(plotly)

shinyUI(
  navbarPage(
    title="",
    windowTitle="Telemetry Data", collapsible=TRUE, fluid=FALSE,
    id='page',
    tabPanel(
      title = "Graphic Explorer",
      id='graphic_tabs',
      fluidRow(
        column(width = 7, includeMarkdown("intro_graphic.md")),
        column(width = 5, includeMarkdown("intro_logo.html"))
      ),
      fluidRow(
        column(width = 12, "This app is intended to simulate output from sensor data.  The app is intentend to demonstrate the use of several different machine learning algorithms for detecting anomalies in the sensor readings and to predict potential anomalies in the future.")
      ),
      hr(),
      fluidRow(
        column(width=4,numericInput("n_sec","Sensor observation time (in sec)",86400)),
        #column(width=4,numericInput("n_freq","Observation frequency (in sec)",1)),
        column(width=8,br(),actionButton("gettraining", "Generate training data", icon("line-chart"),
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
      ),
      fluidRow(
        column(width=9,plotOutput("trainplot",height="300px",width="100%")),
        column(width=3,plotOutput("trainhist",height="300px",width="100%"))
      ),
      hr(),
      tabsetPanel(
        type = "tabs",
        tabPanel("Smart Monitoring",br(),
          tabsetPanel(
            type="pills",
            selected = NULL,
            tabPanel("Anomaly Detection",br(),source("anom_detection.R",local=TRUE)$value),
            tabPanel("Trend Analysis",br(),source("trend_analysis.R",local=TRUE)$value)
#       tabPanel("PCA",br(),source("anom_PCA.R",local=TRUE)$value)
          )
        ),
        tabPanel("Prediction",br(),source("anom_prediction.R",local=TRUE)$value)
      ),
      br(),
      includeCSS("style.css")
    )
  )

)
