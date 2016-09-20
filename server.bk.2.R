library(reshape2)
library(magrittr)
library(saves)
library(shiny)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(ggplot2)
library(xts)

# Create function for estimating likelihood of greater increase/decrease
make_temp_pdf <- function(offset,sigma){
  pdfvals <- c()
  for (i in 0:101){
    pdfvals <- append(pdfvals, exp(-(i-offset)**2 / (2 * sigma**2)))
  }
  return(pdfvals)
}

# Simulate temperatures for input to test algorithm
boundedMarkovChain <- function(offset,maxStepSize,upperBound,lowerBound,pdfvals,n_sec=86400){
  bmc <- c()
  times <- c()
  bmchist <- integer(1000)
  val <- offset
  for (i in 0:(n_sec-1)){
    # Add next value as dependent on current state
      val <- val + (runif(1,-0.5,.5)*maxStepSize*(1.01-pdfvals[round(val)+1]))
      # Check bounds 
        val <- min(c(max(c(val,lowerBound)),upperBound))
        bmchist[min(c(floor(val*10.0)+1,999))] <- bmchist[min(c(floor(val*10.0)+1,999))]+1
        if (mod(i,max(c(round(n_sec/1000),1)))==0){
          bmc <- append(bmc,val)
          times <- append(times,i)
        }
  }
  return(list("times"=times,"bmc"=bmc,"bmchist"=bmchist))
}

shinyServer(function(input, output, session) {

  #---------------------------------------------------
  # Add functionality for plotting data using dygraph
  #---------------------------------------------------

  # Reactive data for updated time series
  pltdata <- eventReactive(input$gettraining,{
    # Some Info to construct Bounded Markov Chain
    stepSize=2.0 #log10(input$n_sec)
    sigma=stepSize*7.5
    offset = 25
    bmc <- boundedMarkovChain(offset,
                              maxStepSize=5.5,
                              upperBound=100.0,
                              lowerBound=0.0,
                              make_temp_pdf(offset,sigma),
                              n_sec=input$n_sec
                              )
    #data.frame(bmc)
  })

  output$trainplot <- renderPlot({
    # Create dataframe for time series plots
    mc <- data.frame(cbind(pltdata()$times,pltdata()$bmc))
    colnames(mc) <- c("times","bmc")
    
    #Calculate basic statistics (mean, std)
    tmean<-sum(pltdata()$bmchist*seq(0.05, 99.95, 0.1))/sum(pltdata()$bmchist)
    tstd <-sqrt(sum(pltdata()$bmchist*((seq(0.05, 99.95, 0.1)-tmean)**2))/sum(pltdata()$bmchist))
    
    # Set up some plotting params
    tlims<-c(max(c( 1.0,min(which(pltdata()$bmchist>0),(tmean-tstd*2)*10)/10.0-0.5)),
             min(c(99.9,max(which(pltdata()$bmchist>0),(tmean+tstd*2)*10)/10.0+0.5)))
    # At most 20 ticks
    tickints <- c(0.25,0.5,1,5,15,30,60,90,120,150,180,240,300,360,720,1440,2880)
    tickint  <- tickints[min(which(tickints-(tail(mc$times,1)/720.0)>=0))]
    if (tickint < 1.0){
      tickunit <- "sec"
      ticklabs <- tickint
    } else if (tickint > 15.0) {
      tickunit <- "hrs"
      ticklabs <- tickint/60.0
    } else {
      tickunit <- "mins"
      ticklabs <- tickint
    }

    # Plot time series data
    ggplot(mc, aes(x=times,y=bmc)) + 
#      geom_hline(yintercept=tmean,color='blue')+
      geom_hline(yintercept=tmean+tstd*2,color='blue',linetype="dashed")+
      geom_hline(yintercept=tmean-tstd*2,color='blue',linetype="dashed")+
#      annotate("text", label = "Mean", x = tail(mc$times,1)+tickint*60, y = tmean, size = 5, colour = "blue",vjust=-0.5,fontface=3)+
#      annotate("text", label = "Max range", x = tail(mc$times,1)+tickint*60, y = tmean+tstd*2, 
#               size = 5, colour = "blue",vjust=-0.5,hjust=0.75,fontface=3)+
#      annotate("text", label = "Min range", x = tail(mc$times,1)+tickint*60, y = tmean-tstd*2, 
#               size = 5, colour = "blue",vjust=1.25,hjust=0.7,fontface=3)+
      geom_line(aes(y=bmc),color='red') +
      scale_x_continuous(name = paste("Time since start (",tickunit,")"), breaks= 0:12*tickint*60, 
                         labels=0:12*ticklabs,limits=c(0,mc$times[length(mc$times)])) +
      scale_y_continuous(name = "Temperature (C)",limits = tlims) +
      theme_minimal() + # start with a minimal theme and add what we need
      theme(text = element_text(color = "gray10"),
            axis.text = element_text(face = "italic",size=10),
            axis.title.x = element_text(vjust = -3, size=14), # move title away from axis
            axis.title.y = element_text(vjust = -1, size=14),# move away for axis
            panel.grid.major.y=element_line(colour="black", linetype = "dashed"),
            panel.grid.major.x=element_blank()
      )
  })
  output$trainhist <- renderPlot({
    newhist<-data.frame(cbind(seq(0,99.9,0.1),pltdata()$bmchist))
    colnames(newhist) <- c("temp","bmchist")
    tmean<-sum(pltdata()$bmchist*seq(0.05, 99.95, 0.1))/sum(pltdata()$bmchist)
    tstd <-sqrt(sum(pltdata()$bmchist*((seq(0.05, 99.95, 0.1)-tmean)**2))/sum(pltdata()$bmchist))
    tlims<-c(max(c( 1.0,min(which(pltdata()$bmchist>0),(tmean-tstd*2)*10)/10.0-0.5)),
             min(c(99.9,max(which(pltdata()$bmchist>0),(tmean+tstd*2)*10)/10.0+0.5)))
    ggplot(newhist, aes(y=newhist$bmc,x=newhist$temp)) + 
      geom_bar(stat="identity",fill="red",width=0.1)+
      geom_vline(xintercept=tmean-tstd*2,color='blue',linetype="dashed")+
      geom_vline(xintercept=tmean+tstd*2,color='blue',linetype="dashed")+
      annotate("text", label = "Max range", x = tmean+tstd*2, y = max(pltdata()$bmchist), 
               size = 5, colour = "blue",vjust=-0.5,hjust=1,fontface=3)+
      annotate("text", label = "Min range", x = tmean-tstd*2, y = max(pltdata()$bmchist), 
               size = 5, colour = "blue",vjust=1.25,hjust=1,fontface=3)+
      coord_flip()+
      scale_x_continuous(name = "",limits = tlims)  +
      scale_y_continuous(name = "Observation count")  +
      theme_minimal() +
      theme(text = element_text(color = "gray10"),
            axis.text = element_text(face = "italic",size=10),
            axis.title.x = element_text(vjust = -3, size=14), # move title away from axis
            axis.title.y = element_blank(),# move away for axis
            axis.text.y  = element_blank(),# remove y ticks
            panel.grid.major.y=element_line(colour="black", linetype = "dashed"),
            panel.grid.major.x=element_blank()
      )
  })
  output$opranges <- renderUI({
    #Calculate basic statistics (mean, std)
    tmean<-sum(pltdata()$bmchist*seq(0.05, 99.95, 0.1))/sum(pltdata()$bmchist)
    tstd <-sqrt(sum(pltdata()$bmchist*((seq(0.05, 99.95, 0.1)-tmean)**2))/sum(pltdata()$bmchist))
    sliderInput("slider2", "Operating range (C)", min = 0, max = 100, value = c(tmean-(tstd*2), tmean+(tstd*2)))
  })
  
})


