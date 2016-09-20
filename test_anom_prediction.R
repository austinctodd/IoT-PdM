# library(reshape2)
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
boundedMarkovChain <- function(offset,
                               maxStepSize,
                               upperBound,
                               lowerBound,
                               pdfvals,
                               n_sec=86400,
                               full_output=FALSE){
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
        if (full_output){
          bmc <- append(bmc,val)
          times <- append(times,i)
        } else {
          if (mod(i,max(c(round(n_sec/1000),1)))==0){
            bmc <- append(bmc,val)
            times <- append(times,i)
          }
        }
  }
  return(list("times"=times,"bmc"=bmc,"bmchist"=bmchist))
}

#---------------------------------------------------
# Add functionality for plotting data using dygraph
#---------------------------------------------------

# Some Info to construct Bounded Markov Chain
stepSize=2.0 #log10(input$n_sec)
sigma=stepSize*7.5
offset = 25
bmc <- boundedMarkovChain(offset,
                          maxStepSize=5.5,
                          upperBound=100.0,
                          lowerBound=0.0,
                          make_temp_pdf(offset,sigma),
                          n_sec=86400,
                          full_output=TRUE
                          )

# Create dataframe for time series plots
mc <- data.frame(cbind(pltdata()$times,pltdata()$bmc))
colnames(mc) <- c("times","bmc")

#Calculate basic statistics (mean, std)
tmean<-sum(pltdata()$bmchist*seq(0.05, 99.95, 0.1))/sum(pltdata()$bmchist)
tstd <-sqrt(sum(pltdata()$bmchist*((seq(0.05, 99.95, 0.1)-tmean)**2))/sum(pltdata()$bmchist))

# Set up some plotting params
tlims<-c(max(c( 1.0,min(which(pltdata()$bmchist>0),(tmean-tstd*2)*10)/10.0-0.5)),
         min(c(99.9,max(which(pltdata()$bmchist>0),(tmean+tstd*2)*10)/10.0+0.5)))

# Plot time series data
ggplot(mc, aes(x=times,y=bmc)) +
      geom_hline(yintercept=tmean+tstd*2,color='blue',linetype="dashed")+
      geom_hline(yintercept=tmean-tstd*2,color='blue',linetype="dashed")+
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


# Run test -> must simulate sensor data. Run first, plot as if "real time"
# Generate new data and test if within normal operating range
bmc2 <- boundedMarkovChain(bmc$bmc[length(bmc$bmc)],
                           maxStepSize=5.5,
                           upperBound=100.0,
                           lowerBound=0.0,
                           make_temp_pdf(offset,sigma),
                           n_sec=7200, #2 hours
                           full_output=TRUE
    )
