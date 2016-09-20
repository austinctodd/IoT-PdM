# Create 'PDF' for determining the change in values based on temperature
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
  val <- offset
  for (i in 0:n_sec-1){
    # Add next value as dependent on current state
    val <- val + (runif(1,-0.5,.5)*maxStepSize*(1.01-pdfvals[round(val)+1]))
    # Check bounds 
    val <- min(c(max(c(val,lowerBound)),upperBound))
    bmc <- append(bmc,val)
  }
  return(bmc)
}

# Some Info to construct Bounded Markov Chain
stepSize=2.0
maxStepSize=5.5
lowerBound=0.0
upperBound=100.0
sigma = stepSize*7.5
offset = 25
n_sec = 100

pdfvals <-make_temp_pdf(offset,sigma)
bmc <- boundedMarkovChain(offset,maxStepSize,upperBound,lowerBound,pdfvals,n_sec)
