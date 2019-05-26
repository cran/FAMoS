## ----setup, include = FALSE, echo = FALSE--------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----createdata, include = F, results='hide',warning=FALSE---------------
library(FAMoS)

#setting data
true.p2 <- 3
true.p5 <- 2
sim.data <- cbind.data.frame(range = 1:10, 
                             y = true.p2^2 * (1:10)^2 - exp(true.p5 * (1:10)))

#define initial parameter values and corresponding test function
inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 0)

cost_function <- function(parms, binary, data){
  if(max(abs(parms)) > 5){
    return(NA)
  }
  with(as.list(c(parms)), {
    res <- p1*4 + p2^2*data$range^2 + p3*sin(data$range) + p4*data$range - exp(p5*data$range)
    diff <- sum((res - data$y)^2)
    
    #calculate AICC
    nr.par <- length(which(binary == 1))
    nr.data <- nrow(data)
    AICC <- diff + 2*nr.par + 2*nr.par*(nr.par + 1)/(nr.data - nr.par -1)
    
    return(AICC)
  })
}


#set swap set
swaps <- list(c("p1", "p5"))

#perform model selection
res <- famos(init.par = inits,
             fit.fn = cost_function,
             homedir = tempdir(),
             method = "swap",
             swap.parameters = swaps,
             init.model.type = c("p1", "p3"),
             optim.runs = 1,
             data = sim.data,
             future.off = TRUE) 

## ----createdata2, eval=FALSE---------------------------------------------
#  library(FAMoS)
#  
#  #setting data
#  true.p2 <- 3
#  true.p5 <- 2
#  sim.data <- cbind.data.frame(range = 1:10,
#                               y = true.p2^2 * (1:10)^2 - exp(true.p5 * (1:10)))
#  
#  #define initial parameter values and corresponding test function
#  inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 0)
#  
#  cost_function <- function(parms, binary, data){
#    if(max(abs(parms)) > 5){
#      return(NA)
#    }
#    with(as.list(c(parms)), {
#      res <- p1*4 + p2^2*data$range^2 + p3*sin(data$range) + p4*data$range - exp(p5*data$range)
#      diff <- sum((res - data$y)^2)
#  
#      #calculate AICC
#      nr.par <- length(which(binary == 1))
#      nr.data <- nrow(data)
#      AICC <- diff + 2*nr.par + 2*nr.par*(nr.par + 1)/(nr.data - nr.par -1)
#  
#      return(AICC)
#    })
#  }
#  
#  
#  #set swap set
#  swaps <- list(c("p1", "p5"))
#  
#  #perform model selection
#  res <- famos(init.par = inits,
#               fit.fn = cost_function,
#               homedir = tempdir(),
#               method = "swap",
#               swap.parameters = swaps,
#               init.model.type = c("p1", "p3"),
#               optim.runs = 1,
#               data = sim.data,
#               future.off = TRUE)

## ----famosperformance, echo = T, fig.width = 4, fig.align= "center", fig.height=6----
famos.performance(input = res$mrun, path = tempdir())

## ----sc.order, echo = T, fig.width = 4, fig.align= "center", fig.height=4----
 fig.sc <- sc.order(input = tempdir(), mrun = res$mrun)

## ----sc.order2,echo = T, fig.width = 8, fig.align= "center", fig.height=4----
 par(mfrow = c(1,2))
 fig.sc1 <- sc.order(input = tempdir(), mrun = res$mrun, colour.par = "p1")
 fig.sc2 <- sc.order(input = tempdir(), mrun = res$mrun, colour.par = "p5")


## ----aicc.weights,echo = T, fig.width = 4, fig.align= "center", fig.height=4----
fig.aicc <- aicc.weights(input = tempdir(), mrun = res$mrun)

