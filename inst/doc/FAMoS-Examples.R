## ----setup, include = TRUE, echo = FALSE--------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ----globalode, eval = FALSE--------------------------------------------------
# #define the global model dynamics
# global.dynamics <- function(t,x,parms){
#   with(as.list(c(x, parms)), {
# 
#     dA <- rho_A*A + mu_BA*B + mu_CA*C - (mu_AB + mu_AC)*C
#     dB <- rho_B*B + mu_AB*A + mu_CB*C - (mu_BA + mu_BC)*B
#     dC <- rho_C*C + mu_AC*A + mu_BC*B - (mu_CA + mu_CB)*C
# 
#     return(list(c(dA,dB,dC)))
#    }
#   )
# }
# 

## ----odesim, eval = FALSE-----------------------------------------------------
# #define simulation parameter set
# pars <- c(rho_A = 0, rho_B = 0, rho_C = 0.1,
#           mu_AB = 0.2, mu_AC = 0, mu_BA = 0,
#           mu_CA = 0, mu_BC = 0.05, mu_CB = 0)
# 
# #set initial values for cells
# init.vals <- c(A = 100, B = 0, C = 0)
# 
# #simulate data
# library(deSolve)
# 
# sim.data <- lsoda(y = init.vals,
#                   times = 0:10,
#                   func = global.dynamics,
#                   parms = pars)
# 

## ----costode, eval = FALSE----------------------------------------------------
# #cost function for famos
# cost.function <- function(parms, binary, data, inits){
#   #simulate the data with the current parameter set
#   fit.data <- lsoda(y = inits,
#                     times = 0:10,
#                     func = global.dynamics,
#                     parms = parms)
#   #calculate the aic
#   ls2 <- sum((sim.data[,-1] - fit.data[,-1])^2)
#   out.aic <- ls2 + 2*sum(binary == 1)
# 
#   return(out.aic)
# }
# 

## ----odefamos, eval = FALSE---------------------------------------------------
# 
# library(FAMoS)
# 
# start.vals <- c(rho_A = 0.1, rho_B = 0.1, rho_C = 0.1,
#                 mu_AB = 0.1, mu_AC = 0.1, mu_BA = 0.1,
#                 mu_CA = 0.1, mu_BC = 0.1, mu_CB = 0.1)
# 
# famos.fit <- famos(init.par = start.vals,
#                    fit.fn = cost.function,
#                    homedir = tempdir(),
#                    init.model.type = c("rho_A"),
#                    data = sim.data,
#                    inits = init.vals)
# 
# print(famos.fit)
# 

## ----bwt, eval = FALSE--------------------------------------------------------
# library(MASS)
# 
# attach(birthwt)
# race <- factor(race, labels = c("white","black","other"))
# ptd <- factor(ptl > 0)
# levels(ftv)[-c(1:2)] <- "2+"
# bwt <- data.frame(low = factor(low), age, lwt, race,
#                   smoke = (smoke>0), ptd, ht = (ht>0), ui =(ui>0), ftv)
# detach();rm(race,ptd,ftv)
# 

## ----glmfittingfunction, eval = FALSE-----------------------------------------
# fit_func <- function(parms, data, binary){
#   #First transform the parameter names into a formula.
#   #The to-be-fitted parameters are identified using the binary vector
#   fitted.pars <- names(parms[which(binary == 1)])
#   glm_formula <- as.formula(
#     paste0("low ~ ", paste0(fitted.pars, collapse = "+"))
#   )
#   #fit the logistic model using glm
#   out <- summary(
#     glm(glm_formula,
#         family = binomial(link = logit),
#         data = data
#     )
#   )
# 
#   #prepare output parameters, in this case only the names of the fitted
#   #parameters are relevant. As FAMoS also expects values with these, we will just
#   #return the value 1 for all parameters
#   out.par <- rep(1, length(fitted.pars))
#   names(out.par) <- fitted.pars
# 
#   #return a list containing the first the selection criterion value and second the
#   #vector of the fitted parameters
#   return(list(SC = out$aic, params = out.par))
# }

## ----glminits, eval = FALSE---------------------------------------------------
# #first we define the available parameters. As the values are not important, we
# #will just set them equal to 1.
# inits <- c(age = 1, lwt = 1, race = 1, smoke = 1, ptd = 1, ht = 1, ui = 1, ftv = 1)
# #Now, we calculate all possible interactions and name the corresponding vector
# #entries accordingly
# combinations <- combn(names(inits),2)
# for(i in 1:ncol(combinations)){
#   inits <- c(inits,1)
#   names(inits)[i + 8] <- paste0(combinations[1,i],":",combinations[2,i])
# }

## ----glmfamos, eval = FALSE---------------------------------------------------
# library(FAMoS)
# famos.glm <- famos(init.par = inits,
#                    fit.fn = fit_func,
#                    init.model.type = names(inits[1:8]),
#                    data = bwt,
#                    use.optim = FALSE,
#                    optim.runs = 1)

