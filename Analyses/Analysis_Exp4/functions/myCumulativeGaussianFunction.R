# My CumulativGaussianFunction
# by Tine - 03.04.2020

#####################################################################
# This is my cumulative gaussian Function

# you need: dplyr

# (1) This is to calculate the Cumulative Gaussians

CG <- function(x= c(20, 30, 40, 50, 60, 70, 80), y){
      p <- y
  
     # make sure there are no 0 or 1, cause these are the asymptodes
     # I dont know if this is necessary
     p <- ifelse(p==0, 0.001 , p)
     p <- ifelse(p ==1 , 0.999, p)
  
  
    # Compute sum of squared residuals to a fit
    f <- function(q) {
          res <- pnorm(x, q[1], q[2]) - p
          sum(res * res)
        }
  
  
  #Find the least squares fit
  coeff <-(fit <- nlm(f, c(50, 15)))$estimate
  
  
  # Calculate the R2
  # but beware: https://statisticsbyjim.com/regression/r-squared-invalid-nonlinear-regression/
  
  res <- pnorm(x, coeff[1], coeff[2]) - p # calculate residuals
  SQR<- sum(res * res)  # residual sum of squares
  SQT<-sum((p-mean(p))^2) # total sum of sqares
  #SQE <-  sum(pnorm(x, coeff[1], coeff[2]) - mean(p))^2)
  
  
  # Calculate the Mean, SD and R2
  data <- c(NA,NA,NA)
  
  data[1]<- coeff[1]
  data[2] <-  coeff[2]
  data[3] <- 1-(SQR/SQT)
  
  return(data)
  
}


# (2) This is the function to use it on datasets together with thy group_by command


myCGfit <- function(df, x = c(20, 30, 40, 50, 60, 70, 80) , y , ...){
          y <- enquo(y)             # bring dv in right format
          groupvar <- quos(...)       # bring groupvariables in right format
  
  # Group dataset and calculate Mean, SD, N, Standard error and CI-Whisker
  df %>% group_by(!!! groupvar) %>% summarise(PSE = CG(x = x , y = !! y)[1],
                                              SD = CG(x = x , y = !! y)[2],
                                              R2 = CG(x = x , y = !! y)[3]) 
    #ungroup -> df

  }


# Anwendung: 
#b <- myCGfit(D, x = unique(D$tML), Resp, Subject, Condition)

