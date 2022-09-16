linreg <- function(formula, data){
  #Create the design matrix X. 
  #The first attribute is the independent variable in formula.
  X <- model.matrix(formula, data)
  #Get the dependent matrix y
  y <- as.matrix(iris[,all.vars(formula)[1]])
  
  #Calculating the equations using ordinary least squares
  est_beta <- solve(t(X)%*%X)%*%(t(X)%*% y) #Regressions coefficient
  y_pred <- X %*% est_beta #Fitted Values
  resid_e <- y-y_pred #Residuals
  
  #Creating the final linregClass object to be returned
  linreg_obj <- linregClass$new(est_beta = est_beta,
                                y_pred = y_pred,
                                resid_e = resid_e)
  return(linreg_obj)
}

linreg_obj <- linreg(Petal.Length~Species, data = iris)
linreg_obj$est_beta
