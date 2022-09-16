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
  est_beta <- solve(t(X) %*% X) %*% (t(X) %*% y)
  pred_y <- X %*% est_beta
  resid_e <- y - pred_y
  deg_freed <- length(y) - length(est_beta)
  resid_var_e <- (t(resid_e) %*% resid_e)/deg_freed
  var_est_beta <- as.vector(resid_var_e) * diag(solve(t(X) %*% X))
  t_val <- (est_beta/(sqrt(var_est_beta)))

  est_beta <- as.vector(est_beta)
  names(est_beta) <- colnames(X)

  #Creating the final linregClass object to be returned
  linreg_obj <- linregClass$new(est_beta = est_beta,
                                y_pred = y_pred,
                                resid_e = resid_e)
  return(linreg_obj)
}

linreg_obj <- linreg(Petal.Length~Species, data = iris)
linreg_obj$est_beta


X <- model.matrix(Petal.Length~Species, iris)
#Get the dependent matrix y
y <- as.matrix(iris[,all.vars(Petal.Length~Species)[1]])
