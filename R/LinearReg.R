linreg <- function(formula, data){
  formula_f <- formula
  data_f <- data
  #Create the design matrix X.
  #The first attribute is the independent variable in formula.
  X <- model.matrix(formula, data)
  #Get the dependent matrix y
  y <- as.matrix(iris[,all.vars(formula)[1]])

  #Calculating the equations using ordinary least squares
  est_beta_f <- solve(t(X)%*%X)%*%(t(X)%*% y) #Regressions coefficient
  y_pred_f <- X %*% est_beta_f #Fitted Values
  resid_e_f <- y - y_pred_f #Residuals
  deg_freed_f <- length(y) - length(est_beta_f)
  resid_var_e_f <- (t(resid_e_f) %*% resid_e_f)/deg_freed_f
  # resid_var_e_f <- as.vector(resid_var_e_f)
  var_est_beta_f <- c(resid_var_e_f) * diag(solve(t(X) %*% X))
  var_est_beta_f <- as.vector(var_est_beta_f)
  t_val_f <- (est_beta_f/(sqrt(var_est_beta_f)))

  est_beta_f <- as.vector(est_beta_f)
  names(est_beta_f) <- colnames(X)

  #Creating the final linregClass object to be returned
  linreg_obj <- linregClass$new(est_beta = est_beta_f,
                                y_pred = y_pred_f,
                                resid_e = resid_e_f,
                                deg_freed = deg_freed_f,
                                resid_var_e = resid_var_e_f,
                                var_est_beta = var_est_beta_f,
                                t_val_beta = t_val_f,
                                formula = formula_f,
                                data = data_f
                                )
  return(linreg_obj)
}

linreg_obj <- linreg(Petal.Length~Species, data = iris)
linreg_obj$t_val_beta
linreg_obj$pred()
linreg_obj$print()
linreg_obj$plot()
linreg_obj$resid()

iris
X <- model.matrix(Petal.Length~Species, iris)
X
#Get the dependent matrix y
y <- as.matrix(iris[,all.vars(Petal.Length~Species)[1]])
y

#Calculating the equations using ordinary least squares
est_beta_f <- solve(t(X)%*%X)%*%(t(X)%*% y) #Regressions coefficient
y_pred_f <- X %*% est_beta_f #Fitted Values
resid_e_f <- y - y_pred_f #Residuals
resid_e_f
