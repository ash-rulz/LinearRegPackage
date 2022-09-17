#' Title
#'
#' @field est_beta vector.
#' @field y_pred matrix.
#' @field resid_e matrix.
#' @field deg_freed numeric.
#' @field resid_var_e matrix.
#' @field var_est_beta vector.
#' @field t_val_beta matrix.
#'
#' @return
#' @export
#' @import ggplot2
#' @import gridExtra
#'
#' @examples
linregClass <- setRefClass('linregClass',
                           fields = list(
                             formula = 'formula',
                             data = 'data.frame',
                             est_beta = 'vector',
                             y_pred = 'matrix',
                             resid_e = 'matrix',
                             deg_freed = 'numeric',
                             resid_var_e = 'matrix',
                             var_est_beta = 'vector',
                             t_val_beta = 'matrix'
                           ),
                           methods=list(
                             initialize=function(formula = formula(),
                                                 data = data.frame()) 
                                                 {
                                                   .self$formula <<- formula
                                                   .self$data <<- data
                                                   #Create the design matrix X.
                                                   #The first attribute is the independent variable in formula.
                                                   X <- model.matrix(formula, data)
                                                   #Get the dependent matrix y
                                                   y <- as.matrix(data[,all.vars(formula)[1]])
                                                   
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
                                                   
                                                   #Assigning to the class attributes
                                                   .self$est_beta <<- est_beta_f
                                                   .self$y_pred <<- y_pred_f
                                                   .self$resid_e <<- resid_e_f
                                                   .self$deg_freed <<- deg_freed_f
                                                   .self$resid_var_e <<- resid_var_e_f
                                                   .self$var_est_beta <<- var_est_beta_f
                                                   .self$t_val_beta <<- t_val_f
                             },
                             pred = function(){
                               return(y_pred)
                             },
                             print = function(){
                               cat("Coefficients:\n")
                               print.default(format(est_beta, digits = 2),
                                             print.gap = 2L, quote = FALSE)
                             },
                             coef = function(){
                               return(est_beta)
                             },
                             plot = function(){
                               df1 <- data.frame(Fitted_Values = y_pred,
                                                 Residuals = resid_e)
                               plot_1 <- ggplot(df1,
                                                aes(x=Fitted_Values, y=Residuals, group = 1)) +
                                 geom_point(size=2.5, shape = 1) +
                                 ggtitle('Residuals vs Fitted') +
                                 xlab(paste('Fitted Values \n','linreg(', format(formula),')'))+
                                 ylab('Residuals')+
                                 theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = 'white', color = 'black')) +
                                 geom_smooth(method = "lm",
                                             linetype = "dotted",
                                             se = FALSE)+
                                 stat_summary(fun=median, colour="red", geom="line", aes(group = 1))
                               
                               
                               stand_e <- sqrt(abs((resid_e - mean(resid_e))/sd(resid_e)))
                               df2 <- data.frame(Fitted_Value = y_pred, Standardized_Residuals = stand_e)
                               plot_2 <- ggplot(df2,
                                                aes(x=Fitted_Value, y=Standardized_Residuals, group = 1)) +
                                 geom_point(size=2.5, shape = 1) +
                                 xlab(paste('Fitted Values \n','linreg(', format(formula),')'))+
                                 ylab(expression(sqrt("|Standardized Residual|")))+
                                 ggtitle('Scale−Location') +
                                 theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = 'white', color = 'black'))+
                                 stat_summary(fun=mean, colour="red", geom="line", aes(group = 1))
                               grid.arrange(plot_1, plot_2, nrow = 1)
                             },
                             resid = function(){
                               return(c(resid_e))
                             }))
linobj <- linregClass$new(Petal.Length~Species, data = iris)
linobj$formula
linobj$data
linobj$est_beta
linobj$y_pred                                    
linobj$resid_e                                   
linobj$deg_freed                                 
linobj$resid_var_e                               
linobj$var_est_beta                              
linobj$t_val_beta                                
linobj$print()
linobj$plot()
linobj$resid()
linobj$pred()
linobj$summary()
