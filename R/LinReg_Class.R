library(ggplot2)
library(gridExtra)
#' @title Computes Multiple Linear Regression For Given Formula And Data
#'
#' @description Linreg class calculates the computations required for multiple regression model
#'     using the ordinary least square method. It also provides methods like print, plot,
#'     resid, coef and summary.
#' @field formula formula. 
#' @field data data.frame. 
#' @field est_beta vector. 
#' @field y_pred matrix. 
#' @field resid_e matrix. 
#' @field deg_freed numeric. 
#' @field resid_var_e matrix. 
#' @field var_est_beta vector. 
#' @field t_val_beta matrix. 
#' @field data_set character. 
#'
#' @exportClass linreg
#' @import ggplot2
#' @import gridExtra
#'
linreg <- setRefClass('linreg',
                           fields = list(
                             formula = 'formula',
                             data = 'data.frame',
                             est_beta = 'vector',
                             y_pred = 'matrix',
                             resid_e = 'matrix',
                             deg_freed = 'numeric',
                             resid_var_e = 'matrix',
                             var_est_beta = 'vector',
                             t_val_beta = 'matrix',
                             data_set = 'character'
                           ),
                           methods=list(
                             initialize=function(formula = formula(),
                                                 data = data.frame())
                                                 {
                                                   .self[['formula']] <<- formula
                                                   .self[['data']] <<- data

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
                                                   data_set_f <- deparse(substitute(data))

                                                   #Assigning to the class attributes
                                                   .self[['est_beta']] <<- est_beta_f
                                                   .self[['y_pred']] <<- round(y_pred_f, 2)
                                                   .self[['resid_e']] <<- resid_e_f
                                                   .self[['deg_freed']] <<- deg_freed_f
                                                   .self[['resid_var_e']] <<- resid_var_e_f
                                                   .self[['var_est_beta']] <<- var_est_beta_f
                                                   .self[['t_val_beta']] <<- t_val_f
                                                   .self[['data_set']] <<- data_set_f
                             },
                             pred = function(){
                               return(y_pred)
                             },
                             print = function(){
                               cat(paste('linreg(formula = ', format(formula), 
                                         ',', ' data = ', data_set, ')\n\n', sep = ''))
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
                                 ggtitle('Scale Location') +
                                 theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = 'white', color = 'black'))+
                                 stat_summary(fun=mean, colour="red", geom="line", aes(group = 1))
                               grid.arrange(plot_1, plot_2, nrow = 1)
                             },
                             resid = function(){
                               return(c(resid_e))
                             },
                             summary = function(){
                               sse <- sum(resid_e ** 2)
                               k <- length(est_beta)-1
                               n = length(resid_e)
                               stand_e <- round(sqrt(sse/(n-(k+1))),4)
                               # coef_matrix <- matrix(ncol = 5)
                               coef_matrix <- matrix(est_beta)
                               # dimnames(coef_matrix) <- list(names(est_beta))
                               std_e <- round(sqrt(var_est_beta),5)
                               p_val <- 2*pt(q = abs(t_val_beta), df = (as.numeric(deg_freed)), lower.tail = FALSE)
                               sign_code_v <- p_val
                               sign_code_v[sign_code_v >0 & sign_code_v <0.001] <- '***'
                               sign_code_v[sign_code_v >0.001 & sign_code_v <0.01] <- '**'
                               sign_code_v[sign_code_v >0.01 & sign_code_v <0.05] <- '*'
                               sign_code_v[sign_code_v >0.05 & sign_code_v <0.1] <- '.'
                               sign_code_v[sign_code_v >0.1 & sign_code_v <1] <- ''
                               coef_matrix <- cbind(coef_matrix, std_e, round(t_val_beta, 2), p_val, sign_code_v)
                               colnames(coef_matrix) <- c('Estimate', 'Std. Error', 't value', 'Pr(>|t|)', 
                                                          'Sign Code')
                               print.default(coef_matrix, quote = FALSE)
                               cat("\nResidual standard error:",
                                   format(stand_e), "on", deg_freed, "degrees of freedom")
                             }))
