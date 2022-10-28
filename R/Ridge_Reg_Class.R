#' Ridge Regression
#' @title Computes Ridge Regression For Given Formula And Data
#'
#' @description ridgereg class calculates the computations required for multiple 
#'     regression model for ridge regression. It also provides methods like predict, 
#'     plot, coef and show.
#'
#' @field formula Formula for the ridge regression.
#' @field data Data frame containing the dependent and independent variables.
#' @field lambda Number for the lambda value.
#' @field beta_ridge The vector for the coefficients.
#' @field X The model matrix.
#' @field y_pred Matrix containg the predicted values.
#' @field data_set character.
#'
#'
#' @export ridgereg
#' @exportClass ridgereg
#'
#' @examples
ridgereg <- setRefClass('ridgereg',
                        fields = list(
                          formula = 'formula',
                          data = 'data.frame',
                          lambda = 'numeric',
                          beta_ridge = 'vector',
                          X = 'matrix',
                          y_pred = 'matrix',
                          data_set = 'character'
                        ),
                        methods = list(
                          initialize = function(formula = formula(),
                                                data = data.frame(),
                                                lambda = numeric())
                          {
                            .self[['formula']] <<- formula
                            .self[['data']] <<- data
                            .self[['lambda']] <<- lambda

                            data_set <<- deparse(substitute(data))

                            X <<- model.matrix(formula, data)
                            X[,-1] <<- scale(X[,-1])
                            y <- as.matrix(data[,all.vars(formula)[1]])

                            beta_ridge_f <- solve((t(X)%*%X)+(diag(ncol(X)))*lambda)%*%(t(X)%*% y)
                            y_pred_f <- X %*% beta_ridge_f

                            beta_ridge_f <- as.vector(beta_ridge_f)
                            names(beta_ridge_f) <- colnames(X)


                            .self[['beta_ridge']] <<- beta_ridge_f
                            .self[['y_pred']] <<- y_pred_f

                          },
                          predict = function(test_data){
                            X_m <- model.matrix(formula, test_data)
                            X_m[,-1] <- scale(X_m[,-1])
                            y_pred_f_m <- X_m %*% beta_ridge

                            return(y_pred_f_m)
                          },
                          coef = function(){
                            return(beta_ridge)
                          },
                          show = function(){
                            cat(paste('ridgereg(formula = ', format(formula),
                                      ',', ' data = ', data_set, ')\n\n', sep = ''))
                            cat("Coefficients:\n")
                            print(beta_ridge)
                          }
                        ))