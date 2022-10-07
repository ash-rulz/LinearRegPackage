#' Ridge Regression
#'
#' @field formula formula.
#' @field data data.frame.
#' @field lambda numeric.
#' @field beta_ridge vector.
#' @field X matrix.
#' @field y_pred matrix.
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
                          data_set = 'character',
                          terms = 'function'
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
                            # t_obj <- terms(object)
                            # y_m <- as.matrix(newdata[,all.vars(formula)[1]])
                            # t_terms <- delete.response(t_obj)
                            # m <- model.frame(t_terms, newdata)
                            X_m <- model.matrix(formula, test_data)
                            X_m[,-1] <- scale(X_m[,-1])
                            # beta_ridge_f_m <- solve((t(X_m)%*%X_m)+(diag(ncol(X_m)))*lambda)%*%(t(X_m) %*% y_m)
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
ridgeobj <- ridgereg$new(formula = medv~., data = train_data, lambda = 4)
ridgeobj$formula
ridgeobj$data
ridgeobj$lambda
ridgeobj$X
ridgeobj$beta_ridge
ridgeobj$predict(test_data)
print(ridgeobj)


# library(MASS)
# lm_test <- lm.ridge(formula = Petal.Length~Species, data = iris, lambda = 4)
# lm_test$plot(formula = Petal.Length~Species, data = iris, lambda = 4)


