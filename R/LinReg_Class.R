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
                        est_beta = 'vector',
                        y_pred = 'matrix',
                        resid_e = 'matrix',
                        deg_freed = 'numeric',
                        resid_var_e = 'matrix',
                        var_est_beta = 'vector',
                        t_val_beta = 'matrix'
                      ),
                      methods = list(
                        pred = function(){
                          return(y_pred)
                        },
                        print = function(){
                          return(list("Coefficients:" = est_beta))
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
                            theme(plot.title = element_text(hjust = 0.5)) +
                            geom_smooth(method = "lm",
                                        linetype = "dotted",
                                        se = FALSE)+
                            stat_summary(fun.y=median, colour="red", geom="line", aes(group = 1))
                          
                          df2 <- data.frame(Fitted_Values = y_pred,
                                            Standardized_Residuals = sqrt(resid_var_e))
                          plot_2 <- ggplot(df2,
                                           aes(x=Fitted_Values, y=Standardized_Residuals, group = 1)) +
                            geom_point(size=2.5, shape = 1) +
                            ggtitle('Scale−Location') +
                            theme(plot.title = element_text(hjust = 0.5)) 
                            # geom_smooth(method = "lm",
                            #             linetype = "dotted",
                            #             se = FALSE)+
                            # stat_summary(fun.y=median, colour="red", geom="line", aes(group = 1))
                          grid.arrange(plot_1, plot_2, nrow = 1)
                        }
                      ))
