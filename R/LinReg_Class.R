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
                        }
                      ))
