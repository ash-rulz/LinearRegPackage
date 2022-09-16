linreg <- setRefClass('linreg',
                      fields = list(
                        est_beta = 'matrix',
                        y_pred = 'matrix',
                        resid_e = 'matrix',
                        deg_freed = 'numeric',
                        resid_var_e = 'matrix',
                        var_est_beta = 'matrix',
                        t_val_beta = 'matrix'
                      ))
