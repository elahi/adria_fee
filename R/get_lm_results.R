################################################################################
##' @title Function to get results from lm()
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2017-02-11
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE)) 

library(broom)

##### FUNCTION TO RETURN LM SUMMARY #####

get_lm_results <- function(df, dep_var, ind_var) {

  n <- dim(df)[1]
  
  # Run model
  mod.i <- lm(as.formula(paste(dep_var, "~", ind_var, sep = " ")), 
               data = df)
  
  # Get dataframe
  mod_results.i <- tidy(mod.i)

  # Estimate the total change in temperature over the study duration
  mod_results.i <- mod_results.i %>%
    mutate(coef_name = c("intercept", "slope"), 
           sample_size = n
           ) 

  return(mod_results.i)
}
