data_from_max_min <- function(x){
  data.frame(x_z = seq(x$min, x$max, length.out = new_data_rows))
}

make_new_data <- function(x, new_data_rows = 100){
  
  ## Get ranges
  max_z <- x %>% summarise_all(funs(max = as.numeric(max(.)))) %>% 
    gather(key = label, value = max) %>% 
    mutate(xlab = sub(pattern = "_max", x = label, replacement = "")) %>% 
    select(-label)

  min_z <- x %>% summarise_all(funs(min = as.numeric(min(.)))) %>% 
    gather(key = label, value = min) %>% 
    mutate(xlab = sub(pattern = "_min", x = label, replacement = "")) %>% 
    select(-label)
  
  max_min_z <- inner_join(max_z, min_z)
  
  ## Create new data
  new_data <- max_min_z %>% 
    group_by(xlab) %>% 
    do(data_from_max_min(.)) %>% ungroup() %>% 
    mutate(id = 1:n())
  
  return(new_data)

}

make_median_data <- function(x, new_data_rows = 100){
  
  ## Get median values
  med_z <- new_dat_cols %>% summarise_all(funs(med = as.numeric(median(.))))
  
  ## Assemble median dataframe
  median_data <- data.frame(dist_pomo_z = med_z$dist_shore_z_med, 
                            SI_LONG_z = med_z$SI_LONG_z_med, 
                            SI_LATI_z = med_z$SI_LATI_z_med, 
                            dist_shore_z = med_z$dist_shore_z_med, 
                            depth_z = med_z$depth_z_med, 
                            slope_z = med_z$slope_z_med, 
                            persist_sum_z = med_z$persist_sum_z_med)
  
  # Replicate the dataframe n times
  median_data <- do.call("rbind", replicate(new_data_rows, median_data, simplify = F)) %>% tbl_df()
  
  return(median_data)
  
}

make_pred_df <- function(new_data, median_data, new_predictor){
  
  ## Get new x_values for prediction
  new_df <- new_data %>% filter(xlab == new_predictor) %>% 
    spread(key = xlab, value = x_z) %>% select(-id)
  ## Select all columns but the new_predictor in the median dataframe
  new_median_df <- median_data %>% select_(paste("-", new_predictor, ""))
  ## Now add the new x values to the median df
  new_df <- cbind(new_df, new_median_df)
  ## Add reserve_timing column
  new_df <- rbind(new_df, new_df) %>%
    mutate(reserve_timing = c(rep("before", new_data_rows), 
                              rep("after", new_data_rows))) %>% 
    mutate(reserve_timing = factor(reserve_timing, levels = c("before", "after")))
  
  return(new_df)
}

predict_gam_tidy <- function(new_df, new_predictor){
  
  ## Make predictions
  preds <- predict(m_check, type = "terms", newdata = new_df, se.fit = TRUE)
  
  ### Get estimates
  fit_pred <- as.data.frame(preds$fit)
  ## Get relevant columns
  fit_pred <- fit_pred %>% select(contains(new_predictor)) %>% tbl_df()
  ## Add these columns (overall coefficient plus the interaction coefficient)
  Estimate_i = as.numeric(rowSums(fit_pred))
  
  ### Get standard errors
  se_pred <- as.data.frame(preds$se)
  ## Get relevant columns
  se_pred <- se_pred %>% select(contains(new_predictor)) %>% tbl_df()
  ## Add these columns (overall coefficient plus the interaction coefficient)
  SE_i = as.numeric(rowSums(se_pred))
  
  ### Get x_z (the predictor)
  x_z <- new_df %>% select_(new_predictor)
  names(x_z) <- "x_z"
  
  ### Update new_df
  new_df2 <- new_df %>% 
    mutate(Estimate = Estimate_i, 
           SE = SE_i, 
           uci = Estimate + SE*2, 
           lci = Estimate - SE*2, 
           xlab = paste(new_predictor))
  
  new_df2 <- cbind(x_z, new_df2)
  
  return(new_df2)
  
}
