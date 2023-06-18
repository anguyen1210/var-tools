# -------------------------------------------------------------------------------
# `extract_lp_lin()` extracts the impulse response vector, along with the upper 
# and lower confidence interval vectors, created by the `lp_lin()` and `lp_lin_iv()`
# function in the `lpirfs` and puts them into a tidy dataframe that allows for 
# customization of the impulse-response function plotting.
#
# For more information on `lpirfs` by Philipp Adämmer, please consult: 
# https://github.com/AdaemmerP/lpirfs
#
# For additional details for this helper function, please consult:
# https://mentalbreaks.rbind.io/posts/extract_lpirfs
# 
# @anguyen1210
# -------------------------------------------------------------------------------


extract_lp_lin <- function(results_lin, return.list=TRUE){
  
  if (!(grepl("lpirfs_lin", class(results_lin)))){
    stop("this function only accepts linear 'lpirf' class objects \n please try extract_lp_nl() instead")
  }
  
  irf_lin_mean <- results_lin[[1]]
  irf_lin_low  <- results_lin[[2]]
  irf_lin_up   <- results_lin[[3]]
  
  specs        <- results_lin$specs
  
  # Plots for lin function
  if(specs$model_type == 0){
    
    res_num        <- 1
    list_lin       <- rep(list(NaN), specs$endog*specs$endog)
    list_lin_name  <- rep(list(NaN), specs$endog*specs$endog)
    
    # Loop to fill to create plots
    for(rr in 1:(specs$endog)){
      for (ss in 1:(specs$endog)){
        
        # Tibbles for linear irfS
        tbl_lin_mean <- as.matrix(t(irf_lin_mean[, 1:specs$hor , ss]))[, rr]
        tbl_lin_low  <- as.matrix(t(irf_lin_low[,  1:specs$hor , ss]))[, rr]
        tbl_lin_up   <- as.matrix(t(irf_lin_up[,   1:specs$hor , ss]))[, rr]
        
        tbl_name    <- paste(specs$column_names[ss], 'on', specs$column_names[rr], sep=" ")
        
        tbl_lin      <- tibble(horizon     = seq_along(tbl_lin_mean),  mean = tbl_lin_mean,
                               lower   = tbl_lin_low, upper = tbl_lin_up)
        
        tbl_lin$impluse_response <- tbl_name
        tbl_lin$class <- class(results_lin)
        
        list_lin[[res_num]] <- tbl_lin
        list_lin_name[[res_num]] <- tbl_name
        
        # Add one to count variable
        res_num     <- res_num + 1
        
      }
    }
    
  } else if(specs$model_type == 1| specs$model_type == 2){
    
    list_lin       <- rep(list(NaN), specs$endog)
    list_lin_name <- rep(list(NaN), specs$endog)
    
    # Loop to fill to create plots
    for(rr in 1:(specs$endog)){
      
      # Tibbles for linear irfS
      tbl_lin_mean <- irf_lin_mean[rr, ]
      tbl_lin_low  <- irf_lin_low[rr, ]
      tbl_lin_up   <- irf_lin_up[rr, ]
      
      tbl_name     <- paste('Shock', 'on', specs$column_names[rr], sep=" ")
      tbl_lin      <- tibble(horizon     = seq_along(tbl_lin_mean),  mean = tbl_lin_mean,     # 1:(specs$hor)
                             lower   = tbl_lin_low,    upper   = tbl_lin_up)
      
      tbl_lin$impluse_response <- tbl_name
      tbl_lin$class            <- class(results_lin)
      
      list_lin[[rr]]           <- tbl_lin
      list_lin_name[[rr]]     <- tbl_name
      
      }
    }
  
  names(list_lin) <- list_lin_name
  
  if(return.list==FALSE){
    
    bind_rows(list_lin)
    
  } else {
    
    return(list_lin)
    
  }
  
}