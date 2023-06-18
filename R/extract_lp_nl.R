# -------------------------------------------------------------------------------
# `extract_lp_nl()` extracts the impulse response vector, along with the upper 
# and lower confidence interval vectors, created by the `lp_nl()` and `lp_nl_iv()` 
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


extract_lp_nl <- function(results_nl, return.list=TRUE){
  
  if (!(grepl("lpirfs_nl", class(results_nl)))){
    stop("this function only accepts non-linear 'lpirf' class objects \n please try extract_lp_lin() instead")
  }
  
  specs   <- results_nl$specs
  
  if(specs$model_type == 0){
    
    irf_s1_mean <- results_nl[[1]]
    irf_s1_low  <- results_nl[[2]]
    irf_s1_up   <- results_nl[[3]]
    
    irf_s2_mean <- results_nl[[4]]
    irf_s2_low  <- results_nl[[5]]
    irf_s2_up   <- results_nl[[6]]
    
    list_nl_s1      <- rep(list(NaN), specs$endog*specs$endog)
    list_nl_s2      <- rep(list(NaN), specs$endog*specs$endog)
    
    list_nl_s1_name      <- rep(list(NaN), specs$endog*specs$endog)
    list_nl_s2_name      <- rep(list(NaN), specs$endog*specs$endog)
    
    res_num   <- 1
    
    
    for(rr in 1:(specs$endog)){
      for (ss in 1:(specs$endog)){
        
        # Tibbles for expansion irfs
        tbl_s1_mean <- as.matrix(t(irf_s1_mean[,  1:specs$hor , ss]))[, rr]
        tbl_s1_low  <- as.matrix(t(irf_s1_low[,   1:specs$hor , ss]))[, rr]
        tbl_s1_up   <- as.matrix(t(irf_s1_up[,    1:specs$hor , ss]))[, rr]
        
        tbl_s1_name <- paste(specs$column_names[ss], 'on', specs$column_names[rr], sep=" ")
        
        tbl_s1     <- data.frame(horizon   = 1:specs$hor,  mean = tbl_s1_mean,
                                 lower = tbl_s1_low,   upper   = tbl_s1_up)
        
        tbl_s1$impluse_response <- tbl_s1_name
        tbl_s1$class <- class(results_nl)
        tbl_s1$regime <- 1
        
        # Tibbles for recessions irfs
        tbl_s1_mean <- as.matrix(t(irf_s2_mean[,  1:specs$hor , ss]))[, rr]
        tbl_s2_low  <- as.matrix(t(irf_s2_low[,   1:specs$hor , ss]))[, rr]
        tbl_s2_up   <- as.matrix(t(irf_s2_up[,    1:specs$hor , ss]))[, rr]
        
        tbl_s2_name <- paste(specs$column_names[ss], 'on', specs$column_names[rr], sep=" ")
        
        tbl_s2      <- data.frame(horizon   = 1:specs$hor,  mean   = tbl_s1_mean,
                                  lower  = tbl_s2_low,   upper     = tbl_s2_up)
        
        tbl_s2$impluse_response <- tbl_s2_name
        tbl_s2$class <- class(results_nl)
        tbl_s2$regime <- 2
        
        list_nl_s1[[res_num]] <- tbl_s1
        list_nl_s1_name[[res_num]] <- tbl_s1_name
        
        list_nl_s2[[res_num]] <- tbl_s2
        list_nl_s2_name[[res_num]] <- tbl_s2_name
        
        res_num <- res_num + 1
        
        
      }
      
      # names(list_nl_s1) <- list_nl_s1_name
      # names(list_nl_s2) <- list_nl_s2_name
      
    }
  } else if(specs$model_type == 1| specs$model_type == 2){
    


    list_nl_s1        <- rep(list(NaN), specs$endog)
    list_nl_s2        <- rep(list(NaN), specs$endog)
    
    list_nl_s1_name      <- rep(list(NaN), specs$endog)
    list_nl_s2_name      <- rep(list(NaN), specs$endog)
    
    # res_num     <- 1

    for(rr in 1:(specs$endog)){

      # Tibbles for expansion irfs
      tbl_s1_mean <-  results_nl$irf_s1_mean[rr, ]
      tbl_s1_low  <-  results_nl$irf_s1_low[rr, ]
      tbl_s1_up   <-  results_nl$irf_s1_up[rr, ]

      tbl_s1_name <- paste('Shock', 'on', specs$column_names[rr], sep=" ")
      
      tbl_s1      <- data.frame(horizon   = 1:specs$hor,  mean = tbl_s1_mean,
                                lower = tbl_s1_low,   upper   = tbl_s1_up)
      
      tbl_s1$impluse_response <- tbl_s1_name
      tbl_s1$class <- class(results_nl)
      tbl_s1$regime <- 1

      # Tibbles for recessions irfs
      tbl_s2_mean <- results_nl$irf_s2_mean[rr, ]
      tbl_s2_low  <- results_nl$irf_s2_low[rr, ]
      tbl_s2_up   <- results_nl$irf_s2_up[rr, ]

      tbl_s2_name <- paste('Shock', 'on', specs$column_names[rr], sep=" ")
      
      tbl_s2      <- data.frame(horizon   = 1:specs$hor,  mean   = tbl_s2_mean,
                                lower = tbl_s2_low,   upper     = tbl_s2_up)
      
      tbl_s2$impluse_response <- tbl_s2_name
      tbl_s2$class <- class(results_nl)
      tbl_s2$regime <- 2


      list_nl_s1[[rr]]      <- tbl_s1 
      list_nl_s1_name[[rr]] <- tbl_s1_name
      
      list_nl_s2[[rr]]      <- tbl_s2
      list_nl_s2_name[[rr]] <- tbl_s2_name  

    }
    
    # names(list_nl_s1) <- list_nl_s1_name
    # names(list_nl_s2) <- list_nl_s2_name
    
  }
  
  names(list_nl_s1) <- list_nl_s1_name
  names(list_nl_s2) <- list_nl_s2_name
  
  if(return.list==FALSE){
    bind_rows(list (regime_1 = bind_rows(list_nl_s1), regime_2 = bind_rows(list_nl_s2)))
    
  }else{
    
    list (regime_1 = list_nl_s1, regime_2 = list_nl_s2)
  }
  
}