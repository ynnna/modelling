#Relative validity

model<- function (df) {
  
  n <- nrow(df)
  #Initiating variables
  v_A_US <- delta_v_A_US <- numeric(n)
  v_B_US <- delta_v_B_US <- numeric(n)
  v_C_US <- delta_v_C_US <- numeric(n)
  
  v_US_A <- delta_v_US_A <- numeric(n)
  v_US_B <- delta_v_US_B <- numeric(n)
  v_US_C <- delta_v_US_C <- numeric(n)
  
  delta_v_A_B <- delta_v_B_A <- delta_v_B_C <- delta_v_C_B<- delta_v_A_C <- delta_v_C_A<- numeric(n)
  v_A_B<- v_B_A <- v_B_C <- v_C_B<- v_A_C <- v_C_A <- numeric(n)
  
  
  
  #Initiating deltas
  # A
  # A-US 
  if (df$trial_type[1] %in% c('AB+', 'AB-')) {
    delta_v_A_US[1] <- df$alpha_A[1]*(df$beta[1] - (v_A_US[1]+ v_B_US[1]))
  } else { #AC+ AC-
    delta_v_A_US[1] <- df$alpha_A[1]*(df$beta[1] - (v_A_US[1]+ v_C_US[1]))
  }
  # US-A
  if (df$trial_type[1] == 'AB+') {
    delta_v_US_A[1] <- df$beta[1]* (df$alpha_A[1] - (v_US_A[1] + v_B_A[1]))
  } else if (df$trial_type[1] == 'AC+') {
     delta_v_US_A[1] <- df$beta[1]* (df$alpha_A[1] - (v_US_A[1] + v_C_A[1]))
  } else {
    delta_v_US_A[1] <- 0
  }
  
  # B
  # B-US
  if (df$trial_type[1] %in% c('AB+', 'AB-')) {
  delta_v_B_US[1] <- df$alpha_B[1]*(df$beta[1] - (v_A_US[1]+ v_B_US[1]))
  } else { #AC+ AC-
    delta_v_B_US[1] <- 0
  }
    
  # US-B
  if (df$trial_type[1] == 'AB+') {
    delta_v_US_B[1] <- df$beta[1]* (df$alpha_B[1] - (v_A_B[1] + v_US_B[1]))
  } else if (df$trial_type[1] == ('AC+')) {
    delta_v_US_B[1] <- df$beta[1]* (df$alpha_B[1] - (v_A_B[1]  +  v_C_B[1] + v_US_B[1]))
  } else { # AB- AC-
    delta_v_US_B[1] <- 0
  }
  # C
  # C-US
    if (df$trial_type[1] %in% c('AC+', 'AC-')) {
      delta_v_C_US[1] <- df$alpha_C[1]*(df$beta[1] - (v_A_US[1]+ v_C_US[1]))
    } else { #AB+ AB-
      delta_v_C_US[1] <- 0
    }
  
  # US-C
  if (df$trial_type[1] == 'AC+') {
    delta_v_US_C[1] <- df$beta[1]* (df$alpha_C[1] - (v_A_C[1]+ v_US_C[1]))
  } else if (df$trial_type[1] == ('AB+')) {
    delta_v_US_C[1] <- df$beta[1]* (df$alpha_C[1] - ( v_A_C[1] + v_B_C[1] + v_US_C[1]))
  } else { # AB- AC-
    delta_v_US_C[1] <- 0
  }
  

  #########################
  #Between elements
  # A - B
  if (df$trial_type[1] == 'AB+') {
    delta_v_A_B[1]  <- df$alpha_A[1] * (df$alpha_B[1] - (v_A_B[1] + v_US_B[1]))
  } else if  (df$trial_type[1] == 'AB-') {
    delta_v_A_B[1]  <- df$alpha_A[1] * (df$alpha_B[1] - v_A_B[1])
  } else if  (df$trial_type[1] == 'AC+') {
    delta_v_A_B[1]  <- df$alpha_A[1] * (df$alpha_B[1] - (v_A_B[1] + v_C_B[1] +v_US_B[1]))
  } else { #AC-
    delta_v_A_B[1]  <- df$alpha_A[1] * (df$alpha_B[1] - (v_A_B[1] + v_C_B[1]))
  }
  
  
  # B - A
  if (df$trial_type[1] == 'AB+') {
    delta_v_B_A[1]  <- df$alpha_B[1] * (df$alpha_A[1] - (v_B_A[1] + v_US_A[1]))
  } else if  (df$trial_type[1] == 'AB-') {
    delta_v_B_A[1]  <- df$alpha_B[1] * (df$alpha_A[1] - v_B_A[1])
  } else  { #AC+ AC-
    delta_v_B_A[1]  <- 0
  }
  
 
  # B - C
  if (df$trial_type[1] == 'AB+') {
    delta_v_B_C[1]  <- df$alpha_B[1] * (df$alpha_C[1] - (v_A_C[1] + v_B_C[1] + v_US_C[1]))
  } else if  (df$trial_type[1] == 'AB-') {
    delta_v_B_C[1]  <- df$alpha_B[1] * (df$alpha_C[1] - (v_A_C[1] + v_B_C[1])) 
  } else {#AC+ AC-
    delta_v_B_C[1]  <- 0
  }
  
  # C - B
    if  (df$trial_type[1] == 'AC+') {
    delta_v_C_B[1]  <- df$alpha_C[1] * (df$alpha_B[1] - (v_A_B[1] + v_C_B[1] + v_US_B[1]))
  } else if (df$trial_type[1] == 'AC-') { 
    delta_v_C_B[1]  <- df$alpha_C[1] * (df$alpha_B[1] - (v_A_B[1] + v_C_B[1]))
  } else { #AB+ AB-
    delta_v_C_B[1] <- 0
  }
  

  # A - C
  if (df$trial_type[1] == 'AB+') {
    delta_v_A_C[1]  <- df$alpha_A[1] * (df$alpha_C[1] - (v_A_C[1] + v_B_C[1] + v_US_C[1]))
  } else if  (df$trial_type[1] == 'AB-') {
    delta_v_A_C[1]  <- df$alpha_A[1] * (df$alpha_C[1] - (v_A_C[1]+ v_B_C[1])) 
  } else if  (df$trial_type[1] == 'AC+') {
    delta_v_A_C[1]  <- df$alpha_A[1] * (df$alpha_C[1] -  (v_A_C[1] + v_US_C[1])) 
  } else { #AC-
    delta_v_A_C[1]  <- df$alpha_A[1] * (df$alpha_C[1] - v_A_C[1]) 
  }
  
  # C - A
 if  (df$trial_type[1] == 'AC+') {
    delta_v_C_A[1]  <- df$alpha_C[1] * (df$alpha_A[1] - (v_C_A[1] + v_US_A[1]))
  } else if (df$trial_type[1] == 'AC-')  {
    delta_v_C_A[1]  <- df$alpha_C[1] * (df$alpha_A[1] - v_C_A[1])
  } else { #AB+ AB-
    delta_v_C_A[1] <- 0
  }
  
  
  #From here down
  ########################################################
  for(i in seq_len(n)[-1]){
    
    v_A_US[i] <- v_A_US[i - 1] + delta_v_A_US[i - 1]
    v_US_A[i] <- v_US_A[i - 1] + delta_v_US_A[i - 1]
    v_B_US[i] <- v_B_US[i - 1] + delta_v_B_US[i - 1]
    v_US_B[i] <- v_US_B[i - 1] + delta_v_US_B[i - 1]
    v_C_US[i] <- v_C_US[i - 1] + delta_v_C_US[i - 1]
    v_US_C[i] <- v_US_C[i - 1] + delta_v_US_C[i - 1]
    
    v_A_B[i]  <- v_A_B[i - 1] + delta_v_A_B[i - 1]
    v_B_A[i]  <- v_B_A[i - 1] + delta_v_B_A[i - 1]
    v_B_C[i]  <- v_B_C[i - 1] + delta_v_B_C[i - 1]
    v_C_B[i]  <- v_C_B[i - 1] + delta_v_C_B[i - 1]
    v_A_C[i]  <- v_A_C[i - 1] + delta_v_A_C[i - 1]
    v_C_A[i]  <- v_C_A[i - 1] + delta_v_C_A[i - 1]
    
    
    # A
    # A-US 
    if (df$trial_type[i] %in% c('AB+', 'AB-')) {
      delta_v_A_US[i] <- df$alpha_A[i]*(df$beta[i] - (v_A_US[i]+ v_B_US[i]))
    } else { #AC+ AC-
      delta_v_A_US[i] <- df$alpha_A[i]*(df$beta[i] - (v_A_US[i]+ v_C_US[i]))
    }
    # US-A
    if (df$trial_type[i] == 'AB+') {
      delta_v_US_A[i] <- df$beta[i]* (df$alpha_A[i] - (v_US_A[i] + v_B_A[i]))
    } else if (df$trial_type[i] == 'AC+') {
      delta_v_US_A[i] <- df$beta[i]* (df$alpha_A[i] - (v_US_A[i] + v_C_A[i]))
    } else {
      delta_v_US_A[i] <- 0
    }
    
    # B
    # B-US
    if (df$trial_type[i] %in% c('AB+', 'AB-')) {
      delta_v_B_US[i] <- df$alpha_B[i]*(df$beta[i] - (v_B_US[i]+ v_A_US[i]))
    } else { #AC+ AC-
      delta_v_B_US[i] <- 0
    }
    
    # US-B
    if (df$trial_type[i] == 'AB+') {
      delta_v_US_B[i] <- df$beta[i]* (df$alpha_B[i] - (v_A_B[i] + v_US_B[i]))
    } else if (df$trial_type[i] == ('AC+')) {
      delta_v_US_B[i] <- df$beta[i]* (df$alpha_B[i] - (v_A_B[i]  +  v_C_B[i] + v_US_B[i]))
    } else { # AB- AC-
      delta_v_US_B[i] <- 0
    }
    
    # C-US
    if (df$trial_type[i] %in% c('AC+', 'AC-')) {
      delta_v_C_US[i] <- df$alpha_C[i]*(df$beta[i] - (v_A_US[i]+ v_C_US[i]))
    } else {
      delta_v_C_US[i] <- 0
    }
    
    # US-C
    if (df$trial_type[i] == 'AC+') {
      delta_v_US_C[i] <- df$beta[i]* (df$alpha_C[i] - (v_A_C[i]+ v_US_C[i]))
    } else if (df$trial_type[i] == ('AB+')) {
      delta_v_US_C[i] <- df$beta[i]* (df$alpha_C[i] - ( v_A_C[i] + v_B_C[i] + v_US_C[i]))
    } else { # AB- AC-
      delta_v_US_C[i] <- 0
    }
    
    
    #########################
    #Between elements
    # A - B
    if (df$trial_type[i] == 'AB+') {
      delta_v_A_B[i]  <- df$alpha_A[i] * (df$alpha_B[i] - (v_A_B[i] + v_US_B[i]))
    } else if  (df$trial_type[i] == 'AB-') {
      delta_v_A_B[i]  <- df$alpha_A[i] * (df$alpha_B[i] - v_A_B[i])
    } else if  (df$trial_type[i] == 'AC+') {
      delta_v_A_B[i]  <- df$alpha_A[i] * (df$alpha_B[i] - (v_A_B[i] + v_C_B[i] +v_US_B[i]))
    } else { #AC-
      delta_v_A_B[i]  <- df$alpha_A[i] * (df$alpha_B[i] - (v_A_B[i] + v_C_B[i]))
    }
    
    
    # B - A
    if (df$trial_type[i] == 'AB+') {
      delta_v_B_A[i]  <- df$alpha_B[i] * (df$alpha_A[i] - (v_B_A[i] + v_US_A[i]))
    } else if  (df$trial_type[i] == 'AB-') {
      delta_v_B_A[i]  <- df$alpha_B[i] * (df$alpha_A[i] - v_B_A[i])
    } else  { #AC+ AC-
      delta_v_B_A[i]  <- 0
    }
    
    
    # B - C
    if (df$trial_type[i] == 'AB+') {
      delta_v_B_C[i]  <- df$alpha_B[i] * (df$alpha_C[i] - (v_A_C[i] + v_B_C[i] + v_US_C[i]))
    } else if  (df$trial_type[i] == 'AB-') {
      delta_v_B_C[i]  <- df$alpha_B[i] * (df$alpha_C[i] - (v_A_C[i] + v_B_C[i])) 
    } else {#AC+ AC-
      delta_v_B_C[i]  <- 0
    }
    
    # C - B
    if  (df$trial_type[i] == 'AC+') {
      delta_v_C_B[i]  <- df$alpha_C[i] * (df$alpha_B[i] - (v_A_B[i] + v_C_B[i] + v_US_B[i]))
    } else if (df$trial_type[i] == 'AC-') { 
      delta_v_C_B[i]  <- df$alpha_C[i] * (df$alpha_B[i] - (v_A_B[i] + v_C_B[i]))
    } else { #AB+ AB-
      delta_v_C_B[i] <- 0
    }
    
    
    # A - C
    if (df$trial_type[i] == 'AB+') {
      delta_v_A_C[i]  <- df$alpha_A[i] * (df$alpha_C[i] - (v_A_C[i] + v_B_C[i] + v_US_C[i]))
    } else if  (df$trial_type[i] == 'AB-') {
      delta_v_A_C[i]  <- df$alpha_A[i] * (df$alpha_C[i] - (v_A_C[i]+ v_B_C[i])) 
    } else if  (df$trial_type[i] == 'AC+') {
      delta_v_A_C[i]  <- df$alpha_A[i] * (df$alpha_C[i] -  (v_A_C[i] + v_US_C[i])) 
    } else { #AC-
      delta_v_A_C[i]  <- df$alpha_A[i] * (df$alpha_C[i] - v_A_C[i]) 
    }
    
    # C - A
    
    if  (df$trial_type[i] == 'AC+') {
      delta_v_C_A[i]  <- df$alpha_C[i] * (df$alpha_A[i] - (v_C_A[i] + v_US_A[i]))
    } else if (df$trial_type[i] == 'AC-')  {
      delta_v_C_A[i]  <- df$alpha_C[i] * (df$alpha_A[i] - v_C_A[i])
    } else { #AB+ AB-
      delta_v_C_A[i] <- 0
    }
    
  }
  
  
  
  df <- data.frame(df, v_A_US, delta_v_A_US, v_US_A, delta_v_US_A, 
                       v_B_US, delta_v_B_US, v_US_B, delta_v_US_B,
                       v_C_US, delta_v_C_US, v_US_C, delta_v_US_C,
                       v_A_B, delta_v_A_B,   v_B_A, delta_v_B_A,
                       v_B_C, delta_v_B_C,   v_C_B, delta_v_C_B,
                       v_A_C, delta_v_A_C,   v_C_A, delta_v_C_A )
}

#Transform model.multiple.2.cs.us to long
to.long<- function (df){
  df_long<- gather(df, ass_indicator, value_ass, - trial, -alpha_A, -alpha_B, -alpha_C, -beta, -trial_type, -discrimination)
  df_long$ass_indicator <- as.factor(df_long$ass_indicator)
  df_long<- as.data.frame(df_long)
  return(df_long) 
}

#Label model
label.df<- function (df_long) {
   df_long$ass_indicator<- ordered(df_long$ass_indicator, 
                                  levels= c( 'v_A_US', 'delta_v_A_US', 'v_US_A', 'delta_v_US_A', 
                                             'v_B_US', 'delta_v_B_US', 'v_US_B', 'delta_v_US_B',
                                             'v_C_US', 'delta_v_C_US', 'v_US_C', 'delta_v_US_C',
                                             'v_A_B', 'delta_v_A_B',   'v_B_A', 'delta_v_B_A',
                                             'v_B_C', 'delta_v_B_C',   'v_C_B', 'delta_v_C_B',
                                             'v_A_C', 'delta_v_A_C',   'v_C_A', 'delta_v_C_A'),
                                  labels= c('A-US', 'delta  V A-US', 'US-A', 'delta  V US-A', 
                                            'B-US', 'delta  V B-US', 'US-B', 'delta  V US-B',
                                            'C-US', 'delta  V C-US', 'US-C', 'delta  V US-C',
                                             'A-B', 'delta V A-B', 'B-A', 'delta V B-A',
                                             'B-C', 'delta V B-C', 'C-B', 'delta V C-B',
                                             'A-C', 'delta V A-C', 'C-A', 'delta V C-A'
                                            ))
  return(df_long)
}




