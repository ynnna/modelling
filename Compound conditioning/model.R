# Compound conditiioning with different associative histories

c<-1

#Model
model<- function (df) {
  
  n <- nrow(df)
  #Initiating variables
  v_A_US <- delta_v_A_US <- numeric(n)
  v_B_US <- delta_v_B_US <- numeric(n)
  v_US_A <- delta_v_US_A <- numeric(n)
  v_US_B <- delta_v_US_B <- numeric(n)
  v_A <- v_B <- numeric(n)
  v_AD_US <- v_US_AD <- v_BC_US <- v_US_BC <- numeric(n)
  
  #History stimuli
  v_C_US <- v_US_C <- v_D_US <- v_US_D<- numeric(n)
  v_AD <- v_BC <- numeric(n)
  
  v_A_US [1] <- prev_beta
  v_B_US [1] <- -(prev_beta)
  v_US_A [1] <- prev_alpha
  
  v_C_US<- rep(prev_beta, n_trial)
  v_US_C<- rep(prev_alpha, n_trial)
  v_D_US<- rep(-prev_beta, n_trial)
  v_US_D<- rep(0, n_trial)
  
  #Initiating deltas
  delta_v_A_US[1] <- df$alpha_A[1]*(df$beta[1] - (v_A_US[1]+ v_B_US[1]))
  delta_v_US_A[1] <- df$beta[1]* (df$alpha_A[1] - v_US_A[1])
  
  delta_v_B_US[1] <- df$alpha_B[1]*(df$beta[1] - (v_B_US[1]+ v_A_US[1]))
  delta_v_US_B[1] <- df$beta[1]* (df$alpha_B[1] - v_US_B[1])
  
  for(i in seq_len(n)[-1]){
    #A
    v_A_US[i] <- v_A_US[i - 1] + delta_v_A_US[i - 1]
    v_B_US[i] <- v_B_US[i - 1] + delta_v_B_US[i - 1]
    
    
    if (df$trial_type[i]== 'AB') {
      delta_v_A_US[i] <- df$alpha_A[i]*(c* df$beta[i] - (v_A_US[i]+ v_B_US[i]))
    } else {
      delta_v_A_US[i] <- df$alpha_A[i]*(c* df$beta[i] - v_A_US[i])
    }
    
    v_US_A[i] <- v_US_A[i - 1] + delta_v_US_A[i - 1]
    delta_v_US_A[i] <- df$beta[i]*(c* df$alpha_A[i] - v_US_A[i])
    
    
    #B
    
    if (df$trial_type[i]== 'AB') {
      delta_v_B_US[i] <- df$alpha_B[i]*(c* df$beta[i] - (v_B_US[i]+ v_A_US[i]))
    } else {
      delta_v_B_US[i] <- df$alpha_B[i]*(c* df$beta[i] - v_B_US[i]) 
    }
    
    v_US_B[i] <- v_US_B[i - 1] + delta_v_US_B[i - 1]
    delta_v_US_B[i] <- df$beta[i]*(c* df$alpha_B[i] - v_US_B[i])
    
    v_A[i]<- v_A_US[i] + (1/c * v_A_US[i] * v_US_A[i])
    v_B[i]<- v_B_US[i] + (1/c * v_B_US[i] * v_US_B[i])
    
    # Compound Vs
    
    v_AD_US[i] <- v_A_US[i] + v_D_US[i]
    v_US_AD[i] <- v_US_A[i] + v_US_D[i]
    v_BC_US[i] <- v_B_US[i] + v_C_US[i]
    v_US_BC[i] <- v_US_B[i] + v_US_C[i]
    v_AD[i] <- v_AD_US[i] + (1/c * v_AD_US[i] * v_US_AD[i])
    v_BC[i] <- v_BC_US[i] + (1/c * v_BC_US[i] * v_US_BC[i])
    
    
  }
  
  
  
  
  df <- data.frame(df, v_A_US, delta_v_A_US, v_US_A, delta_v_US_A, v_A,
                   v_B_US, delta_v_B_US, v_US_B, delta_v_US_B, v_B, 
                   v_C_US, v_US_C, v_D_US, v_US_D,
                   v_AD_US, v_US_AD, v_BC_US, v_US_BC,
                   v_AD, v_BC)
  df[is.na(df)] <- 0
  
  combined_alpha<- alpha_value_A + alpha_value_B
  
  #Performance
  df<-df%>% mutate(cs_oriented_AD = (combined_alpha / (combined_alpha + abs(df$v_AD_US))) * df$v_AD,
                   cs_oriented_BC = (combined_alpha / (combined_alpha + abs(df$v_BC_US))) * df$v_BC,
                   us_oriented_AD = (abs(df$v_AD_US) / (abs(df$v_AD_US) + combined_alpha )) * df$v_AD,
                   us_oriented_BC = (abs(df$v_BC_US) / (abs(df$v_BC_US) + combined_alpha )) * df$v_BC)
  
  
}

#Transform model
to.long<- function (df){
  
  df_long<- gather(df,  ass_indicator, value_ass, -trial, -trial_type,   -alpha_A, -alpha_B, -beta, 
                   -cs_oriented_AD, -cs_oriented_BC, 
                   -us_oriented_AD, -us_oriented_BC)
  df_long<- gather(df_long,response_type, value_response, -trial, -trial_type,   -alpha_A, -alpha_B, -beta, -ass_indicator, -value_ass)
  
  
  return(df_long) 
}

#Labelling function
label.df<- function (df_long) {
  df_long$response_type<- ordered(df_long$response_type, levels=c( 'cs_oriented_AD', 'cs_oriented_BC',  'us_oriented_AD', 'us_oriented_BC'), 
                                  labels= c('CS-oriented (AD)', 'CS-oriented (BC)',  
                                            'US-oriented (AD)', 'US-oriented (BC)'))
  
  
  
  df_long$ass_indicator<- ordered(df_long$ass_indicator, levels= c('v_A_US', 'delta_v_A_US', 'v_US_A', 'delta_v_US_A', 
                                                                   'v_B_US', 'delta_v_B_US', 'v_US_B', 'delta_v_US_B',
                                                                   'v_C_US', 'v_US_C', 'v_D_US', 'v_US_D',
                                                                   'v_AD_US', 'v_US_AD', 'v_BC_US', 'v_US_BC',
                                                                   'v_A', 'v_B', 'v_AD', 'v_BC'),
                                  labels= c('A-US', 'delta  V A-US', 'US-A', 'delta  V US-A', 
                                            'B-US', 'delta  V B-US', 'US-B', 'delta  V US-B',
                                            'C-US', 'US-C', 'D-US', 'US-D',
                                            'AD-US', 'US-AD', 'BC-US', 'US-BC', 
                                            'Σ A', 'Σ B', 'Σ AD', 'Σ BC'))
  return(df_long)
}

