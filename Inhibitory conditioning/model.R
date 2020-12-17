# Inhibitory conditioning

c<-1


model<- function (df) {
  
  n <- nrow(df)
  #Initiating variables
  v_A_US <- delta_v_A_US <- numeric(n)
  v_B_US <- delta_v_B_US <- numeric(n)
  v_US_A <- delta_v_US_A <- numeric(n)
  v_US_B <- delta_v_US_B <- numeric(n)
  v_A<- v_B <- v_AB <- numeric(n)
  v_AB_US<-v_US_AB<- numeric(n)
  
  #Initiating deltas
  delta_v_A_US[1] <- df$alpha_A[1]*(df$beta[1] - (v_A_US[1]+ v_B_US[1]))
  delta_v_US_A[1] <- df$beta[1]* (df$alpha_A[1] - v_US_A[1])
  
  delta_v_B_US[1] <- df$alpha_B[1]*(df$beta[1] - (v_B_US[1]+ v_A_US[1]))
  delta_v_US_B[1] <- df$beta[1]* (df$alpha_B[1] - v_US_B[1])
  
  for(i in seq_len(n)[-1]){
    #A
    v_A_US[i] <- v_A_US[i - 1] + delta_v_A_US[i - 1]
    v_B_US[i] <- v_B_US[i - 1] + delta_v_B_US[i - 1]
    
    
    if (df$trial_type[i]== 'AB-') {
      delta_v_A_US[i] <- df$alpha_A[i]*(c* df$beta[i] - (v_A_US[i]+ v_B_US[i]))
    } else {
      delta_v_A_US[i] <- df$alpha_A[i]*(c* df$beta[i] - v_A_US[i])
    }
    
    v_US_A[i] <- v_US_A[i - 1] + delta_v_US_A[i - 1]
    delta_v_US_A[i] <- df$beta[i]*(c* df$alpha_A[i] - v_US_A[i])
    
    
    #B
    
    if (df$trial_type[i]== 'AB-') {
      delta_v_B_US[i] <- df$alpha_B[i]*(c* df$beta[i] - (v_B_US[i]+ v_A_US[i]))
    } else {
      delta_v_B_US[i] <- df$alpha_B[i]*(c* df$beta[i] - v_B_US[i]) 
    }
    
      v_US_B[i] <- v_US_B[i - 1] + delta_v_US_B[i - 1]
      delta_v_US_B[i] <- df$beta[i]*(c* df$alpha_B[i] - v_US_B[i])
    
    v_A[i]<- v_A_US[i] + (1/c * v_A_US[i] * v_US_A[i])
    v_B[i]<- v_B_US[i] + (1/c * v_B_US[i] * v_US_B[i])
    v_AB_US[i]<- v_A_US[i] + v_B_US[i]
    v_US_AB[i]<- v_US_A[i] + v_US_B[i]
    v_AB[i]<- (v_A_US[i] + v_B_US[i]) + (1/c * (v_A_US[i] + v_B_US[i]) * (v_US_A[i]+ v_US_B[i]))
  }
  
  
  
  df <- data.frame(df, v_A_US, delta_v_A_US, v_US_A, delta_v_US_A, v_A,
                   v_B_US, delta_v_B_US, v_US_B, delta_v_US_B, v_B, v_AB)
  df[is.na(df)] <- 0
  
  df<-df%>% mutate(cs_oriented_A = (df$alpha_A / (df$alpha_A + abs(df$v_A_US))) * df$v_A,
                   cs_oriented_B = (df$alpha_B / (df$alpha_B + abs(df$v_B_US))) * abs(df$v_B),
                   us_oriented_A = (abs(df$v_A_US) / (abs(df$v_A_US) + df$alpha_A)) * df$v_A,
                   us_oriented_B = (abs(df$v_B_US) / (abs(df$v_B_US) + df$alpha_B)) * df$v_B,
                   cs_oriented_AB = (df$alpha_A + df$alpha_B) / ((df$alpha_A + df$alpha_B)+ abs(df$v_A_US + df$v_B_US)) * df$v_AB,
                   us_oriented_AB = abs(df$v_A_US + df$v_B_US) / (abs(df$v_A_US + df$v_B_US) + (df$alpha_A + df$alpha_B))  * df$v_AB)
  
  
  }
  
#Transform model.multiple.2 to long
to.long<- function (df){
  df$trial<- c(1: n_trial)
  
  df_long<- gather(df, ass_indicator, value_ass, - trial, -alpha_A, -alpha_B, -beta, -trial_type, -cs_oriented_AB, -cs_oriented_A, -cs_oriented_B, 
                   -us_oriented_AB, -us_oriented_A, -us_oriented_B)
  df_long<- gather(df_long, response_type, value_response, - trial, -alpha_A, -alpha_B, -beta, -trial_type, -ass_indicator, -value_ass)

  
  return(df_long) 
}

#Label model.multiple.2 long
label.df<- function (df_long) {
  df_long$response_type<- ordered(df_long$response_type, levels=c( 'cs_oriented_A', 'cs_oriented_B',  
                                                                   'us_oriented_A', 'us_oriented_B', 
                                                                   'cs_oriented_AB', 'us_oriented_AB'), 
                                                        labels= c('CS-oriented (A)', 'CS-oriented (B)',  
                                                                  'US-oriented (A)', 'US-oriented (B)', 
                                                                  'CS-oriented (AB)', 'US-oriented (AB)'))
  
  
  
  df_long$ass_indicator<- ordered(df_long$ass_indicator, 
                                  levels= c('v_A_US', 'delta_v_A_US', 'v_US_A', 'delta_v_US_A', 
                                             'v_B_US', 'delta_v_B_US', 'v_US_B', 'delta_v_US_B', 
                                             'v_A', 'v_B', 'v_AB'),
                                  labels= c('A-US', 'delta  V A-US', 'US-A', 'delta  V US-A', 
                                             'B-US', 'delta  V B-US', 'US-B', 'delta  V US-B', 
                                             'Σ A', 'Σ B', 'Σ AB'))
  return(df_long)
}
