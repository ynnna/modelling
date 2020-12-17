#Model used for one CS and one US (Conditioning, Extinction)
c<-1


#Model used for two CS and two US (Downshift unblocking)
model<- function (df) {
  
  n <- nrow(df)
  #Initiating variables for US1 
  v_A_US1 <- delta_v_A_US1 <- numeric(n)
  v_B_US1 <- delta_v_B_US1 <- numeric(n)
  v_US1_A <- delta_v_US1_A <- numeric(n)
  v_US1_B <- delta_v_US1_B <- numeric(n)
  v_sum_A_US1 <- v_sum_A_US2<- v_sum_B_US1 <- v_sum_B_US2 <- numeric(n)
  
  
  #Initiating variables for US2
  v_A_US2 <- delta_v_A_US2 <- numeric(n)
  v_B_US2 <- delta_v_B_US2 <- numeric(n)
  v_US2_A <- delta_v_US2_A <- numeric(n)
  v_US2_B <- delta_v_US2_B <- numeric(n)
  v_A2 <- v_B2 <- numeric(n)
  
  #Initiating variables compound associations
  delta_v_AB<- delta_v_BA<- numeric(n)
  v_AB <- v_BA <- numeric(n)
  
  #Initiating deltas US1
  delta_v_A_US1[1] <- df$alpha_A[1]*(df$beta_1[1] - (v_A_US1[1]+ v_B_US1[1]))
  delta_v_US1_A[1] <- df$beta_1[1]* (df$alpha_A[1] - (v_US1_A[1] + v_US2_A[1]+ v_BA[1]))
  delta_v_B_US1[1] <- df$alpha_B[1]*(df$beta_1[1] - (v_B_US1[1]+ v_A_US1[1]))
  delta_v_US1_B[1] <- df$beta_1[1]* (df$alpha_B[1] - (v_US1_B[1] + v_US2_B[1]+ v_AB[1]))
  
  #Initiating deltas US2
  delta_v_A_US2[1] <- df$alpha_A[1]*(df$beta_2[1] - (v_A_US2[1]+ v_B_US2[1]))
  delta_v_US2_A[1] <- df$beta_2[1]* (df$alpha_A[1] - (v_US1_A[1] + v_US2_A[1]+ v_BA[1]))
  delta_v_B_US2[1] <- df$alpha_B[1]*(df$beta_2[1] - (v_B_US2[1]+ v_A_US2[1]))
  delta_v_US2_B[1] <- df$beta_2[1]* (df$alpha_B[1] - (v_US1_B[1] + v_US2_B[1]+ v_AB[1]))
  
  
  #Initiating delta for compound associations
  delta_v_AB[1] <- df$alpha_A[1]*(df$alpha_B[1]-(v_AB[1]+ v_US1_B[1]+ v_US2_B[1]))
  delta_v_BA[1] <- df$alpha_B[1]*(df$alpha_A[1]-(v_BA[1]+ v_US1_A[1]+ v_US2_A[1]))
  
  for(i in seq_len(n)[-1]){
    
    #A
    #A-US1 and -US2
    v_A_US1[i] <- v_A_US1[i - 1] + delta_v_A_US1[i - 1]
    v_US1_A[i] <- v_US1_A[i - 1] + delta_v_US1_A[i - 1]
    v_A_US2[i] <- v_A_US2[i - 1] + delta_v_A_US2[i - 1]
    
    if (df$beta_2[i] == 0) {
      v_US2_A[i] <- 0
    } else {
      v_US2_A[i] <- v_US2_A[i - 1] + delta_v_US2_A[i - 1]
    }
    
    #B
    #B-US1 and -US2
    v_B_US1[i] <- v_B_US1[i - 1] + delta_v_B_US1[i - 1]
    v_US1_B[i] <- v_US1_B[i - 1] + delta_v_US1_B[i - 1]
    v_B_US2[i] <- v_B_US2[i - 1] + delta_v_B_US2[i - 1]
    
    if (df$beta_2[i]== 0){
      v_US2_B[i] <- 0
    } else {
      v_US2_B[i] <- v_US2_B[i - 1] + delta_v_US2_B[i - 1]
    }
    
    
    
    #compound associations
    v_AB[i]<- v_AB[i - 1] + delta_v_AB[i - 1]
    v_BA[i]<- v_BA[i - 1] + delta_v_BA[i - 1]
    
    
    #deltas A
    #A-US1 and -US2
    delta_v_A_US1[i] <- df$alpha_A[i]*(c* df$beta_1[i] - (v_A_US1[i]+ v_B_US1[i]))
    delta_v_US1_A[i] <- df$beta_1[i]* (c* df$alpha_A[i] - (v_US1_A[i] + v_US2_A[i]+ v_BA[i]))
    delta_v_A_US2[i] <- df$alpha_A[i]*(c* df$beta_2[i] - (v_A_US2[i]+ v_B_US2[i]))
    delta_v_US2_A[i] <- df$beta_2[i]* (c* df$alpha_A[i] - (v_US1_A[i] + v_US2_A[i]+ v_BA[i]))
    
    #deltas B
    #B-US1 and -US2
    delta_v_B_US1[i] <- df$alpha_B[i]*(c* df$beta_1[i] - (v_B_US1[i]+ v_A_US1[i]))
    delta_v_US1_B[i] <- df$beta_1[i]* (c* df$alpha_B[i] - (v_US1_B[i] + v_US2_B[i]+ v_AB[i]))
    delta_v_B_US2[i] <- df$alpha_B[i]*(c* df$beta_2[i] - (v_B_US2[i]+ v_A_US2[i]))
    delta_v_US2_B[i] <- df$beta_2[i]* (c* df$alpha_B[i] - (v_US1_B[i] + v_US2_B[i]+ v_AB[i]))
    
    #deltas compound associations
    delta_v_AB[i] <- df$alpha_A[i]*(c* df$alpha_B[i]-(v_AB[i]+ v_US1_B[i]+ v_US2_B[i]))
    delta_v_BA[i] <- df$alpha_B[i]*(c* df$alpha_A[i]-(v_BA[i]+ v_US1_A[i]+ v_US2_A[i]))
    
    #V A-US1 & -US2 total
    v_sum_A_US1[i] <- v_A_US1[i]+ (1/c * v_A_US1[i]*v_US1_A[i])
    v_sum_A_US2[i] <- v_A_US2[i]+ (1/c * v_A_US2[i]*v_US2_A[i])
    
    #V B US2 total
    v_sum_B_US1[i] <- v_B_US1[i]+ (1/c * v_B_US1[i]*v_US1_B[i])
    v_sum_B_US2[i] <- v_B_US2[i]+ (1/c * v_B_US2[i]*v_US2_B[i])
    
  }
  
  
  df<- data.frame(df, v_A_US1, delta_v_A_US1, v_US1_A, delta_v_US1_A, v_sum_A_US1,
                  v_A_US2, delta_v_A_US2, v_US2_A, delta_v_US2_A, v_sum_A_US2,
                  v_B_US1, delta_v_B_US1, v_US1_B, delta_v_US1_B, v_sum_B_US1,
                  v_B_US2, delta_v_B_US2, v_US2_B, delta_v_US2_B, v_sum_B_US2,
                  v_AB, delta_v_AB, v_BA, delta_v_BA)
}

#Transform model.multiple.2.cs.us to long
to.long<- function (df){
  df_long<- gather(df, ass_indicator, value_ass, - trial, -alpha_A, -alpha_B, -beta_1,
                   -beta_2, -trial_type, -data_frame)
  return(df_long) 
}

#Label model.multiple.2.cs.us long
label.df<- function (df_long) {
  
  df_long$ass_indicator<- ordered(df_long$ass_indicator, 
                                  levels= c('v_A_US1', 'delta_v_A_US1', 'v_US1_A', 'delta_v_US1_A', 'v_sum_A_US1',
                                            'v_A_US2', 'delta_v_A_US2', 'v_US2_A', 'delta_v_US2_A', 'v_sum_A_US2',
                                            'v_B_US1', 'delta_v_B_US1', 'v_US1_B', 'delta_v_US1_B', 'v_sum_B_US1',
                                            'v_B_US2', 'delta_v_B_US2', 'v_US2_B', 'delta_v_US2_B', 'v_sum_B_US2',
                                            'v_AB', 'delta_v_AB', 'v_BA', 'delta_v_BA'),
                                  labels= c('A-US1', 'delta  V A-US1', 'US1-A', 'delta  V US1-A', 'Σ A-US1',
                                            'A-US2', 'delta  V A-US2', 'US2-A', 'delta  V US2-A', 'Σ A-US2',
                                            'B-US1', 'delta  V B-US1', 'US1-B', 'delta  V US1-B', 'Σ B-US1',
                                            'B-US2', 'delta  V B-US2', 'US2-B', 'delta  V US2-B', 'Σ B-US2',
                                            'A-B', 'delta V A-B', 'B-A', 'delta V B-A'))
  return(df_long)
}

