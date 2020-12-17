# Excitatory conditioning
c<-1

model<- function(df){ 
  
  n <- nrow(df)
  #Initiating variables
  v_CS_US <- delta_v_CS_US <- numeric(n)
  v_US_CS <- delta_v_US_CS <- numeric(n)
  
  #Initiating deltas
  delta_v_CS_US[1] <- alpha[1]*(beta[1] - v_CS_US[1])
  delta_v_US_CS[1] <- beta[1]*(alpha[1] - v_US_CS[1])
  for(i in seq_len(n)[-1]){
    v_CS_US[i] <- v_CS_US[i - 1] + delta_v_CS_US[i - 1]
    delta_v_CS_US[i] <- alpha[i]*(c* beta[i] - v_CS_US[i])
    v_US_CS[i] <- v_US_CS[i - 1] + delta_v_US_CS[i - 1]
    delta_v_US_CS[i] <- beta[i]*(c* alpha[i] - v_US_CS[i])
    v<- v_CS_US + (1/c * v_US_CS * v_CS_US) }
  df[is.na(df)] <- 0
  cs_oriented<- (alpha / (alpha + v_CS_US)) * v
  us_oriented<- (v_CS_US / (v_CS_US + alpha)) * v
  
  df <- data.frame(trial, alpha, beta,v, v_CS_US, delta_v_CS_US,  v_US_CS,  delta_v_US_CS, cs_oriented, us_oriented)

  }

#Transform model to long
to.long<- function(df){
  df_long<- gather(df, ass_indicator, value_ass, - trial, -alpha, -beta,  -cs_oriented, -us_oriented)
  df_long<- gather(df_long, response_type, value_response, - trial, -alpha, -beta,  -ass_indicator, -value_ass)
}

#Label model long
label.df<- function (df) {
  df$response_type<- ordered(df$response_type, levels=c("cs_oriented", "us_oriented"), 
                             labels= c("CS-oriented", 'US-oriented'))
  df$ass_indicator<- ordered(df$ass_indicator, levels= c('v_CS_US', 'v_US_CS', 'v', 'delta_v_CS_US', 'delta_v_US_CS'),
                             labels= c( 'CS-US', 'US-CS', 'ΣV', 'delta V CS-US', 'delta V US-US'))
  return(df)
}

