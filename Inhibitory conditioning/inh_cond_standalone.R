# Inhibitory conditioning
source('load_packages.R')
source('my_theme.R')
source('model.R')


alpha_value_A= 0.5
alpha_value_B= 0.5
beta_value = 0.7
selected_trial = 20


#Inputs
n_trial<-as.numeric(selected_trial)
half_trial<- n_trial/2
trial<- c(1:n_trial)


#Data frame
alpha_A<- rep(alpha_value_A, n_trial)
values<- c(rep(beta_value, half_trial), rep(0, half_trial))
beta<- sample(values)
df<- data.frame(trial, alpha_A, beta)
df <- df%>% mutate (alpha_B = ifelse (df$beta==0, alpha_value_B, 0))
df <- df%>% mutate (trial_type = ifelse (df$beta==0, 'AB-', 'A+'))
df<- model(df)
df_long<- to.long(df)
df_long<- label.df(df_long)



#Spacing on x axis
if (n_trial <= 20) {
  seq(0, n_trial, 5)
  x_spacing <- seq(0, n_trial, 1)
} else if (n_trial > 20){
  x_spacing<- seq(0, n_trial, 5)}


#Limits y axis
decide_max_y_limit<- function (y_vector) {
  max_y_limit<- max(y_vector)
  max_y_limit<- round((1.5* max_y_limit),1)
  return(max_y_limit)
}


decide_min_y_limit<- function (y_vector) {
  min_y_limit<- min(y_vector)
  min_y_limit<- round((1.5* min_y_limit),1)
  return(min_y_limit)
}
max_y_limit<- decide_max_y_limit(df_long$value_ass)
min_y_limit<- decide_min_y_limit(df_long$value_ass)



#Associative strength plot

df_long_vcomb<- (df_long[df_long$ass_indicator %in% c('Σ A', 'Σ AB'), ])
df_long_vcomb$value<- df_long_vcomb$value_ass
labels_p<- c(expression('V'[COMB-A]), expression('V'[COMB-AB]))
a<- ggplot(df_long_vcomb, aes(x= factor(trial), y= value, group= factor(ass_indicator)))+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 2, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#dba800","#07a32e"), labels= labels_p)+
  scale_fill_manual(values=  c("#dba800","#07a32e"), labels= labels_p)+
  scale_shape_manual(values= c(21,22))+
  scale_y_continuous (name= expression('Distribution of V'[COMB]), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Combined associative strenght') +
  theme_bw()+
  my_theme




#Performance plot

df_long_performance<- (df_long[df_long$response_type %in% c('CS-oriented (A)', 'CS-oriented (AB)', 'US-oriented (A)', 'US-oriented (AB)'), ])
df_long_performance$response_type<- ordered(df_long_performance$response_type, levels= c('CS-oriented (A)', 'CS-oriented (AB)', 
                                                                 'US-oriented (A)', 'US-oriented (AB)'))

df_long_performance$value<- df_long_performance$value_response

labels_q<- c(expression('A R'[CS]), expression('AB R'[CS]),  expression('A R'[US]), expression('AB R'[US]))
b<-ggplot(df_long_performance, aes(x= factor(trial), y= value, group= factor(response_type)))+
  geom_line(size= 1, aes(color= response_type), linetype = 'solid')+
  geom_point(size= 2, color= '#000000', aes(shape= trial_type, fill= response_type))+
  scale_color_manual(values= c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"), labels= labels_q)+
  scale_fill_manual(values=  c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"), labels= labels_q)+
  scale_shape_manual(values= c(21,22))+
  scale_y_continuous (name= expression('Distribution of V'[COMB]), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Performance') +
  theme_bw()+
  my_theme




p <- plot_grid(NULL, 
               a,
               b,
               align = 'hv',
               labels = c(" ", "A", "B"),
               hjust = -1,
               ncol=1,
               nrow = 3, rel_heights = c(0.3, 1,1))

p<- p+ draw_label(label= 'Inhibitory learning', x= 0.5 ,y=0.9, color =  "#d73925", size= 16)
p

