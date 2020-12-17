source('load_packages.R', local= TRUE)
source('my_theme.R', local= TRUE)
source('model.R', local= TRUE)


#Inputs
alpha_value_A= 0.3
alpha_value_B= 0.5
alpha_value_C= 0.5
beta_value = 0.7
selected_trial = 20



set.seed(20)

n_trial<-as.numeric(selected_trial)
half_trial<- n_trial/2
trial<- c(1:n_trial)



# True discrinination data frame
alpha_A<- rep(alpha_value_A, n_trial)
set.seed(19)
vector_B<- c(rep(alpha_value_B, half_trial), rep(0, half_trial))
alpha_B<- sample(vector_B,length(vector_B),replace=FALSE)
df_true<- data.frame(trial, alpha_A, alpha_B)

df_true <- df_true%>% mutate (alpha_C = ifelse (alpha_B ==0, alpha_value_C, 0),
                              beta = ifelse (alpha_C ==0, beta_value, 0),
                              trial_type = ifelse (beta==0, 'AC-', 'AB+'))
df_true$discrimination<- 'true_discrimination'

# Pseudo discrinination data frame
alpha_A<- rep(alpha_value_A, n_trial)
alpha_B<- rep(c(alpha_value_B, 0, alpha_value_B, 0), n_trial/4)
alpha_C<- rep(c(0, alpha_value_C, 0, alpha_value_C), n_trial/4)
beta<- rep(c(0, 0, beta_value, beta_value), n_trial/4)
df_pseudo_sample<- data.frame(cbind(alpha_A, alpha_B, alpha_C, beta))

set.seed(19)
df_pseudo <- df_pseudo_sample[sample(nrow(df_pseudo_sample)), ]
df_pseudo<- cbind.data.frame(trial, df_pseudo)

df_pseudo <- df_pseudo%>% mutate (trial_type = ifelse (alpha_C == 0 & beta > 0, 'AB+',
                                                       ifelse (alpha_C == 0 & beta == 0, 'AB-',
                                                               ifelse (alpha_B == 0 & beta > 0, 'AC+',
                                                                       ifelse (alpha_B == 0 & beta == 0, 'AC-', 'error')))))
df_pseudo$discrimination<- 'pseudo_discrimination'

#Modelled
df_true<- model(df_true)
df_pseudo<- model(df_pseudo)
#Long format
df_true_long<- to.long(df_true)
df_pseudo_long<- to.long(df_pseudo)


#Labelled
df_true_long<- label.df(df_true_long)
df_pseudo_long<- label.df(df_pseudo_long)


df_long<- rbind.data.frame(df_true_long, df_pseudo_long)
df<- rbind.data.frame(df_true, df_pseudo)


#Spacing on x axis
if (n_trial <= 20) {
  seq(0, n_trial, 5)
  x_spacing <- seq(0, n_trial, 1)
} else if (n_trial > 20){
  x_spacing<- seq(0, n_trial, 5)}


#Limits y axis
decide_max_y_limit<- function (y_vector) {
  max_y<- max(y_vector)
  max_y<- round((1.5* max_y),1)
  return(max_y)
}


decide_min_y_limit<- function (y_vector) {
  min_y<- min(y_vector)
  min_y<- round((1.5* min_y),1)
  return(min_y)
}
max_y_limit<- decide_max_y_limit(df_long$value_ass)
min_y_limit<- decide_min_y_limit(df_long$value_ass)

# True discrimnination df
### CS-US
df_true_cs_us<- (df_true_long[df_true_long$ass_indicator %in% 
                                c('A-US',  'B-US', 'C-US'), ])
df_true_cs_us$trial_type<- ordered(df_true_cs_us$trial_type, levels= c('AB+', 'AC-'))



a<- ggplot(df_true_cs_us, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 2, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#ff4747","#00abd6",  "#8c8c8c"))+
  scale_fill_manual(values=  c("#ff4747","#00abd6",  "#8c8c8c"))+
  scale_shape_manual(values= c(21,24))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('CS-US associations') +
  theme_bw()+
  my_theme


#Between elements
df_true_elements<- (df_true_long[df_true_long$ass_indicator %in% 
                                   c('A-B',  'A-C'), ])
df_true_elements$trial_type<- ordered(df_true_elements$trial_type, levels= c('AB+', 'AC-'))



b<- ggplot(df_true_elements, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 2, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#dba800","#07a32e"))+
  scale_fill_manual(values=  c("#dba800","#07a32e"))+
  scale_shape_manual(values= c(21,24))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Elements associations') +
  theme_bw()+
  my_theme


grid1 <- plot_grid( NULL,
                    a,
                    b,
                    align = 'vh',
                    labels = c(" ", "A", "B"),
                    hjust = -1,
                    ncol=1,
                    nrow = 3,
                    rel_heights =  c(0.3, 1,1), rel_widths = c(1))


p <-grid1 + draw_label(label= 'True discrimination', x= 0.5 ,y=0.94, color = "#d73925")

# Pseudo discrimnination
### CS-US
df_pseudo_cs_us<- (df_pseudo_long[df_pseudo_long$ass_indicator %in% 
                                    c('A-US',  'B-US', 'C-US'), ])
df_pseudo_cs_us$trial_type<- ordered(df_pseudo_cs_us$trial_type, levels= c('AB+', 'AB-', 'AC+', 'AC-'))




c<- ggplot(df_pseudo_cs_us, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 2, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#ff4747","#00abd6",  "#8c8c8c"))+
  scale_fill_manual(values=  c("#ff4747","#00abd6",  "#8c8c8c"))+
  scale_shape_manual(values= c(21,22,23, 24))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('CS-US associations') +
  theme_bw()+
  my_theme


df_pseudo_elements<- (df_pseudo_long[df_pseudo_long$ass_indicator %in% 
                                       c('A-B',  'A-C'), ])
df_pseudo_elements$trial_type<- ordered(df_pseudo_elements$trial_type, levels= c('AB+', 'AB-', 'AC+', 'AC-'))



# Between elements


d<- ggplot(df_pseudo_elements, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 2, color= '#000000', aes(fill= ass_indicator, shape= trial_type))+
  scale_color_manual(values= c("#dba800","#07a32e"))+
  scale_fill_manual(values=  c("#dba800","#07a32e"))+
  scale_shape_manual(values= c(21,22,23, 24))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Elements associations') +
  theme_bw()+
  my_theme




grid2 <- plot_grid( NULL,
                    c,
                    d,
                    align = 'vh',
                    labels = c(" ", "C", "D"),
                    hjust = -1,
                    ncol=1,
                    nrow = 3,
                    rel_heights =  c(0.3, 1,1), rel_widths = c(1))


q <-grid2 + draw_label(label= 'Pseudo discrimination', x= 0.5 ,y=0.94, color = "#d73925", size= 16)

















