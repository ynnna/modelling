source('load_packages.R', local= TRUE)
source('my_theme.R', local= TRUE)
source('model.R', local= TRUE)

#Inputs
selected_trial = 20
alpha_value_A= 0.5
alpha_value_B = 0.6
beta_value_1= 0.7
beta_value_2 = 0

#Blocking dataframe
n_trial<-as.numeric(selected_trial)
half_trial<- n_trial/2
trial<- c(1:n_trial)
alpha_A<- rep(alpha_value_A, n_trial)
alpha_B<- c(rep(0, half_trial), rep(alpha_value_B, half_trial))

#Both beta values are set to the same value for blocking
beta_1<- rep(beta_value_1, n_trial)
beta_2<- c(rep(beta_value2, n_trial)) 

#Dataframe blocking
df_blocking<- data.frame(trial, alpha_A, alpha_B, beta_1, beta_2)
df_blocking <- df_blocking%>% mutate (trial_type = ifelse (df_blocking$alpha_B==0, 'A+', 'AB+'),
                                      data_frame= rep('blocking', n_trial))
df_blocking<- model(df_blocking)
df_blocking_long<- to.long(df_blocking)
df_blocking_long<- label.df(df_blocking_long)


################################
#Downshift dataframe
alpha_A<- rep(alpha_value_A, n_trial)
alpha_B<- c(rep(0, half_trial), rep(alpha_value_B, half_trial))

beta_1<- rep(beta_value_1, n_trial)
beta_2<- c(rep(beta_value_2, half_trial), rep(0, half_trial)) 

#Dataframe
df_downshift<- data.frame(trial, alpha_A, alpha_B, beta_1, beta_2)
df_downshift <- df_downshift%>% mutate (trial_type = ifelse (df_downshift$alpha_B==0, 'A', 'AB'),
                                        data_frame= rep('downshift', n_trial))
df_downshift<- model(df_downshift)
df_downshift_long<- to.long(df_downshift)
df_downshift_long<- label.df(df_downshift_long)


df <- rbind.data.frame(df_blocking, df_downshift)

#Spacing on x axis
if (n_trial <= 30) {
  seq(0, n_trial, 5)
  x_spacing <- seq(0, n_trial, 2)
} else if (n_trial > 30){
  x_spacing<- seq(0, n_trial, 10)}

#Standard blocking
df_blocking_long_us1<- (df_blocking_long[df_blocking_long$ass_indicator %in% 
                                           c('A-US1',  'US1-A',
                                             'B-US1',  'US1-B'), ])
df_blocking_long_us2<- (df_blocking_long[df_blocking_long$ass_indicator %in% 
                                           c('A-US2',  'US2-A',
                                             'B-US2',  'US2-B'), ])

df_blocking_long_AB<- (df_blocking_long[df_blocking_long$ass_indicator %in% 
                                           c('A-B', 'B-A'), ])

#Downshift unblocking
df_downshift_long_us1<- (df_downshift_long[df_downshift_long$ass_indicator %in% 
                                             c('A-US1',  'US1-A',
                                               'B-US1',  'US1-B'), ])
df_downshift_long_us2<- (df_downshift_long[df_downshift_long$ass_indicator %in% 
                                             c('A-US2',  'US2-A',
                                               'B-US2',  'US2-B'), ])

df_downshift_long_AB<- (df_downshift_long[df_downshift_long$ass_indicator %in% 
                                            c('A-B', 'B-A'), ])


df_long<- rbind(df_blocking_long, df_downshift_long)

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


#Associations with US1 
a<- ggplot(df_blocking_long_us1, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  annotate("rect", fill="#CCCCCC", xmin= half_trial-1, xmax=half_trial+1, ymin= -Inf, ymax = Inf, alpha=0.4)+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 3, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"))+
  scale_fill_manual(values=  c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"))+
  scale_shape_manual(values= c(21, 22))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Blocking')+
  theme_bw() +
  my_theme


b<- ggplot(df_downshift_long_us1, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  annotate("rect", fill="#CCCCCC", xmin= half_trial-1, xmax=half_trial+1, ymin= -Inf, ymax = Inf, alpha=0.4)+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 3, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"))+
  scale_fill_manual(values=  c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"))+
  scale_shape_manual(values= c(21, 22))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Downshift unblocking')+
  theme_bw() +
  my_theme



grid1 <- plot_grid( NULL,
                    a  + theme(legend.position="none"),
                    NULL,
                    b + theme(legend.position="none"),
                    NULL,
                    align = 'vh',
                    labels = c(" ", "A", "B", " "),
                    hjust = -1,
                    ncol=1,
                    nrow = 5,
                    rel_heights =  c(0.3, 1, 0.1, 1, 0.3),rel_widths = c(1))


legend_a <- get_legend(a)
p <-grid1 + draw_grob(legend_a, x=0 ,y= -0.42) + draw_label(label= 'Associations with US1', x= 0.5 ,y=0.94, color = "#d73925", size= 16)

#Associations with US1 (ex b and f)

c<- ggplot(df_blocking_long_us2, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  annotate("rect", fill="#CCCCCC", xmin= half_trial-1, xmax=half_trial+1, ymin= -Inf, ymax = Inf, alpha=0.4)+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 3, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"))+
  scale_fill_manual(values=  c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"))+
  scale_shape_manual(values= c(21, 22))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Blocking')+
  theme_bw() +
  my_theme


d<- ggplot(df_downshift_long_us2, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  annotate("rect", fill="#CCCCCC", xmin= half_trial-1, xmax=half_trial+1, ymin= -Inf, ymax = Inf, alpha=0.4)+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 3, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"))+
  scale_fill_manual(values=  c("#bf0000", "#ff9e9e", "#0025c9", "#00d3eb"))+
  scale_shape_manual(values= c(21, 22))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Downshift unblocking')+
  theme_bw() +
  my_theme

grid2 <- plot_grid( NULL,
                    c + theme(legend.position="none"),
                    NULL,
                    d + theme(legend.position="none"),
                    NULL,
                    align = 'vh',
                    labels = c(" ", "C", "D", " "),
                    hjust = -1,
                    ncol=1,
                    nrow = 5,
                    rel_heights =  c(0.3, 1, 0.1, 1, 0.3),rel_widths = c(1))


legend_c <- get_legend(c)
q<-grid2 + draw_grob(legend_c, x=0 ,y= -0.42) + draw_label(label= 'Associations with US2', x= 0.5 ,y=0.94, color = "#d73925", size= 16)


#Associations between A and B (ex c and g)


e<- ggplot(df_blocking_long_AB, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  annotate("rect", fill="#CCCCCC", xmin= half_trial-1, xmax=half_trial+1, ymin= -Inf, ymax = Inf, alpha=0.4)+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 3, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#bf0000",  "#C0C0C0"))+
  scale_fill_manual(values=  c("#bf0000",  "#C0C0C0"))+
  scale_shape_manual(values= c(21, 22))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Blocking')+
  theme_bw() +
  my_theme


f<- ggplot(df_downshift_long_AB, aes(x= factor(trial), y= value_ass, group= factor(ass_indicator)))+
  annotate("rect", fill="#CCCCCC", xmin= half_trial-1, xmax=half_trial+1, ymin= -Inf, ymax = Inf, alpha=0.4)+
  geom_line(size= 1, aes(color= ass_indicator), linetype = 'solid')+
  geom_point(size= 3, color= '#000000', aes(shape= trial_type, fill= ass_indicator))+
  scale_color_manual(values= c("#bf0000",  "#C0C0C0"))+
  scale_fill_manual(values=  c("#bf0000",  "#C0C0C0"))+
  scale_shape_manual(values= c(21, 22))+
  scale_y_continuous (name= expression('Associative strength (V)'), limits = c(min_y_limit, max_y_limit)) +
  scale_x_discrete(name= "Trials", breaks= x_spacing)+
  ggtitle('Downshift unblocking')+
  theme_bw() +
  my_theme

grid3 <- plot_grid( NULL,
                    e + theme(legend.position="none"),
                    NULL,
                    f + theme(legend.position="none"),
                    NULL,
                    align = 'vh',
                    labels = c(" ", "E", "F", " "),
                    hjust = -1,
                    ncol=1,
                    nrow = 5,
                    rel_heights =  c(0.3, 1, 0.1, 1, 0.3),rel_widths = c(1))


legend_e <- get_legend(e)
z<-grid3 + draw_grob(legend_e, x=0 ,y= -0.42) + draw_label(label= 'Associations between A and B', x= 0.5 ,y=0.94, color = "#d73925", size= 16)







