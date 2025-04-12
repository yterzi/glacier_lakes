library(tidyverse)
library(vegan)
library(ggpubr)


# NMDS --------------------------------------------------------------------

composition <- readxl::read_xlsx('data/composition.xlsx')

composition %>% 
  select(sample_type,  film,fragment, sphere, fiber) -> shape_data

distance_matrix <- vegdist(shape_data[2:5])
shape_nmds <- metaMDS(distance_matrix)
shape_scores <- as.data.frame(scores(shape_nmds))
plot_data <- cbind(shape_scores, shape_data)

plot_data  %>% 
  ggplot(aes(NMDS1, NMDS2, color = sample_type))+ 
  geom_point()+
  stat_ellipse(geom = 'polygon', aes(fill = sample_type, color = sample_type), alpha = 0.2)+
  scale_color_manual(values = c('#A6998D', '#9FC8FF'))+
  scale_fill_manual(values = c('#A6998D', '#9FC8FF'))+
  annotate('text',x =-1,y= 0.6, label= paste0('stress =', round(shape_nmds$stress,2)))+
  annotate('text',x =-1,y= -0.6, label= paste0('ANOSIM, p <0.05'))+
  theme_bw()+
  theme(legend.position = 'bottom',
        legend.title = element_blank()) -> shape_nmds_plot

composition %>% 
  select(sample_type,pa,pe,pet,pp,ps) -> polymer_data

distance_matrix <- vegdist(polymer_data[2:6])
polymer_nmds <- metaMDS(distance_matrix)
polymer_scores <- as.data.frame(scores(polymer_nmds))
plot_data <- cbind(polymer_scores, polymer_data)

plot_data  %>% 
  ggplot(aes(NMDS1, NMDS2, color = sample_type))+ 
  geom_point()+
  stat_ellipse(geom = 'polygon', aes(fill = sample_type, color = sample_type), alpha = 0.2)+
  scale_color_manual(values = c('#A6998D', '#9FC8FF'))+
  scale_fill_manual(values = c('#A6998D', '#9FC8FF'))+
  annotate('text',x =-1,y= 0.6, label= paste0('stress =', round(polymer_nmds$stress,3)))+
  annotate('text',x =-1,y= -0.6, label= paste0('ANOSIM, p <0.05'))+
  theme_bw()+
  theme(legend.position = 'bottom',
        legend.title = element_blank()) -> polymer_nmds_plot


ggarrange(shape_nmds_plot,  polymer_nmds_plot, common.legend = T, labels = c('e','f'))

