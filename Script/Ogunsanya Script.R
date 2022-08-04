############################################################################
############################################################################
###                                                                      ###
###                       FIELD DATA VISUALIZATION                       ###
###                                                                      ###
############################################################################
############################################################################


library(tidyverse)
library(readxl)
library(cowplot)
theme_set(theme_classic())

data1 <- read_excel('Data/Copy of Field data Mature trait.xlsx')


data1 <- data1 %>% mutate(across(.cols = c(1:2),.fns = factor))

# Vigour
p_demo <- data1 |>
  group_by(Species,Category...4) |>
  summarise(count=n()) |>
  mutate(percent=count/sum(count)*100) |>
  ggplot(mapping = aes(x = Species,y = percent,fill=Category...4))+
  geom_col(position = 'stack')+labs(y='Vigour',fill='')+
  scale_y_continuous(breaks = c(0,25,50,75,100),labels = c('0%','25%','50%','75%','100%'),expand = c(0,0))+
  scale_fill_brewer(palette = 'Set1')






p11 <- data1 %>% count(Species,Category...4) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...4))+
  geom_col()+labs(y='Vigour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')




# Stem Color
p1 <- data1 %>% count(Species,Category...6) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...6))+
  geom_col()+labs(y='Stem Colour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')

# p1 <- ggplot(data = data1,mapping = aes(x = Species,y = `Stem Colour`,fill=Category...6))+geom_col()+scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 10))


# Leaf Margin color

p2 <- data1 %>% count(Species,Category...8) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...8))+
  geom_col()+labs(y='Leaf Margin Colour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


#
# p2 <- ggplot(data = data1,mapping = aes(x = Species,y = `Leaf Margin Colour`,fill=Category...8))+geom_col()+
#   scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 10))


# Petiole Colour
p3<- data1 %>% count(Species,Category...10) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...10))+
  geom_col()+labs(y='Petiole Colour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')



# p3 <- ggplot(data = data1,aes(x = Species,y = `Petiole Colour`,fill=Category...10))+
#   geom_col()+
#   scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 10))


# Leaf density
p4 <- data1 %>% count(Species,Category...12) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...12))+
  geom_col()+labs(y='Leaf Density',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')



# p4 <- ggplot(data = data1,aes(x = Species,y = `Leaf density`,fill=Category...12))+
#   geom_col()+
#   scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 10))

# Leaf Colour
p5 <- data1 %>% count(Species,Category...14) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...14))+
  geom_col()+labs(y='Leaf Colour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')



# p5 <- ggplot(data = data1,aes(x = Species,y = `Leaf Colour-`,fill=Category...14))+
#   geom_col()+
#   scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 10))

# Leaf Vein Colour Upper

p6 <- data1 %>% count(Species,Category...16) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...16))+
  geom_col()+labs(y='Leaf Vein Colour (Upper Surface)',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# p6 <- ggplot(data = data1,aes(x = Species,y = `Leaf vein colour(Upper Surface)`,fill=Category...16))+
#   geom_col()+
#   scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 10))


# Leaf Vein Colour (Lower Surface)
p7 <- data1 %>% count(Species,Category...18) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...18))+
  geom_col()+labs(y='Leaf Vein Colour (Lower Surface)',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# p7 <- ggplot(data = data1,aes(x = Species,y = `Leaf vein colour(Lower Surface)`,fill=Category...18))+
#   geom_col()+
#   scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 10))



# Leaf Shape

p8 <- data1 %>% count(Species,Category...20) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...20))+
  geom_col()+labs(y='Leaf Shape',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# p8 <- ggplot(data = data1,aes(x = Species,y = `Leaf Shape`,fill=Category...20))+
#   geom_col()+
#   scale_fill_brewer(name='',palette = 'Set1')


# Leaf Apex Shape
p9 <- data1 %>% count(Species,Category...22) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...22))+
  geom_col()+labs(y='Leaf Apex Shape',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# p9 <- ggplot(data = data1,aes(x = Species,y = `Leaf apex shape`,fill=Category...22))+
#   geom_col()+
#   scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 10))

# Distance between lobes
p10 <- data1 %>% count(Species,Category...24) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...24))+
  geom_col()+labs(y='Distance between Lobes',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')



# p10 <- ggplot(data = data1,aes(x = Species,y = `Distance between lobes`,fill=Category...24))+
#   geom_col()+
#   scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 10))
#

plot_grid(p6,p2,p3,p4,p5,p1,p7,p8,p9,p10,p11,nrow = 4,ncol = 4,rel_widths = c(1,1),labels = 'AUTO',label_size = 8,label_fontfamily = 'serif')


ggsave(path = here::here('Plot'),'Field.png',width = 26,height = 12,dpi = 450)




# Analysis for Pot Experiment

data <- read_excel('Data/Copy of Mature trait data pot exp.xlsx')

# Stem Color for Pot Experiment

p12 <- data %>% count(Specie,Category...4) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...4))+
  geom_col()+labs(y='Stem Colour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# ggplot(data = data,aes(x = Specie,`Stem Colour`,fill=Category...4))+geom_col()+scale_fill_brewer(name='',palette = 'Set1')+
# theme(axis.title.y = element_text(family = 'serif',size = 10))


# Leaf Color
p13 <- data %>% count(Specie,Category...6) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...6))+
  geom_col()+labs(y='Leaf Colour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# ggplot(data = data,aes(x = Specie,`Leaf colour`,fill=Category...6))+geom_col()+scale_fill_brewer(name='',palette = 'Set1')+
# theme(axis.title.y = element_text(family = 'serif',size = 14))

# Leaf Margin Color
p14 <- data %>% count(Specie,Category...8) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...8))+
  geom_col()+labs(y='Margin Colour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# ggplot(data = data,aes(x = Specie,`Leaf margin colour`,fill=Category...8))+geom_col()+scale_fill_brewer(name='',palette = 'Set1')+
#   theme(axis.title.y = element_text(family = 'serif',size = 14))

# Leaf Vein Color (US)
p15 <- data %>% count(Specie,Category...10) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...10))+
  geom_col()+labs(y='Leaf Vein Colour (US)',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# Leaf Vein Color (LS)
p16 <- data %>% count(Specie,Category...12) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...12))+
  geom_col()+labs(y='Leaf Vein Colour (LS)',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# Petiole Colour
p17 <- data %>% count(Specie,Category...14) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...14))+
  geom_col()+labs(y='Petiole Colour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')

# Tip Color
p18 <- data %>% count(Specie,Category...16) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...16))+
  geom_col()+labs(y='Tip Colour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')

# Vigour
p19 <- data %>% count(Specie,Category...18) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...18))+
  geom_col()+labs(y='Vigour',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# Leaf Shape
p20 <- data %>% count(Specie,Category...20) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...20))+
  geom_col()+labs(y='Leaf Shape',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


# Leaf Vein Color
p21 <- data %>% count(Specie,Category...22) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...22))+
  geom_col()+labs(y='Leaf Apex Shape',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')

# Leaf Vein Color
p22 <- data %>% count(Specie,Category...24) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Specie,y = Perc_Stem_Color,fill=Category...24))+
  geom_col()+labs(y='Leaf Density',fill='')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Set1')


plot_grid(p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,nrow = 4,ncol = 4,rel_widths = c(1,1),labels = 'AUTO',label_size = 8,label_fontfamily = 'serif')


ggsave(path = here::here('Plot'),'Pot.png',width = 26,height = 12,dpi = 450)

