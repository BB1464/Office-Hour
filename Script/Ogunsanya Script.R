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

data1 <- read_excel('Data/Copy of Field data Mature trait.xlsx')


data1 <- data1 %>% mutate(across(.cols = c(1:2),.fns = factor))

# Demo
data1 %>% count(Species,Category...4) %>%
  mutate(Perc_Stem_Color=n/sum(n)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...4))+
  geom_col()+
  scale_y_continuous(labels = scales::percent)



data1 %>% count(Species,Category...4) %>%
  mutate(Perc_Stem_Color=round(n/sum(n),1)) %>%
  ggplot(mapping = aes(x = Species,y = Perc_Stem_Color,fill=Category...4))+
  geom_col()+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=Perc_Stem_Color),position = position_dodge(.2),hjust=1)



# Stem Color
p1 <- ggplot(data = data1,mapping = aes(x = Species,y = `Stem Colour`,fill=Category...6))+geom_col()+scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))


# Leaf Margin color
p2 <- ggplot(data = data1,mapping = aes(x = Species,y = `Leaf Margin Colour`,fill=Category...8))+geom_col()+
  scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))


# Petiole Colour
p3 <- ggplot(data = data1,aes(x = Species,y = `Petiole Colour`,fill=Category...10))+
  geom_col()+
  scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))


# Leaf density
p4 <- ggplot(data = data1,aes(x = Species,y = `Leaf density`,fill=Category...12))+
  geom_col()+
  scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))

# Leaf Colour
p5 <- ggplot(data = data1,aes(x = Species,y = `Leaf Colour-`,fill=Category...14))+
  geom_col()+
  scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))

# Leaf Vein Colour Upper
p6 <- ggplot(data = data1,aes(x = Species,y = `Leaf vein colour(Upper Surface)`,fill=Category...16))+
  geom_col()+
  scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))


# Leaf Vein Colour (Lower Surface)
p7 <- ggplot(data = data1,aes(x = Species,y = `Leaf vein colour(Lower Surface)`,fill=Category...18))+
  geom_col()+
  scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))



# Leaf Shape
p8 <- ggplot(data = data1,aes(x = Species,y = `Leaf Shape`,fill=Category...20))+
  geom_col()+
  scale_fill_brewer(name='',palette = 'Set1')

# Leaf Apex Shape
p9 <- ggplot(data = data1,aes(x = Species,y = `Leaf apex shape`,fill=Category...22))+
  geom_col()+
  scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))

# Distance between lobes
p10 <- ggplot(data = data1,aes(x = Species,y = `Distance between lobes`,fill=Category...24))+
  geom_col()+
  scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))

# Vigour
p11 <- ggplot(data = data1,mapping = aes(x = Species,y = Vigour,fill=Category...4))+geom_col()+scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 10))


plot_grid(p6,p2,p3,p4,p5,p1,p7,p8,p9,p10,p11,nrow = 4,ncol = 4,rel_widths = c(1,1),labels = 'AUTO',label_size = 8,label_fontfamily = 'serif')


ggsave(path = here::here('Plot'),'Demo.png',width = 17,height = 12,dpi = 450)




# Analysis for Pot Experiment

data <- read_excel('Data/Copy of Mature trait data pot exp.xlsx')

# Stem Color for Pot Experiment
ggplot(data = data,aes(x = Specie,`Stem Colour`,fill=Category...4))+geom_col()+scale_fill_brewer(name='',palette = 'Set1')+
theme(axis.title.y = element_text(family = 'serif',size = 10))


# Leaf Color
ggplot(data = data,aes(x = Specie,`Leaf colour`,fill=Category...6))+geom_col()+scale_fill_brewer(name='',palette = 'Set1')+
theme(axis.title.y = element_text(family = 'serif',size = 14))

# Leaf Margin Color
ggplot(data = data,aes(x = Specie,`Leaf margin colour`,fill=Category...8))+geom_col()+scale_fill_brewer(name='',palette = 'Set1')+
  theme(axis.title.y = element_text(family = 'serif',size = 14))
