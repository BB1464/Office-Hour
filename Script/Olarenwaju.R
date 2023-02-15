library(tidyverse)
library(readxl)

stch <- read_excel(("~/starch n pap/Root yield and starch/data_SP_olanrewaju.xlsx"))


stch <- read_excel(path = here::here('Data/data_SP_olanrewaju.xlsx'))

stch <- stch %>% mutate(sy=(sc*ry)/100)

stch1 <- stch %>% unite(PLMHARV,c(HARV,PLM,), sep = "")

unique(stch1$PLMHARV)

stch1$PLMHARV = factor(stch1$PLMHARV, levels=c("H1P1", "H2P1",
                                               "H3P1", "H1P2",
                                               "H2P2", "H3P2",
                                               "H1P3", "H2P3" ,
                                               "H3P3"))

levels(stch1$PLMHARV)<- c(H1P1="9 \nApril",H2P1="11 \nApril",H3P1="13 \nApril",H1P2="9 \nJune",
                          H2P2= "11 \nJune",H3P2="13 \nJune",H1P3="9 \nAugust",H2P3="11 \nAugust",
                          H3P3="13 \nAugust")


levels(stch1$LOC)<- c(fu="FUNAAB", ps="PSALTRY")


stch1 %>%
  mutate(VART=if_else(condition = VART=='V1',true = 'TME 419',false = 'TME 30572')) |>
  mutate(FERT=if_else(condition = FERT=='F0',true = 'nill',false = '75:20:90 kg/ha NPK')) |>
  mutate(FERT=factor(FERT,levels = c('nill','75:20:90 kg/ha NPK'))) |>
  ggplot(aes(PLMHARV,ry,fill=LOC))+
  #facet_grid(.~HARV)+
  stat_summary(fun.data = mean_se ,geom = "errorbar",width=0.3,position = position_dodge(0.9),size=0.6)+
  stat_summary(fun = mean, geom="bar", position=position_dodge(0.9))+
  theme_test()+

  labs(x=' ',fill='')+
  ylab(bquote('Fresh root yield' ~ (Mg~ha^-1)))+
  #scale_x_discrete(labels=c('Planting \nin April','Planting \nin June','Planting \nin August'))+,
  scale_fill_manual (values =  c('#7F7F7F','#3B3B3B'), name="Location",
                     labels=c("FUNAAB", "PSALTRY"))+
  coord_cartesian(clip = 'off')+
  scale_y_continuous(expand = expansion(mult = c(0,0.01),add = c(0,0.1)),limits = c(0,30))+
  facet_grid(VART~FERT)+

  theme(axis.text = element_text (colour="black"),
        legend.position = c(0.1,0.9),
        legend.background = element_blank(),
        text = element_text(family = 'serif',face = 'bold',colour = 'black',size =12),
        legend.key.size  = unit(0.5, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.3, "cm"))+

  theme(strip.text = element_text(size=12,face="bold", lineheight=10),
        strip.background = element_rect(colour = "black", fill = "white"),axis.title = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.ticks = element_line(size = 0.5))




# Save the Plot --------------------------------------------------


ggsave(filename = 'ry_new.png', width = 9,height = 9,dpi = 200,path = here::here('Plot'))

