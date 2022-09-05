
library(pell)
library(tidyverse)
library(geofacet)
library(gghighlight)
library(ggtext)
library(showtext)

# Aes
showtext_auto()
font_add_google("Advent Pro")
font <- "Advent Pro"

IvyCols <- c("#381C00", "#a9dbf5", "#B31B1B", "#00693e",
             "#A51C30")


data(pell)
head(pell)

levels(pell$NAME)


pell_clean <- pell |>
  group_by(STATE,YEAR) |>
  summarise(Award=sum(AWARD,na.rm = TRUE),.groups = 'drop') |>
  arrange(desc(Award)) |>
  ungroup()




ggplot(data = pell,aes(x = YEAR,y = Award,col=STATE))+
  geom_line(size=1.2)+
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K",trim = TRUE))+
  gghighlight(max(Award)>2e+09,use_direct_label = TRUE,unhighlighted_params = list(colour='gray60'))+
  scale_color_manual(values = c(CA="#381C00",TX= "#f58025",FL="#B31B1B",NY="#00693e",AZ="#00356B"))+
  ggtitle("Pell Grants Award to Students in US",
  subtitle = paste0(
"Plot shows that <span style = 'color:#381C00'>**California**</span>","; <span style = 'color:#F58025'>**Texas**</span>",
            "; <span style = 'color:#B31b1B'>**Florida**</span>",
            "; <span style = 'color:#00693e'>**New York**</span>",
            " and  <span style = 'color:#00356B'>**Arizonia**</span>",
            " Awarded the highest pell grants to student in United State from 2014 to 2017"))+labs(y="", x="", size = "Number of Recipients", caption = "Twitter: @BlakeRobMills | Source: U.S. Department of Education | GitHub: BlakeRMills") +
   theme(plot.title = element_text(size=60, face="bold", hjust=0.5, family=font, color = "#202A44"),plot.caption = element_text(hjust=0.5, family=font, face="bold", size=25, vjust=1, color = "#202A44"),plot.background = element_rect(fill="#f5eee3"),panel.background = element_blank(),
         panel.grid.major = element_line(color="grey65", size=1, linetype = "dashed"),panel.grid.minor = element_blank(),
         panel.spacing = unit(4, "lines"),axis.text.x = element_text(size=21, family=font, face="bold", color = "#202A44"),
         axis.text.y = element_text(size=18, family=font, face="bold", color = "#202A44"),plot.subtitle = element_markdown(size = 13))



