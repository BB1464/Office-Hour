# Gant chart in R

library(plotrix)
library(lubridate)

task = list(labels=c('Research Proposal','Data Collection',
                     'Data Analysis','Report Writting','Submission'),
            starts=ymd(c('2022-05-01','2022-05-31','2022-06-19','2022-07-19','2022-09-10')),
            ends=ymd(c('2022-07-10','2022-09-15','2022-10-11','2022-12-02','2022-10-13')),
            priorities=c(2,1,1,2,1))


# Vertical grid position

vgridpos=ymd(c('2022-01-01','2022-03-01','2022-04-01','2022-07-01','2022-08-01','2022-09-01'))

# Vertical grid labels

vgridlab=c('Jan','Mar','Apr','Jul','Aug','Sep')

gantt.chart(x = task,vgridpos = vgridpos,vgridlab = vgridlab,hgrid = TRUE,priority.legend = TRUE,xlim = ymd(as.Date(c('2022-01-02','2022-09-25'))),
            taskcolors = c('lightgreen','skyblue'),border.col = 'black')



###########################################################################
###########################################################################
###                                                                     ###
###                           GANT CHART IN R                           ###
###                                                                     ###
###########################################################################
###########################################################################


# GGPLOT GANTT CHART
library(ggplot2)
library(tidyr)
library(lubridate)

df <- data.frame(Stage = c("Review literature" , "Hypothesis formulation"
                           , "Get Data"          , "Data Cleansing"
                           , "Data Validation"   , "Analysis"
                           ,"Write Report")
                 , Start = c("2022-01-01"      ,"2022-02-01"
                             ,"2022-01-15"        ,"2022-02-15"
                             ,"2022-02-17"        , "2022-02-17"
                             , "2022-03-01")

                 ,End = c("2022-02-01"         ,"2022-02-15"
                          ,"2022-01-27"        ,"2022-02-20"
                          ,"2022-03-01"        , "2022-03-17"
                          , "2022-03-30")
                 , Complete = c(TRUE, FALSE,TRUE, FALSE, FALSE, FALSE, FALSE))





df$Start <- ymd(df$Start)
df$End <- ymd(df$End)



df.melt <- df%>%
  tidyr::pivot_longer(col = c(Start,End))


today <- as.Date('2022-02-05')


# First Chart

pl <- ggplot(df.melt, aes(x = value, y = Stage, colour = Complete ))
pl <- pl + geom_line( alpha = 0.5, size = 7)
pl <- pl + geom_label(aes(label = format(value, "%d %b")), vjust = -0.5, angle = 45, size = 3, color = "black")
pl <- pl + theme_bw()
pl <- pl + geom_vline(xintercept = today, color = "grey", size = 2, alpha = 0.5)
pl <- pl + labs(title = "Gantt Chart")
pl <- pl + labs(subtitle = "created in ggplot #techanswers88")
pl <- pl + labs(caption = "#techanswers88")
pl <- pl + labs(x = "Date")
pl <- pl + labs(y = "Items")
pl <- pl + scale_color_manual(values = c("red", "blue"))
pl <- pl + theme(legend.position = "none")
pl <- pl + scale_x_date(name = "Dates"
                        ,  date_labels = "%d %b"
                        ,  date_breaks = "1 week"
                        , minor_breaks = "1 day"
                        ,  sec.axis =  dup_axis(name = "Week number",labels = scales::date_format('%W')))

pl


# Second Chart

pl <- ggplot(df.melt, aes(x = value, y = Stage, colour = Complete ))
pl <- pl + geom_line( alpha = 0.5, size = 7)
pl <- pl + geom_label(aes(label = format(value, "%d %b")), vjust = -0.5, angle = 45, size = 3, color = "black")
pl <- pl + theme_bw()
pl <- pl + geom_vline(xintercept = today, color = "grey", size = 2, alpha = 0.5)
pl <- pl + labs(title = "Gantt Chart")
pl <- pl + labs(subtitle = "created in ggplot #techanswers88")
pl <- pl + labs(caption = "#techanswers88")
pl <- pl + labs(x = "Date")
pl <- pl + labs(y = "Items")
pl <- pl + scale_color_manual(values = c("red", "blue"))
pl <- pl + theme(legend.position = "none")
pl <- pl + scale_x_date(name = "Dates"
                        ,  date_labels = "%d %b"
                        ,  date_breaks = "1 week"
                        , minor_breaks = "1 day"
                        ,  sec.axis =  dup_axis(name = "Week number",labels = scales::date_format('%W')))

pl


# Third Chart

pl <- ggplot(df.melt, aes(x = value, y = Stage, colour = Complete ))
pl <- pl + geom_line( alpha = 0.2, size = 10)
pl <- pl + geom_text(aes(label = format(value, "%d %b")), vjust = -0.5, angle = 45, size = 3, color = "black")
pl <- pl +   theme_classic()
pl <- pl + geom_vline(xintercept = today, color = "red", size = 2, alpha = 0.5)
pl <- pl + labs(title = "Gantt Chart")
pl <- pl + labs(subtitle = "created in ggplot #techanswers88")
pl <- pl + labs(caption = "#techanswers88")
pl <- pl + labs(x = "Date")
pl <- pl + labs(y = "Items")
pl <- pl + scale_color_manual(values = c("red", "blue"))
pl <- pl + theme(legend.position = "none")
pl <- pl + theme(panel.background = element_rect(color = "black"))
pl <- pl + scale_x_date(name = "Dates"
                        ,  date_labels = "%d %b"
                        ,  date_breaks = "1 week"
                        , minor_breaks = "1 day"
                        ,  sec.axis =  dup_axis(name = "Week number",labels = scales::date_format('%W')))

pl


# Fourt Chart

pl <- ggplot(df.melt, aes(x = value, y = Stage, colour = Complete ))
pl <- pl + geom_line( alpha = 0.2, size = 10)
pl <- pl + geom_text(aes(label = format(value, "%d %b")), vjust = 0, angle = 0, size = 3, color = "black")
pl <- pl +  theme_gray()
pl <- pl + geom_vline(xintercept = today, color = "red", size = 2, alpha = 0.5)
pl <- pl + labs(title = "Gantt Chart")
pl <- pl + labs(subtitle = "created in ggplot #techanswers88")
pl <- pl + labs(caption = "#techanswers88")
pl <- pl + labs(x = "Date")
pl <- pl + labs(y = "Items")
pl <- pl + scale_color_manual(values = c("red", "blue"))
pl <- pl + theme(legend.position = "none")
pl <- pl + scale_x_date(name = "Dates"
                        ,  date_labels = "%d %b"
                        ,  date_breaks = "1 week"
                        , minor_breaks = "1 day"
                        ,  sec.axis =  dup_axis(name = "Week number",labels = scales::date_format('%W')))

pl


# How to save to power-point file

library(officer)
#
# my_pres <- read_pptx()
#
# my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
# my_pres <- ph_with(x = my_pres, value = pl1,location = ph_location_fullsize() )
# print(my_pres, target = "e:/tmp/gantt charts in ggplot.pptx")  # give your folder location and file name









# Example

# First Year Study Plan

library(plotrix)
library(tidyverse)

set.seed(123)

Ymd.format<-"%Y/%m/%d"

gantt.info<-list(labels=
                   c("Research Proposal","Meta-Analysis",'Startup of the Experiment',"Data collection",'Course Work'),
                 starts=
                   as.POSIXct(strptime(
                     c("2022/06/01","2022/07/01","2022/09/01","2022/09/01","2022/09/01"),
                     format=Ymd.format)),
                 ends=
                   as.POSIXct(strptime(
                     c("2022/08/01","2022/09/01","2022/10/01","2022/10/31","2022/12/31"),
                     format=Ymd.format)),
                 priorities=c(1,2,3,4,5))


vgridpos<-as.POSIXct(strptime(c("2022/01/01","2022/02/01","2022/03/01",
                                "2022/04/01","2022/05/01","2022/06/01","2022/07/01","2022/08/01",
                                "2022/09/01","2022/10/01","2022/11/01","2022/12/01"),format=Ymd.format))


vgridlab<-
  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


gantt.chart(gantt.info,main="First Year Study Plan (2022)",
            priority.legend=FALSE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE)


# Second Year Study Plan

set.seed(123)


Ymd.format<-"%Y/%m/%d"
gantt.info<-list(labels=
                   c("Data Collection","Writting of First Draft Thesis","Model calibration and Validation","Data Wrangling and Analysis","Writting of Manuscript","Course Work"),
                 starts=
                   as.POSIXct(strptime(
                     c("2022/01/01","2022/01/01","2022/09/01","2022/09/01","2022/09/01","2022/01/01"),
                     format=Ymd.format)),
                 ends=
                   as.POSIXct(strptime(
                     c("2022/10/01","2022/07/31","2022/12/31","2022/12/01","2022/12/31","2022/09/01"),
                     format=Ymd.format)),
                 priorities=c(1,2,3,4,5,6))


vgridpos<-as.POSIXct(strptime(c("2022/01/01","2022/02/01","2022/03/01",
                                "2022/04/01","2022/05/01","2022/06/01","2022/07/01","2022/08/01",
                                "2022/09/01","2022/10/01","2022/11/01","2022/12/01"),format=Ymd.format))


vgridlab<-
  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


gantt.chart(gantt.info,main="Second Year Study Plan (2023)",
            priority.legend=FALSE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE)



# Third Year Plan

set.seed(123)

Ymd.format<-"%Y/%m/%d"
gantt.info<-list(labels=
                   c("Data Collection","Data Wrangling and Analysis","Model calibration and Validation","Submission of Thesis","Writting of Manuscript"),
                 starts=
                   as.POSIXct(strptime(
                     c("2022/01/01","2022/01/01","2022/09/01","2022/09/01","2022/09/01"),
                     format=Ymd.format)),
                 ends=
                   as.POSIXct(strptime(
                     c("2022/10/01","2022/07/31","2022/12/31","2022/12/01","2022/12/31"),
                     format=Ymd.format)),
                 priorities=c(1,2,3,4,5,6))


vgridpos<-as.POSIXct(strptime(c("2022/01/01","2022/02/01","2022/03/01",
                                "2022/04/01","2022/05/01","2022/06/01","2022/07/01","2022/08/01",
                                "2022/09/01","2022/10/01","2022/11/01","2022/12/01"),format=Ymd.format))


vgridlab<-
  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


gantt.chart(gantt.info,main="Third Year Study Plan (2024)",
            priority.legend=FALSE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE)



# Fourth Year
set.seed(123)

Ymd.format<-"%Y/%m/%d"
gantt.info<-list(labels=
                   c("Preparation for Defence","Submission of Thesis","Submission of Manuscript","PhD Defence"),
                 starts=
                   as.POSIXct(strptime(
                     c("2022/01/01","2022/01/01","2022/09/01","2022/09/01"),
                     format=Ymd.format)),
                 ends=
                   as.POSIXct(strptime(
                     c("2022/10/01","2022/07/31","2022/12/31","2022/12/01"),
                     format=Ymd.format)),
                 priorities=c(1,2,3,4,5,6))


vgridpos<-as.POSIXct(strptime(c("2022/01/01","2022/02/01","2022/03/01",
                                "2022/04/01","2022/05/01","2022/06/01","2022/07/01","2022/08/01",
                                "2022/09/01","2022/10/01","2022/11/01","2022/12/01"),format=Ymd.format))


vgridlab<-
  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


gantt.chart(gantt.info,main="Fourth Year Study Plan (2025)",
            priority.legend=FALSE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE)



# Stop Here and modify the code







# add a little extra space on the right side
gantt.chart(gantt.info,main="Calendar date Gantt chart (2004)",
            priority.legend=TRUE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE,
            xlim=as.POSIXct(strptime(c("2004/01/01","2004/12/20"),
                                     format=Ymd.format)))
# if both vgidpos and vgridlab are specified,
# starts and ends don't have to be dates
info2<-list(labels=c("Jim","Joe","Jim","John","John","Jake","Joe","Jed","Jake"),
            starts=c(8.1,8.7,13.0,9.1,11.6,9.0,13.6,9.3,14.2),
            ends=c(12.5,12.7,16.5,10.3,15.6,11.7,18.1,18.2,19.0))
gantt.chart(info2,vgridlab=8:19,vgridpos=8:19,
            main="All bars the same color",taskcolors="lightgray")
gantt.chart(info2,vgridlab=8:19,vgridpos=8:19,
            main="A color for each label",taskcolors=c(2,3,7,4,8))
gantt.chart(info2,vgridlab=8:19,vgridpos=8:19,
            main="A color for each interval - with borders",
            taskcolors=c(2,3,7,4,8,5,3,6,"purple"),border.col="black")


# Edit this script


Ymd.format<-"%Y/%m/%d"
gantt.info<-list(labels=
                   c("First task","Second task","Third task","Fourth task","Fifth task"),
                 starts=
                   as.POSIXct(strptime(
                     c("2004/01/01","2004/02/02","2004/03/03","2004/05/05","2004/09/09"),
                     format=Ymd.format)),
                 ends=
                   as.POSIXct(strptime(
                     c("2004/03/03","2004/05/05","2004/05/05","2004/08/08","2004/12/12"),
                     format=Ymd.format)),
                 priorities=c(1,2,3,4,5))
vgridpos<-as.POSIXct(strptime(c("2004/01/01","2004/02/01","2004/03/01",
                                "2004/04/01","2004/05/01","2004/06/01","2004/07/01","2004/08/01",
                                "2004/09/01","2004/10/01","2004/11/01","2004/12/01"),format=Ymd.format))
vgridlab<-
  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
gantt.chart(gantt.info,main="Calendar date Gantt chart (2004)",
            priority.legend=TRUE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE)

