###########################################################################
###########################################################################
###                                                                     ###
###       GHANT CHART OF MY STUDY PLAN AT UNIVERSITY OF HOHENHEIM       ###
###                                                                     ###
###########################################################################
###########################################################################



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


ggsave(filename = 'Graph/Ghant.png',width = 20,height = 8,dpi =400,bg = '#f5f5f5',plot = last_plot())

# Second Year Study Plan

set.seed(123)


Ymd.format<-"%Y/%m/%d"
gantt.info<-list(labels=
                   c("Data Collection","Writting of First Draft Thesis","Data Wrangling and Analysis","Model Calibration and Validation","Writting of Manuscript","Course Work"),
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

