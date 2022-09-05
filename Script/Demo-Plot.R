
library(tidyverse)
library(openxlsx)
library(stringr)
library(lubridate)
library(janitor)
library(data.table)

# wanted to use the additional data in xlsx file
# didn't find a quick way to import xlsx file from github so downloaded file locally
# import sheets from xlsx file
setwd('..')
path <- "./data/challenge.xlsx"
data_frame <- lapply(getSheetNames(path), read.xlsx, xlsxFile=path)
names(data_frame) <- str_replace_all(getSheetNames(path), "[^[:alnum:]]", "")
list2env(data_frame,envir=.GlobalEnv)


# basic cleanup

grad_df <- PilotRosterRawData %>%
  mutate(date_grad = as.Date(Graduation.date, origin = "1899-12-30")) %>%
  separate(Name, c("name2","name1","name3"), ",") %>%
  mutate(Name = trimws(str_squish(paste(name1, name2, coalesce(name3,"")))),
         Status = 'Graduated') %>%
  select(Name, date_grad, Status) %>%
  drop_na()

pow_df <- POWData %>%
  transmute(Name = trimws(str_squish(gsub(".*Capt. ","",
                                          gsub(".*Officer ","",
                                               gsub(".*Lt ","",
                                                    gsub(".*Lt. ","", NAME)))))),
            date_pow = coalesce(mdy(DATE),excel_numeric_to_date(as.numeric(DATE))),
            Status = 'POW')

kiaMia_df <- KIAMIAData %>%
  transmute(date_kmia = coalesce(dmy(DATE),excel_numeric_to_date(as.numeric(DATE))),
            Name = trimws(gsub(".*FO ","",
                               gsub(".*Capt. ","",
                                    gsub(".*Officer ","",
                                         gsub(".*Lt ","",
                                              gsub(".*Lt. ","", `PILOT’S.NAME`)))))),
            Status = 'KIA|MIA') %>%
  separate(Name, c("name1","name2"), ",") %>%
  mutate(Name = str_squish(paste(name1, coalesce(name2,"")))) %>%
  select(Name,date_kmia,Status) %>%
  drop_na()

victory_df <- PilotRosterRawData %>%
  filter(is.na(Aerial.Victory.Credits) == FALSE) %>%
  separate(Name, c("name2","name1","name3"), ",") %>%
  mutate(Name = trimws(str_squish(paste(name1, name2, coalesce(name3,"")))),
         Status = 'Victory') %>%
  separate_rows(Aerial.Victory.Credits, sep = c("; ")) %>%
  mutate(date_vict = mdy(gsub(".*on ","", Aerial.Victory.Credits))) %>%
  separate_rows(Aerial.Victory.Credits, sep = c(" and")) %>%
  mutate(p = coalesce(as.numeric(gsub('1/2', '1',
                                      gsub('\\s+', '',
                                           str_extract_all(Aerial.Victory.Credits, " 1/2 | 1 | 2 | 3 | 4 ")))),2)) %>%
  select(Name, date_vict, Status, p) %>%
  group_by(Name) %>%
  mutate(p = cumsum(p)) %>%
  distinct() %>%
  ungroup()



# check out date ranges

summary(grad_df)
summary(grad_df$date_grad)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
# "1942-03-06" "1943-10-22" "1944-05-23" "1944-07-02" "1945-04-15" "1948-10-12"

summary(pow_df)
summary(pow_df$date_pow)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
# "1944-01-15" "1944-08-07" "1944-10-06" "1944-10-29" "1945-03-15" "1945-04-23"

summary(kiaMia_df)
summary(kiaMia_df$date_kmia)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
# "1943-07-02" "1944-06-08" "1944-08-30" "1944-09-04" "1944-12-29" "1945-04-23"

summary(victory_df)
summary(victory_df$date_vict)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
# "1943-07-02" "1944-07-15" "1944-07-26" "1944-09-06" "1945-03-25" "1945-04-26"

# 1945-09-02 official end of wwii
# check grad_df max dates
qplot(grad_df$date_grad)
boxplot(grad_df$date_grad)
boxplot(grad_df$date_grad, plot=FALSE)$out
outliers <- boxplot(grad_df$date_grad, plot=FALSE)$out
grad_df <- grad_df[-which(grad_df$date_grad %in% outliers),]
boxplot(grad_df$date_grad)
summary(grad_df$date_grad) # max still one year +

qplot(pow_df$date_pow)
boxplot(pow_df$date_pow)

qplot(kiaMia_df$date_kmia)
boxplot(kiaMia_df$date_kmia)

qplot(victory_df$date_vict)
boxplot(victory_df$date_vict)

pow_df %>% filter(date >= ymd("1945-09-02"))
kiaMia_df %>% filter(date >= ymd("1945-09-02"))
victory %>% filter(date >= ymd("1945-09-02"))


# joing tables

df_test_join <- pow_df %>%
  # inner_join(grad_df, by = c("Name" = "Name")) # 25/32
  left_join(grad_df, by = c("Name" = "Name")) # slight name variation


library(fedmatch)
grad_df$id1 <- seq(1:nrow(grad_df))
pow_df$id2 <- seq(1:nrow(pow_df))
df_test_join <- merge_plus(pow_df, grad_df,
                           by.x = "Name", by.y = "Name",
                           match_type = 'fuzzy',
                           unique_key_1 = "id2", unique_key_2 = "id1",
                           suffixes = c("_1", "_2"),
                           fuzzy_settings = build_fuzzy_settings(maxDist = .8))

View(df_test_join$matches) # 32/32

pow_df <- df_test_join$matches %>%
  select(Name_2, Status_1, date_pow) %>%
  `colnames<-`(c("Name", "Status", "date"))


df_test_join <- kiaMia_df %>%
  # inner_join(grad_df, by = c("Name" = "Name")) # 89/121
  left_join(grad_df, by = c("Name" = "Name")) # again, slight name variation

kiaMia_df$id2 <- seq(1:nrow(kiaMia_df))
df_test_join <- merge_plus(kiaMia_df, grad_df,
                           by.x = "Name", by.y = "Name",
                           match_type = 'fuzzy',
                           unique_key_1 = "id2", unique_key_2 = "id1",
                           suffixes = c("_1", "_2"),
                           fuzzy_settings = build_fuzzy_settings(maxDist = .8))

View(df_test_join$matches) # 121/121

kiaMia_df <- df_test_join$matches %>%
  select(Name_2, Status_1, date_kmia) %>%
  `colnames<-`(c("Name", "Status", "date")) %>%
  distinct()



df_test_join <- kiaMia_df %>%
  group_by(Name, Status) %>%
  summarize(start=min(date),end=max(date + days(2))) %>%
  do(data.frame(Name=.$Name, Status=.$Status, days=seq(.$start,.$end,by="1 day")))



kiaMia_df <- kiaMia_df %>%
  # inner_join(pow_df, by = c("Name" = "Name")) # 30/32
  # left_join(pow_df, by = c("Name" = "Name")) # 119/122
  left_join(pow_df, by = c("Name" = "Name", "date" = "date")) %>% # 119/122
  mutate(Status = coalesce(Status.y, Status.x),
         p = NA) %>%
  select(Name, date, Status, p)

names(victory_df)[2] <- 'date'
names(grad_df)[2] <- 'date'

pilot_df <- grad_df %>%
  select(-id1) %>%
  mutate(p = NA) %>%
  rbind(victory_df, kiaMia_df) %>%
  filter(date <= ymd("1945-09-02")) %>%
  group_by(Name) %>%
  mutate(Status = gsub('-NA', '', paste0(Status,"-",p)),
         status_rank = rank(date),
         p = coalesce(p,0)) %>%
  group_by(Name) %>%
  arrange(Name,status_rank) %>%
  mutate(end = coalesce(lead(date), ymd("1945-09-02")),
         start=date) %>%
  group_by(Name, Status, p, status_rank) %>%
  mutate(date = list(seq(start, end, "day"))) %>%
  unnest(date) %>%
  mutate(date = floor_date(date, unit = "month")) %>%
  ungroup() %>%
  select(-c(start,end,status_rank)) %>%
  distinct() %>%
  group_by(Name,Status) %>%
  mutate(grad_rank = rank(date),
         Status = ifelse(Status == 'Graduated' & grad_rank > 1, 'Active', Status)) %>%
  select(-grad_rank) %>%
  group_by(date) %>%
  arrange(date,Status,Name) %>%
  mutate(y = row_number()) %>%
  ungroup() %>%
  mutate(x = dense_rank(date))



# plot

library(ggplot2)
library(gridtext)

cols <- c("Active" = "#eab676",
          "Graduated" = "#6a4a4b",
          "KIA|MIA" = "#bf8687",
          "POW" = "#21130d",
          "Victory-1" = "#abdbe3",
          "Victory-2" = "#76b5c5",
          "Victory-3" = "#1e81b0",
          "Victory-4" = "#063970")


y1 <- grid::textGrob(unique(year(pilot_df$date))[1], gp=gpar(fontsize=10, fontface="bold"))
y2 <- grid::textGrob(unique(year(pilot_df$date))[2], gp=gpar(fontsize=10, fontface="bold"))
y3 <- grid::textGrob(unique(year(pilot_df$date))[3], gp=gpar(fontsize=10, fontface="bold"))
y4 <- grid::textGrob(unique(year(pilot_df$date))[4], gp=gpar(fontsize=10, fontface="bold"))

pilot_df %>%
  ggplot(aes(x=date, y=y, color=Status)) +
  geom_point(alpha = 1, size = 2) +
  scale_colour_manual(values = cols) +
  scale_x_date(date_breaks = "1 months",
               minor_breaks = NULL,
               labels = function(x) numform::f_month(x),
               limits = c(as.Date("1942-03-01"), as.Date("1945-09-01")),
               #limits =  c(n,NA),
               expand=c(0,20)) +
  annotation_custom(y1,ymin=-100,ymax=-100,
                    xmin=as.Date("1942-03-01"),
                    xmax=as.Date("1942-03-01")) +
  annotation_custom(y2,ymin=-100,ymax=-100,
                    xmin=unique(floor_date(pilot_df$date, unit = "year"))[2],
                    xmax=unique(floor_date(pilot_df$date, unit = "year"))[2]) +
  annotation_custom(y3,ymin=-100,ymax=-100,
                    xmin=unique(floor_date(pilot_df$date, unit = "year"))[3],
                    xmax=unique(floor_date(pilot_df$date, unit = "year"))[3]) +
  annotation_custom(y4,ymin=-100,ymax=-100,
                    xmin=unique(floor_date(pilot_df$date, unit = "year"))[4],
                    xmax=unique(floor_date(pilot_df$date, unit = "year"))[4]) +
  coord_cartesian(clip = "off") +
  labs(title = "#TuskegeeAirmenChallenge", subtitle = "#TidyTuesday", x = "", y = "") +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 12))


