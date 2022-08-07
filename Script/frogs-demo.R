  # Setup

  ## Load libraries
library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)

# geographical libraries
library(sf)
library(osmdata)
library(gganimate)
library(transformr)
library(magick)

## Load data
frog <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')


glimpse(frog)



## Data wrangling

# Change the formats
frog_v2 <- frog %>%
  mutate(Frequency = as.factor(as.numeric(Frequency))) %>%
  mutate(Sex = factor(ifelse(frog$Female == 1, "Female", "Male"),
                      levels = c("Female", "Male"))) %>%
  mutate(Water = factor(Water,
                        levels = c("Unknown water", "No water",
                                   "Shallow water","Deep water"))) %>%
  mutate(Type = factor(Type,
                       levels = c("Non-aquatic",
                                  "Reservoir",
                                  "Marsh/Pond",
                                  "Stream/Canal"))) %>%
  mutate(SurveyDate = mdy(SurveyDate))


# Rename frog frequencies as subsite + id
frog_labs <- frog_v2 %>%
  group_by(Subsite, Frequency) %>%
  summarize(n = n())

frog_labs$location <- case_when(
  frog_labs$Subsite == "Cow Camp Pond" ~ "cowcamp",
  frog_labs$Subsite == "Cow Camp River" ~ "cowcamp",
  frog_labs$Subsite == "N Res" ~ "n_res",
  frog_labs$Subsite == "W Res" ~ "w_res",
  frog_labs$Subsite == "NE Res" ~ "ne_res",
  frog_labs$Subsite == "SE Pond" ~ "se_pond",
)

frog_labs$id_no = c(1:4, 1:4, 1:8, 1:2, 1:8, 1:8)

frog_labs <- frog_labs %>%
  unite("frog_id", location, id_no, sep = "_", remove = FALSE) %>%
  mutate(frog_id = factor(frog_id)) %>%
  dplyr::select(frog_id, Frequency)

frog_v2 <- inner_join(frog_v2, frog_labs, by = c("Frequency", "Subsite"))

frog_v2 %>%
  group_by(Frequency, Subsite, frog_id) %>%
  summarize(n = n())


## Add missing dates as NA values

# Idea from https://stackoverflow.com/questions/51019672/complete-dataframe-with-missing-combinations-of-values
frog_v3 <- complete(frog_v2, frog_id,
                    SurveyDate = seq(ymd('2018-09-01'), ymd('2018-12-1'), by ='day'),
                    fill = list(Water = NA))

head(frog_v3)

# Habitat plots

p1 <- ggplot(frog_v3, aes(x = SurveyDate, y = frog_id,
                          fill = Water)) +
  scale_fill_manual(values = c("#9B9B9B",
                               "#FFF1D0",
                               "#7BADD2",
                               "#3A6FB0"),
                    na.value = "#f5f5f5") +
  scale_x_date(breaks = "1 month",
               date_labels = "%d-%b",
               expand = c(0, 0)
  ) +
  geom_tile(color = "white") +
  theme_classic() +
  labs(x = NULL, y = NULL
       #title = "Water Depth"
  ) +
  theme(
    legend.title = element_text(size=10),
    legend.key.size = unit(0.1, 'cm'))
p1



p2 <- ggplot(frog_v3, aes(x = SurveyDate, y = frog_id,
                          fill = Type)) +
  scale_fill_manual(values = c("#DAC386",
                               "#92CBC1",
                               "#85ABCE",
                               "#476EAB"),
                    na.value = "#f5f5f5") +
  #scale_fill_brewer(palette = "BrBG", direction = -1,
  #                  na.value = "#f5f5f5") +
  scale_x_date(breaks = "1 month",
               date_labels = "%d-%b",
               expand = c(0, 0)
  ) +
  geom_tile(color = "white") +
  theme_classic() +
  labs(x = NULL, y = NULL
       #title = "Water Body"
  ) +
  theme(
    legend.title = element_text(size=10),
    legend.key.size = unit(0.1, 'cm'))

p2

p3 <- ggplot(frog_v3, aes(x = SurveyDate, y = frog_id,
                          fill = Substrate)) +
  scale_fill_manual(values = c("#d4ac6e",
                               "#a96e5b",
                               "#4f3222",
                               "#9B9B9B"),
                    na.value = "#f5f5f5") +
  scale_x_date(breaks = "1 month",
               date_labels = "%d-%b",
               expand = c(0, 0)
  ) +
  geom_tile(color = "white") +
  theme_classic() +
  labs(x = NULL, y = NULL
       #title = "Soil type"
  )+
  theme(
    legend.title = element_text(size=10),
    legend.key.size = unit(0.1, 'cm'))

p3

p4 <- ggplot(frog_v3, aes(x = SurveyDate, y = frog_id,
                          fill = Structure)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("#3A8372",
                               "#92CBC1",
                               "#D5FEF9",
                               "#DAC386",
                               "#9D652C"),
                    na.value = "#f5f5f5") +
  #scale_fill_brewer(palette = "BrBG",
  #                  direction = -1,
  #                  na.value = "#f5f5f5") +
  scale_x_date(breaks = "1 month",
               date_labels = "%d-%b",
               expand = c(0, 0)) +
  theme_classic() +
  labs(x = NULL, y = NULL
       #title = "Structures"
  )  +
  theme(
    legend.title = element_text(size=10),
    legend.key.size = unit(0.1, 'cm'))

p4


## Combining the plots


plot_row <- plot_grid(p1, p2, p3, p4, ncol = 2)

title <- ggdraw() +
  draw_label(
    "Finding frogs",
    fontface = 'bold',
    x = 0.01,
    hjust = 0,
    size = 20
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 1, 0, 1)
  )

subtitle <- ggdraw() +
  draw_label(
    "An analysis of Oregon Spotted Frog location data from Crane Prairie Reservoir, Oregon. These frogs prefer shallow waters, reservoirs, and flocc soil.",
    x = 0.03,
    hjust = 0,
    size = 12
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

caption <- ggdraw() +
  draw_label(
    "Graphic: @qhuitan | Source: USGS.gov @FGazzelloni",
    x = 0.99,
    fontface = "italic",
    hjust = 1,
    size = 8
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )





hab_plots <- plot_grid(
  title, subtitle, plot_row, caption,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.05, 0.05, 1, 0.05)
)

hab_plots


# Saving plot
png(file = "hab.png", width = 12, height=8, units="in", res=100)

hab_plots

dev.off()

# The gganimate parts

# To convert UTM coords to lat long : https://stackoverflow.com/questions/67106215/sf-from-utm-to-latitude-longitude

utm_coords <- frog %>%
  select(UTME_83, UTMN_83)

longlat <- st_as_sf(x = utm_coords,
                    coords = c("UTME_83", "UTMN_83"),
                    crs = "+proj=utm +zone=10") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  as_tibble()

list_long <- list()
list_lat <- list()

for (i in 1:nrow(longlat)) {
  list_long[[i]] <- longlat$geometry[[i]][1]
  list_lat[[i]] <- longlat$geometry[[i]][2]
}

frog_map <- frog %>%
  mutate(long = unlist(list_long),
         lat = unlist(list_lat)) %>%
  mutate(Frequency = as.factor(Frequency)) %>%
  mutate(Sex = factor(ifelse(frog$Female == 1, "Female", "Male"),
                      levels = c("Female", "Male"))) %>%
  mutate(Water = factor(Water,
                        levels = c("No water", "Shallow water",
                                   "Deep water", "Unknown water"))) %>%
  mutate(SurveyDate = mdy(SurveyDate))


# Get OSM data ---

# Crane Prairie Reservoir coordinates
coords <- c(-121.84, 43.75, -121.74, 43.83)


water <- opq(bbox = coords) %>%
  add_osm_feature(key = "natural",
                  value = "water") %>%
  osmdata_sf()

forest1 <- opq(bbox = coords) %>%
  add_osm_feature(key = "landuse",
                  value = "forest") %>%
  osmdata_sf()

forest2 <- opq(bbox = coords) %>%
  add_osm_feature(key = "natural",
                  value = "wood") %>%
  osmdata_sf()

river <- opq(bbox = coords) %>%
  add_osm_feature(key = "waterway",
                  value = "river") %>%
  osmdata_sf()

wetland <- opq(bbox = coords) %>%
  add_osm_feature(key = "natural",
                  value = "wetland") %>%
  osmdata_sf()

road <- opq(bbox = coords) %>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "unclassified")) %>%
  osmdata_sf()

p <- ggplot() +
  geom_sf(data = forest1$osm_polygons,
          inherit.aes = FALSE,
          fill = "#A1C88A", color = "#A1C88A",  alpha = 0.8) +
  geom_sf(data = forest2$osm_polygons,
          inherit.aes = FALSE,
          fill = "#A1C88A", color = "#A1C88A", alpha = 0.8) +
  geom_sf(data = water$osm_polygons,
          inherit.aes = FALSE,
          fill = "#cae9f5", colour = "#9ECAD8", alpha = 0.8) +
  geom_sf(data = wetland$osm_polygons,
          inherit.aes = FALSE,
          fill = "moccasin", colour = "moccasin", alpha = 0.8) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          fill = "#cae9f5", colour = "#cae9f5", alpha = 0.8) +
  geom_sf(data = road$osm_lines,
          inherit.aes = FALSE,
          fill = "grey90", colour = "grey90", alpha = 0.8) +
  coord_sf(xlim = c(-121.84, -121.74),
           ylim = c(43.75, 43.83),
           expand = TRUE) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#EFEBE3"))


p


## Map of subsite locations
cowcamppond_coord <- c("Cow Camp Pond", -121.764, 43.814)
cowcampriver_coord <- c("Cow Camp River", -121.79, 43.81 )
nres_coord <- c("N Res", -121.79,  43.7999)
neres_coord <- c("NE Res", -121.7650, 43.8075)
sepond_coord <- c("SE Pond", -121.78, 43.76473)
wres_coord <- c("W Res", -121.815, 43.80066)

frog_labs <- as.data.frame(rbind(cowcamppond_coord, cowcampriver_coord,
                                 nres_coord, neres_coord,
                                 sepond_coord, wres_coord))

colnames(frog_labs) <- c("Subsite", "long", "lat")
frog_labs$long <- as.numeric(frog_labs$long)
frog_labs$lat <- as.numeric(frog_labs$lat)

map <- p +
  geom_point(data = frog_map,
             aes(x = long, y = lat, color = Subsite)) +
  scale_color_brewer(palette = "Accent") +
  geom_label(data = frog_labs,
             aes(x = long, y = lat),
             label = frog_labs$Subsite) +
  labs(x = NULL, y = NULL,
       title = "Survey sites for the Oregon Spotted Frog",
       subtitle = "Crane Prairie Reservoir, Oregon, USA") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold",
                                  size = "15",
                                  family = "sans serif"))

map



## Cross-migration between subsites?

# Any cross-migrations across subsites?
mig <- p +
  geom_label(data = frog_labs,
             aes(x = long, y = lat),
             label = frog_labs$Subsite) +
  geom_point(data = frog_map,
             aes(x = long, y = lat, color = Subsite),
             alpha = 0.3) +
  geom_path(data = frog_map,
            aes(x = long, y = lat,
                color = Frequency,
                group = Frequency),
            color = "black") +
  labs(title = "Frogs do not migrate across subsites",
       subtitle = "Movement patterns of Oregon Spotted Frogs (Rana pretiosa) in Crane Prairie Reservoir, \nOregon.\nAll frogs stay within distinct geographical boundaries, and do not migrate between survey sites.",
       x = NULL, y = NULL,
       caption = "Data source: https://doi.org/10.5066/P9DACPCV.") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold",
                                  size = "15",
                                  family = "sans serif"))

# No cross-migrations between subsites! as expected!
mig

# Saving plot
png(file = "mig.png", width = 8, height=8, units="in", res=100)

mig

dev.off()


## Migration patterns within subsites (static)


# Define coordinates for each subsite
sepond_xlim = c(-121.7909, -121.78955)
sepond_ylim = c(43.7644, 43.7654)

wres_xlim = c(-121.8247, -121.8047)
wres_ylim = c(43.789, 43.803)

nres_xlim = c(-121.8, -121.78)
nres_ylim = c(43.799, 43.811)

cowcamp_xlim = c(-121.78, -121.771)
cowcamp_ylim = c(43.8065, 43.815)

neres_xlim = c(-121.772, -121.763)
neres_ylim = c(43.807, 43.813)


## Migration patterns within subsites(animated)

p +
  geom_point(data = df_loc,
             aes(x = long, y = lat, color = df_loc[[color]], group = Frequency)) +
  geom_path(data = df_loc,
            aes(x = long, y = lat, color = df_loc[[color]], group = Frequency),
            alpha = 0.3) +
  coord_sf(
    xlim = x_lim,
    ylim = y_lim
    #xlim = c(-121.7909, -121.78955),
    #ylim = c(43.7644, 43.7654),
  ) +
  labs(title = title, subtitle = 'Date: {frame_along}',
       x = NULL, y = NULL) +
  transition_reveal(SurveyDate) +
  theme_bw() +
  theme(legend.title = element_blank())

# Creating a function for making animated plots

plot_animated <- function(subsite,
                          x_lim,
                          y_lim,
                          title = "Title",
                          color = "Sex") {
  df_loc <- frog_map %>%
    filter(Subsite %in% subsite)

  a <- p +
    geom_point(data = df_loc,
               aes(x = long, y = lat, color = df_loc[[color]], group = Frequency)) +
    geom_path(data = df_loc,
              aes(x = long, y = lat, color = df_loc[[color]], group = Frequency),
              alpha = 0.3) +
    coord_sf(
      xlim = x_lim,
      ylim = y_lim
      #xlim = c(-121.7909, -121.78955),
      #ylim = c(43.7644, 43.7654),
    ) +
    labs(title = title, subtitle = 'Date: {frame_along}',
         x = NULL, y = NULL) +
    transition_reveal(SurveyDate) +
    theme_bw() +
    theme(legend.title = element_blank())



  a_mgif <- animate(a,
                    duration = 20,
                    renderer = gifski_renderer(loop = FALSE),
                    height = 500, width = 500
  )

  return(a_mgif)
}


sepond <- plot_animated(subsite = "SE Pond",
                        title = "SE Pond",
                        x_lim = sepond_xlim,
                        y_lim = sepond_ylim,
                        color = "Sex"
)

sepond

wres <- plot_animated(subsite = "W Res",
                      title = "W Res",
                      x_lim = c(-121.8247, -121.8047),
                      y_lim = c(43.789, 43.803),
                      color = "Sex")
wres

nres <- plot_animated(subsite = "N Res",
                      title = "N Res",
                      x_lim = nres_xlim,
                      y_lim = nres_ylim,
                      color = "Sex")
nres


cowcamp <- plot_animated(subsite = c("Cow Camp Pond", "Cow Camp River"),
                         title = "Cow Camp Pond and River",
                         x_lim = cowcamp_xlim,
                         y_lim = cowcamp_ylim,
                         color = "Sex")
cowcamp


neres <- plot_animated(subsite = c("NE Res"),
                       title = "NE Res",
                       x_lim = neres_xlim,
                       y_lim = neres_ylim,
                       color = "Sex")
neres



# Combining everything together

wres_p <- image_read(wres)
nres_p <- image_read(nres)
cowcamp_p <- image_read(cowcamp)
neres_p <- image_read(neres)
sepond_p <- image_read(sepond)

new_gif <- image_append(image = c(wres_p[1], nres_p[1],
                                  cowcamp_p[1], neres_p[1],
                                  sepond_p[1]
)
)

for(i in 2:100){
  combined <- image_append(c(wres_p[i], nres_p[i],
                             cowcamp_p[i], neres_p[i],
                             sepond_p[i]
  ))
  new_gif <- c(new_gif, combined)
}

#saveRDS(new_gif, file = "combined_5plots.RDS")

image_write(new_gif, path = "5plots.gif", format = "gif")






# The SE Pond
sepond <- frog_map %>%
  filter(Subsite == "SE Pond")

a <- p +
  geom_point(data = sepond,
             aes(x = long, y = lat, color = Sex, group = Frequency)) +
  geom_path(data = sepond,
            aes(x = long, y = lat, color = Sex, group = Frequency),
            alpha = 0.3) +
  coord_sf(xlim = c(-121.7909, -121.78955),
           ylim = c(43.7644, 43.7654)) +
  labs(title = "SE Pond", subtitle = 'Date: {frame_along}',
       x = NULL, y = NULL) +
  transition_reveal(SurveyDate) +
  theme(legend.position = "none") +
  theme_bw()


a_mgif <- animate(a,
                  duration = 20,
                  renderer = gifski_renderer(loop = FALSE),
                  height = 500, width = 500
)

# The Western Res

wres <- frog_map %>%
  filter(Subsite == "W Res")

b <- p +
  geom_point(data = wres,
             aes(x = long, y = lat, color = Sex, group = Frequency)) +
  geom_path(data = wres,
            aes(x = long, y = lat, color = Sex, group = Frequency),
            alpha = 0.3) +
  coord_sf(xlim = c(-121.8247, -121.8047),
           ylim = c(43.789, 43.803)) +
  labs(title = "W Res", subtitle = 'Date: {frame_along}',
       x = NULL, y = NULL) +
  transition_reveal(SurveyDate) +
  theme_bw()


b_mgif <- animate(b,
                  duration = 20,
                  renderer = gifski_renderer(loop = FALSE),
                  height = 500, width = 500
)


# Combining two plots
a_mgif <- image_read(a_mgif)
b_mgif <- image_read(b_mgif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif




# Work in progress: static migration plots
# Creating a function for making static plots

plot_static <- function(subsite,
                        x_lim,
                        y_lim,
                        title = "Title",
                        color = "Sex") {
  df_loc <- frog_map %>%
    filter(Subsite %in% subsite)

  a <- p +
    geom_point(data = df_loc,
               aes(
                 x = long,
                 y = lat,
                 color = df_loc[[color]],
                 group = Frequency
               )) +
    geom_path(
      data = df_loc,
      aes(
        x = long,
        y = lat,
        color = Frequency,
        group = Frequency
      ),
      alpha = 0.3
    ) +
    coord_sf(xlim = x_lim,
             ylim = y_lim) +
    labs(title = title,
         x = NULL, y = NULL) +

    theme_bw() +
    theme(legend.title = element_blank())

  return(a)
}

plot_static(subsite = "W Res", x_lim = wres_xlim,
            y_lim = wres_ylim)

sessionInfo()
