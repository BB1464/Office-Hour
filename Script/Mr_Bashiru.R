


library(rnaturalearth)

dat <- ne_states(country = 'Nigeria',returnclass = 'sf')



ggplot(data = dat)+
geom_sf()+
geom_sf_text(label=dat$gns_name)+
theme_void()
