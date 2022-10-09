
## Heat colours


heat_colors <- grDevices::colorRampPalette(c("#800026FF", "#FC4E2AFF", "#FEB24CFF", "#FFFFCCFF"))(10)

heat_palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = -1)

heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = -1))(10)

heat_colors %>% scales::show_col()


heat_palette %>% scales::show_col()

heat_colors_interpolated %>% scales::show_col()



### Sample plot

ggplot(data = iris,aes(x = Species,y = Sepal.Length,fill=Species))+geom_col()+
scale_fill_manual(values = c('#800026','#A91A27','#FC4E2A'))


## Example 2

ggplot(data = iris,aes(x = Species,y = Sepal.Length,fill=Species))+geom_col()+scale_fill_viridis_d(option = 'rocket',direction = -1,alpha = 0.8)+theme_void()
