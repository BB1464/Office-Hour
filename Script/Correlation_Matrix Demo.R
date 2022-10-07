############################################################################
############################################################################
###                                                                      ###
###                          CORRELATION MATRIX                          ###
###                                                                      ###
############################################################################
############################################################################



library(ggplot2)
library(ggcor)
quickcor(mtcars) + geom_colour()


iris |>
  dplyr::select(1:4) |>
  quickcor()+
  geom_color()


quickcor(mtcars, cor.test = TRUE) +
  geom_square(data = get_data(type = "lower", show.diag = FALSE)) +
  geom_mark(data = get_data(type = "upper", show.diag = FALSE), size = 2.5) +
  geom_abline(slope = -1, intercept = 12)


### Add the square to upper
quickcor(mtcars, cor.test = TRUE) +
  geom_square(data = get_data(type = "upper", show.diag = FALSE)) +
  geom_mark(data = get_data(type = "lower", show.diag = FALSE), size = 2.5) +
  geom_abline(slope = -1, intercept = 12)


