library(ggplot2)
library(ggtrace)

polar_plot <- ggplot(mtcars ,aes(hp, mpg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  expand_limits(y = c(0, 60)) +
  coord_polar(start = 0, theta = "y")

with_ggtrace(
  x = polar_plot +
    facet_wrap(~am) +
    theme(aspect.ratio = 1/.48),
  method = Layout$render,
  trace_steps = 5L,
  trace_expr = quote({
    panels <- lapply(panels, editGrob, vp = viewport(xscale = c(0.48, 1)))
  }),
  out = "g"
)

with_ggtrace(
  x = polar_plot +
    facet_grid(vs~am) +
    theme(aspect.ratio = 1/.48),
  method = Layout$render,
  trace_steps = 5L,
  trace_expr = quote({
    panels <- lapply(panels, editGrob, vp = viewport(xscale = c(0.48, 1)))
  }),
  out = "g"
)
