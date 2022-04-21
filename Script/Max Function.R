


library(tidyverse)

df <- tibble(
  RC1 = c(.382, .083, -.203,  .140),
  RC2 = c(.687, .889,  .567,  .01),
  RC3 = c(.120, .279, -.496, -.929)
)

df %>%
  mutate(res = apply(.,
                     1,
                     function(x) {
                       max_abs <- names(which.max(abs(x)))
                       sorted_abs <- sort(abs(x), decreasing = TRUE)
                       distance <- sorted_abs[1] - sorted_abs[2]
                       list(max_abs = max_abs, distance = distance)
                     })) %>%
  unnest_wider(res)
