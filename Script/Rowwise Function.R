ID <- c('A', 'B', 'C', 'D','E')
Q1 <- c(NA, 1, 2, NA, 4)
Q2 <- c(5, NA, 7, 8, 9)
Q3 <- c(0, 1, NA, 3, 4)
df <- data.frame(ID, Q1, Q2, Q3)

df

df %>% group_by(ID) %>%
  summarise(
    Q1 = sum(Q1, na.rm = TRUE),
    Q2 = sum(Q2, na.rm = TRUE),
    Q3 = sum(Q3, na.rm = TRUE)
  )


df %>% rowwise() %>% mutate(m=sum(Q1,Q2,Q3,na.rm = TRUE))


