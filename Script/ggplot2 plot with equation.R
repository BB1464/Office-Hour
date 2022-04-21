
# Initial problem

library(ggplot2)

df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)

p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()

p



# Add Significant Asterisk to the plot

lm_eqn <- function(df){

m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)


p1


# Second Step


library(ggplot2)
library(ggpmisc)

df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)


my.formula <- y ~ x

p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula)+
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  geom_point()


p



# Another Approach

library(ggpmisc)
df <- data.frame(x = c(1:100))
df$y <- 20 * c(0, 1) + 3 * df$x + rnorm(100, sd = 40)
df$group <- factor(rep(c("A", "B"), 50))

my.formula <- y ~ x

p <- ggplot(data = df, aes(x = x, y = y, colour = group)) +
geom_smooth(method = "lm", se=FALSE, formula = my.formula) +
stat_poly_eq(formula = my.formula,
aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
parse = TRUE) +
geom_point()

p



p <- ggplot(data = df, aes(x = x, y = y, linetype = group)) +
geom_smooth(method = "lm", se=FALSE, formula = my.formula) +
stat_poly_eq(formula = my.formula,
aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
parse = TRUE) +
geom_point()

p





# GGPURBR Option

library(ggpubr)

# reproducible data
set.seed(1)
df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)

# By default showing Pearson R
ggscatter(df, x = "x", y = "y", add = "reg.line") +
  stat_cor(label.y = 300) +
  stat_regline_equation(label.y = 280)


