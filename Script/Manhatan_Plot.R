
# Install the required package for generating random data for GWAS --------

devtools::install_github("norment/normentR")


# GWAS Study --------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(normentR)

set.seed(2404)


# Simulated dataset -------------------------------------------------------


gwas_data_load <- simulateGWAS(nSNPs = 1e6, nSigCols = 3) %>%
  janitor::clean_names()



# Get all the significant snips -------------------------------------------


sig_data <- gwas_data_load %>%
  subset(p < 0.05)

notsig_data <- gwas_data_load %>%
  subset(p >= 0.05) %>%
  group_by(chr) %>%
  sample_frac(0.1)

gwas_data <- bind_rows(sig_data, notsig_data)


# Preparing the data ------------------------------------------------------


data_cum <- gwas_data %>%
  group_by(chr) %>%
  summarise(max_bp = max(bp)) %>%
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>%
  select(chr, bp_add)

gwas_data <- gwas_data %>%
  inner_join(data_cum, by = "chr") %>%
  mutate(bp_cum = bp + bp_add)


# Axis limit --------------------------------------------------------------


axis_set <- gwas_data %>%
  group_by(chr) %>%
  summarize(center = mean(bp_cum))

ylim <- gwas_data %>%
  filter(p == min(p)) %>%
  mutate(ylim = abs(floor(log10(p))) + 2) %>%
  pull(ylim)

sig <- 5e-8


# Plotting the data -------------------------------------------------------


manhplot <- ggplot(gwas_data, aes(x = bp_cum, y = -log10(p),
                                  color = as_factor(chr), size = -log10(p))) +
  geom_hline(yintercept = -log10(sig), color = "grey40", linetype = "dashed") +
  geom_point(alpha = 0.75) +
  scale_x_continuous(label = axis_set$chr, breaks = axis_set$center) +
  scale_y_continuous(expand = c(0,0), limits = c(0, ylim)) +
  scale_color_manual(values = rep(c("#276FBF", "#183059"), unique(length(axis_set$chr)))) +
  scale_size_continuous(range = c(0.5,3)) +
  labs(x = NULL,
       y = "-log<sub>10</sub>(p)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.y = element_markdown(),
    axis.text.x = element_text(angle = 60, size = 8, vjust = 0.5)
  )



# View the plot -----------------------------------------------------------


print(manhplot)
