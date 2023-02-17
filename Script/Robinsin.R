

# Load the Required Packages ----------------------------------------------


library(ggplot2)
library(ggpubr)
library(GGally)


# Load the dataset --------------------------------------------------------

data <- read_csv('Data/Robinsin_data.csv')

# setwd("/Users/user/Desktop/farmer_varieties_report/farmer_gwas/gapit_farmer/all_cmplot/figure_revised/")
# setwd("/Users/user/Desktop/2021_Trial_data/TDRG2_TFE/gblup_info/")
# figure_good <- read.csv("good_Farmer_varieties_breeding_lines.csv", header = T)



colnames(data)

####  used for marker segragation
table(data$Genotype)
table(boiled_data$Chrom3_16645839_GA)
table(boiled_data$Chrom5_30676151_AC)
table(boiled_data$Chrom7_27241780_TA)
table(boiled_data$Chrom15_22974625_AT)



# Comparism ---------------------------------------------------------------


my_comparisons1 <- list( c("SCG1", "SCG2"),c("SCG1","PPT1"),c("SCG1","PPT3"),c("SCG1","APT1"),c("SCG1","APT3"),c("SCG1","NPT1"))


# Second Comparism --------------------------------------------------------


my_comparisons2 <- list( c("SCG2", "SCG1"),c("SCG2","PPT1"),c("SCG2","PPT3"),c("SCG2","APT1"),c("SCG2","APT3"),c("SCG2","NPT1"))


my_comparisons3 <- list( c("PPT1", "SCG1"),c("PPT1","SCG2"),c("PPT1","PPT3"),c("PPT1","APT1"),c("PPT1","APT3"),c("PPT1","NPT1"))


my_comparisons4 <- list( c("PPT3", "SCG1"),c("PPT3","SCG2"),c("PPT3","PPT1"),c("PPT3","APT1"),c("PPT3","APT3"),c("PPT3","NPT1"))

my_comparisons5 <- list( c("APT1", "SCG1"),c("APT1","SCG2"),c("APT1","PPT1"),c("APT1","PPT3"),c("APT1","APT3"),c("APT1","NPT1"))

my_comparisons6 <- list( c("APT3", "SCG1"),c("APT3","SCG2"),c("APT3","PPT1"),c("APT3","PPT3"),c("APT3","APT1"),c("APT3","NPT1"))

my_comparisons7 <- list( c("NPT1", "SCG1"),c("NPT1","SCG2"),c("NPT1","PPT1"),c("NPT1","PPT3"),c("NPT1","APT1"),c("NPT1","APT3"))



# my_comparisons2 <- list( c("SCG1", "PPT1"), c("SCG1", "GG"), c("AG", "GG"))
# my_comparisons3 <- list( c("AA", "CA"), c("AA", "CC"), c("CA", "CC"))
# my_comparisons4 <- list( c("AA", "AT"), c("AA", "TT"), c("AT", "TT"))
# my_comparisons5 <- list( c("Check", "Selected"), c("Check", "Unselected"), c("Selected", "Unselected"))

par(mfrow = c(3, 2))

# new_data_used <- read.csv("Robinson_data_currated.csv", header = T)

attach(data)

library(ggplot2)
library(ggsignif)
detach(data)

table(data$Trial_type)
table(data$Genotype)

colnames(data)


# Visualization for First Comparism ---------------------------------------


p1 <- ggboxplot(data, x = "Selection", y = "Yield", linetype = "solid",
                font.label = list(size = 24, color = "black", style = "bold"),color = "Selection", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons1)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)



# Visualization for Second Comparism --------------------------------------


p2 <- ggboxplot(data, x = "Selection", y = "DMC", linetype = "solid",
font.label = list(size = 24, color = "black", style = "bold"),
color = "Selection", palette = "jco")+
stat_compare_means(comparisons = my_comparisons2)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)



# Visualization for Third Comparism --------------------------------------

p3 <- ggboxplot(data, x = "Selection", y = "YMV", linetype = "solid",
font.label = list(size = 24, color = "black", style = "bold"),
color = "Selection", palette = "jco")+
stat_compare_means(comparisons = my_comparisons3)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)


# Visualization for 4TH Comparism --------------------------------------


p4 <- ggboxplot(data, x = "Selection", y = "Tuber_flesh_Oxidation", linetype = "solid",font.label = list(size = 24, color = "black", style = "bold"),color = "Selection", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons4)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)


# Visualization for 5TH Comparism --------------------------------------

p5 <- ggboxplot(data, x = "Selection", y = "Tuber_flesh_Oxidation", linetype = "solid",font.label = list(size = 24, color = "black", style = "bold"),color = "Selection", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)


# Visualization for 6TH Comparism --------------------------------------

p6 <- ggboxplot(data, x = "Selection", y = "Tuber_flesh_Oxidation", linetype = "solid",font.label = list(size = 24, color = "black", style = "bold"),color = "Selection", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons6)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)


# Visualization for 7TH Comparism --------------------------------------

p7 <- ggboxplot(data, x = "Selection", y = "Tuber_flesh_Oxidation", linetype = "solid",font.label = list(size = 24, color = "black", style = "bold"),color = "Selection", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons7)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)


# Save the Plot as JPEG file ----------------------------------------------


jpeg(filename = "all_figure.jpeg", units = "in", res = 600, width = 15, height = 15)


# Arrange the Plot into a Grid --------------------------------------------


ggarrange(p1, p2, p3, p4,p5,p6,p7, widths = c(2,2), labels = c("a", "b", "c", "d","e","f","g"))



dev.off()







# View(figure_good)
#
# colnames(logistic1)
# attach(logistic1)
# ggboxplot(logistic1, x = "snpZM00179", y = "STRCO2", color = "snpZM00179",
#           add = "jitter", legend = "none") +
#   rotate_x_text(angle = 45)+
#   geom_hline(yintercept = mean(STRCO2), linetype = 0)+ # Add horizontal line at base mean
#   stat_compare_means(method = "anova", label.y = c(0, 5))+        # Add global annova p-value
#   stat_compare_means(label = "p.signif", method = "t.test",
#                      ref.group = ".all.", hide.ns = F)
# b1 <- ggboxplot(figure_good, x = "snpZM00179", y = "YMV", color = "snpZM00179",
#                 add = "jitter", legend = "none") +
#   rotate_x_text(angle = 45)+
#   geom_hline(yintercept = mean(YMV), linetype = 0)+ # Add horizontal line at base mean
#   stat_compare_means(method = "anova", label.y = c(0, 5))+        # Add global annova p-value
#   stat_compare_means(label = "p.signif", method = "t.test",
#                      ref.group = ".all.", hide.ns = F)
# c1 <- ggboxplot(figure_good, x = "Origin", y = "VIGOR", color = "Origin",
#                 add = "jitter", legend = "none") +
#   rotate_x_text(angle = 45)+
#   geom_hline(yintercept = mean(VIGOR), linetype = 0)+ # Add horizontal line at base mean
#   stat_compare_means(method = "anova", label.y = c(0, 5))+        # Add global annova p-value
#   stat_compare_means(label = "p.signif", method = "t.test",
#                      ref.group = ".all.", hide.ns = F)
# d1 <- ggboxplot(figure_good, x = "Origin", y = "OXID", color = "Origin",
#                 add = "jitter", legend = "none") +
#   rotate_x_text(angle = 45)+
#   geom_hline(yintercept = mean(OXID), linetype = 0)+ # Add horizontal line at base mean
#   stat_compare_means(method = "anova", label.y = c(0, 5))+        # Add global annova p-value
#   stat_compare_means(label = "p.signif", method = "t.test",
#                      ref.group = ".all.", hide.ns = F)
# e1 <- ggboxplot(figure_good, x = "Origin", y = "DM", color = "Origin",
#                 add = "jitter", legend = "none") +
#   rotate_x_text(angle = 45)+
#   geom_hline(yintercept = mean(DM), linetype = 0)+ # Add horizontal line at base mean
#   stat_compare_means(method = "anova", label.y = c(0, 5))+        # Add global annova p-value
#   stat_compare_means(label = "p.signif", method = "t.test",
#                      ref.group = ".all.", hide.ns = F)
# pdf(file = "all_trait_combined.pdf", height = 15, width = 15)
# jpeg(filename = "all_figure_farmers_mauscript.jpeg", units = "in", res = 600, width = 15, height = 15)
# pdf(file = "all_trait_combined.pdf", height = 15, width = 15)
# ggarrange(a1, b1, c1, d1, e1, widths = c(3,2), labels = c("a", "b", "c", "d", "e"))
# dev.off()
#
