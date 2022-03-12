############################# metan ##########################################

#install.packages("metan")

library(metan)

#install.packages("ggplot2")

library(ggplot2)

options(max.print = 10000)

############################### set wd ######################################

getwd()

############################## importing data ###############################

library(readxl)
setwd(dir = '~/../Desktop')

MMET <- read_excel("stabmul.xls")

View(MMET)

class(MMET)

str(MMET)

############################# factors with unique levels ####################

MMET$Env <- factor(MMET$Env, levels=unique(MMET$Env))

MMET$Geno <- factor(MMET$Geno, levels=unique(MMET$Geno))

MMET$Rep <- factor(MMET$Rep, levels=unique(MMET$Rep))

str(MMET)

###################### data inspection and manipulation #####################

inspect(MMET, plot=TRUE)

##################### check for outliers ###################################

find_outliers(MMET, var=Height, plots=TRUE)

find_outliers(MMET, var=Yield, plots=TRUE)

##################### extra clean functions ###############################

##################### remove NA ###########################################

remove_rows_na(MMET)

####################### replace zero #####################################

replace_zero(MMET)

###################### find text in numbers ##############################

find_text_in_num(MMET$Height)

find_text_in_num(MMET$Yield)


######################### data analysis ##################################

###################### descriptive stats ################################

desc_stat(MMET)

desc_stat(MMET, stats="all")

ds <- desc_stat(MMET, stats="all")

ds

View(ds)

class(ds)

######################### importing a table ############################

install.packages("writexl")

library(writexl)

write_xlsx(ds, "ds.xlsx")

######################## mean performance ##############################

####################### mean of genotypes #############################

mg <- means_by(MMET, Geno)

mg

View(mg)

######################### mean of environmnets #######################

me <- means_by(MMET, Env)

me

View(me)

########### mean performance of genotypes across environments ###########

mge <- MMET %>%

  group_by(Env, Geno) %>%

  desc_stat(Height, Yield, stats="mean")

mge

View(mge)

############### plotting performace across environments ################

## HT

pht <- ge_plot(MMET, Env, Geno, Height)

pht

pht2 <- ge_plot(MMET, Env, Geno, Height, type=2)

pht2

## YLD

pyd <- ge_plot(MMET, Env, Geno, Yield)

pyd

pyd2 <- ge_plot(MMET, Env, Geno, Yield, type=2)

pyd2

######################## winners within each env ###########################

win <- ge_winners(MMET, Env, Geno, resp = everything())

View(win)

####################### ranks of genotypes #################################

ranks <- ge_winners(MMET, Env, Geno, resp = everything(), type = "ranks")

View(ranks)

##### more details on  performance

ge_details(MMET, Env, Geno, resp = everything())


########################## fixed effect models #############################

########################### ind anova ####################################

indav <- anova_ind(MMET, Env, Geno, Rep, resp = c(Height, Yield))

# anova for height

indav$Height$individual

iaht <- indav$Height$individual

View(iaht)

?anova_ind

## Bartlett test

bartlett.test(MMET$Height~MMET$Env, data = MMET)

# anova for yield

indav$Yield$individual

iayd <- indav$Yield$individual

View(iayd)

write_xlsx(iayd, "yieldanv.xlsx")

######################## pooled anova #####################################

# pooled anova for height

panv1 <- anova_joint(MMET, Env, Geno, Rep, Height)

pavt1 <- panv1$Height$anova

View(pavt1)

# pooled anova for yield

panv2 <- anova_joint(MMET, Env, Geno, Rep, Yield)

pavt2 <- panv2$Yield$anova

View(pavt2)


######################## stability analysis ###########################

######################## anova based stability #######################

# Annichiarico env index

ann1 <- Annicchiarico(MMET, Env, Geno, Rep, Height)

print(ann1)

View(ann1$Height$environments)

ann2 <- Annicchiarico(MMET, Env, Geno, Rep, Yield)

print(ann2)

View(ann2$Yield$environments)

# ecovalence

eco1 <- ecovalence(MMET, Env, Geno, Rep, Height)

eco1

View(eco1$Height)

eco2 <- ecovalence(MMET, Env, Geno, Rep, Yield)

eco2

print(eco2)

# Shukla

shu1 <- Shukla(MMET, Env, Geno, Rep, Height)

shu1

shu2 <- Shukla(MMET, Env, Geno, Rep, Yield)

shu2

## Reg based

reg1 <- ge_reg(MMET, Env, Geno, Rep, Height)

print(reg1)

reg1anv <- reg1$Height$anova

View(reg1anv)

write_xlsx(reg1anv, "reganv1.xlsx")

plot(reg1)



reg2 <- ge_reg(MMET, Env, Geno, Rep, Yield)

print(reg2)

reg2anv <- reg2$Yield$anova

View(reg2anv)

plot(reg2)


################################ non parametric ###########################

## superiority by Lin and  Binns

super1 <- superiority(MMET, Env, Geno, Height)

print(super1)

?superiority

View(super1$Height$index)

View(super1$Height$environments)


super2 <- superiority(MMET, Env, Geno, Yield)

View(super2$Yield$index)

View(super2$Yield$environments)


## fox top third cirteria

?Fox

fox1 <- Fox(MMET, Env, Geno, Height)

View(fox1$Height)


fox2 <- Fox(MMET, Env, Geno, Yield)

View(fox2$Yield)


############################ factor based #################################

fact1 <- ge_factanal(MMET, Env, Geno, Rep, Height)

print(fact1)


fact2 <- ge_factanal(MMET, Env, Geno, Rep, Yield)

print(fact2)

plot(fact2)

View(fact2$Yield$PCA)

?ge_factanal


########################## wrap stab parameters ############################

stabp1 <- ge_stats(MMET, Env, Geno, Rep, Height)

data1 <- get_model_data(stabp1)

View(data1)

View(stabp1$Height)


ranksp1 <- get_model_data(stabp1, "ranks")

View(ranksp1)


stabp2 <- ge_stats(MMET, Env, Geno, Rep, Yield)

View(stabp2$Yield)

data2 <- get_model_data(stabp2)

View(data2)

ranks2 <- get_model_data(stabp2, "ranks")

View(ranks2)


?ge_stats


####################### correlation b/w stab index #######################

csi1 <- corr_stab_ind(stabp1)

csi1

View(csi1$corr)

View(csi1$pval)


csi2 <- corr_stab_ind(stabp2, stats = "ammi")

?corr_stab_ind

View(csi2$corr)



###################### ammi models #####################################

## HT

amod1 <- performs_ammi(MMET, Env, Geno, Rep, Height)

print(amod1)

plot(amod1)

View(amod1$Height$ANOVA)

write_xlsx(amod1$Height$ANOVA, "ammianova1.xlsx")


## YLD

amod2 <- performs_ammi(MMET, Env, Geno, Rep, Yield)

print(amod2)

View(amod2$Yield$ANOVA)


#### Significance of ipca

get_model_data(amod1, "ipca_pval")

get_model_data(amod2, "ipca_pval")


########################## ammi biplots ###################################

### HT

a1 <- plot_scores(amod1)

a1

a1 <- plot_scores(amod1, x.lab = "Height")

a1


b1 <- plot_scores(amod1, type = 2)

b1

b1 <- plot_scores(amod1, type = 2, polygon = TRUE)

b1

b1 <- plot_scores(amod1,

                  type = 2,

                  col.env = "blue",

                  col.gen = transparent_color(),

                  col.segm.env = "orange",

                  highlight = c("G1", "G2"),

                  col.highlight = "darkcyan",

                  axis.expand = 1.5)

b1

# export

c1 <- plot_scores(amod1, type = 4)

c1

?plot_scores

c1 <- plot_scores(amod1, type=4, repulsion = 2)

c1

c1 <- plot_scores(amod1, type = 4,

                  size.tex.gen = 2,

                  x.lab = "PC1 of E",

                  y.lab = "Nominal Height",

                  title=FALSE,

                  col.alpha.gen = 0)

c1

arrange_ggplot(a1, b1, c1, tag_levels = "a", nrow = 1)


## Yield


a2 <- plot_scores(amod2)

a2


a2 <- plot_scores(amod2, x.lab = "Yield")

a2


b2 <- plot_scores(amod2, type = 2, polygon = TRUE)

b2


c2 <- plot_scores(amod2, type = 4, size.tex.gen = 2,

                  x.lab = "PC1 of E",

                  y.lab = "Nominal Yield")

c2


######################### ammi based stability statistics ##################

abs1 <- AMMI_indexes(amod1)

print(abs1)

View(abs1$Height)


abs2 <- AMMI_indexes(amod2)

print(abs2)

View(abs2$YLD)


################ ammi based on waas #################################

## Height

waas1 <- waas(MMET, Env, Geno, Rep, Height)

View(waas1$Height$anova)

print(waas1)


wp1_3 <- plot_scores(waas1, type = 3)

wp1_3


wp1_2 <- plot_scores(waas1, type = 2, polygon = TRUE)

wp1_2


## Yield

waas2 <- waas(MMET, Env, Geno, Rep, Yield)

View(waas2$Yield$anova)


wp2_3 <- plot_scores(waas2, type = 3)

wp2_3

################### waas based stats ################################

wabs1 <- AMMI_indexes(waas1)

View(wabs1$HT)

print(wabs1)


wabs2 <- AMMI_indexes(waas2)

View(wabs2$YLD)


################### cross verification #############################


###################### GGE Model ###############################

# svp = environment # by default

## Height

gge_model1 <- gge(MMET, Env, Geno, Height)

predict(gge_model1)

pgge1 <- predict(gge_model1)

View(pgge1$Height)


## yield

gge_model2 <- gge(MMET, Env, Geno, Yield)

pgge2 <- predict(gge_model2)

View(pgge2$Yield)


# 1 Basic biplot

# HT

bbp1 <- plot(gge_model1)

bbp1


bbp1 <- plot(gge_model1, col.gen = "red")

bbp1


# YLD

bbp2 <- plot(gge_model2)

bbp2


# 2 Discriminativeness vs representativenss

# HT

dvr1 <- plot(gge_model1, type = 4)

dvr1


# YLD

dvr2 <- plot(gge_model1, type = 4)

dvr2


dvr2 <- plot(gge_model1, type = 4, plot_theme = theme_gray())

dvr2

?theme


# 3 Ranking environments

## HT


re1 <- plot(gge_model1, type = 6)

re1


## YLD


re2 <- plot(gge_model2, type = 6)

re2


# 4 relation among enironments


## HT


rae1 <- plot(gge_model1, type = 10)

rae1


## YLD


rae2 <- plot(gge_model2, type = 10)

rae2


############################# svp = genotype ############################

# Height

gpg1 <- gge(MMET, Env, Geno, Height, svp="genotype")

pgpg1 <- predict(gpg1)

View(pgpg1$Height)


# Yield

gpg2 <- gge(MMET, Env, Geno, Yield, svp="genotype")

pgpg2 <- predict(gpg2)

View(pgpg2$Yield)


# 5 Mean performance vs stability


## HT

mvs1 <- plot(gpg1, type=2)

mvs1


## YLD

mvs2 <- plot(gpg2, type=2)

mvs2


# 6 Examine a genotype

eg6ht <- plot(gpg1, type = 7, sel_gen = "G6")

eg6ht


eg1yd <- plot(gpg2, type = 7, sel_gen = "G1")

eg1yd


# 7 Ranking of genotype

# HT

rg1 <- plot(gpg1, type = 8)

rg1


# YLD

rg2 <- plot(gpg2,type=8)

rg2


######################### svp = symmetrical ################################

## Height

gps1 <- gge(MMET, Env, Geno, Height, svp = "symmetrical")

pgps1 <- predict(gps1)

View(pgps1$Height)


## Yield

gps2 <- gge(MMET, Env, Geno, Yield, svp = "symmetrical")

pgps2 <- predict(gps2)

View(pgps2$Yield)


# 8 Which won where


## HT

www1 <- plot(gps1, type = 3)

www1


## YLD

www2 <- plot(gps2, type = 3)

www2


# 9 Examine a environment


e1ht <- plot(gps1, type = 5, sel_env = "E1")

e1ht


e2yd <- plot(gps2, type = 5, sel_env = "E2")

e2yd


# 10 comparision between two genotypes


ht2and3 <- plot(gps1, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")

ht2and3


yd1and6 <- plot(gps2, type = 9, sel_gen1 = "G1", sel_gen2 = "G6")

yd1and6


####################### correlation and covariance #########################

de1 <- subset(MMET, Env=="E1")

View(de1)

cr_cv <- covcor_design(de1, Geno, Rep, resp = c(Height, Yield), design = "RCBD")

cr_cv

View(cr_cv$geno_cor)

























