library(agridat)
library(desplot)

data(yates.oats)


###########################################################################
###########################################################################
###                                                                     ###
###                  DESIGN OF FIELD EXPERIMENT LAYOUT                  ###
###                                                                     ###
###########################################################################
###########################################################################


desplot(yates.oats,block~col+row,col=nitro,text=gen,cex=1,out1=block,out2=gen,out2.gpar=list(col='gray50',lwd=1,lty=1))




############################################################################
############################################################################
###                                                                      ###
###             DESIGN OF FIELD EXPERIMENT LAYOUT APPROACH 2             ###
###                                                                      ###
############################################################################
############################################################################


data(yates.oats)
dat <- yates.oats



libs(desplot)
# Experiment design & yield heatmap
desplot(dat, block ~ col*row, col.regions=c("black","yellow"),
        out1=block, num=nitro, col=gen,
        cex=1, aspect=511/176, # true aspect
        main="yates.oats")


# Roughly linear gradient across the field.  The right-half of each
# block has lower yield.  The blocking is inadequate!

libs("lattice")
xyplot(yield ~ col|factor(nitro), dat,
       type = c('p', 'r'), xlab='col', as.table = TRUE,
       main="yates.oats")

libs(lme4)
# Typical split-plot analysis. Non-significant gen differences
m3 <- lmer(yield ~ factor(nitro) * gen + (1|block/gen), data=dat)
# Residuals still show structure
xyplot(resid(m3) ~ dat$col, xlab='col', type=c('p','smooth'),
       main="yates.oats")

# Add a linear trend for column
m4 <- lmer(yield ~ col + factor(nitro) * gen + (1|block/gen), data=dat)
# xyplot(resid(m4) ~ dat$col, type=c('p','smooth'), xlab='col')

## Compare fits
AIC(m3,m4)



# --- nlme ---

libs(nlme)
libs(emmeans)
# create unbalance
dat2 <- yates.oats[-c(1,2,3,5,8,13,21,34,55),]
m5l <- lme(yield ~ factor(nitro) + gen, random = ~1 | block/gen,
           data = dat2)

# asreml r 4 has a bug with asreml( factor(nitro))
dat2$nitrof <- factor(dat2$nitro)

# --- asreml4  ---
libs(asreml)
m5a <- asreml(yield ~ nitrof + gen,
              random = ~ block + block:gen, data=dat2)
libs(lucid)
vc(m5l)
vc(m5a)

emmeans::emmeans(m5l, "gen")
predict(m5a, data=dat2, classify="gen")$pvals

# ----------

if(0){

  # Demonstrate use of regress package, compare to lme

  libs(regress)
  m6 <- regress(yield ~ nitrof + gen, ~block + I(block:gen), identity=TRUE,
                verbose=1, data=dat)
  summary(m6)

  # ordinal causes clash with VarCorr
  if(is.element("package:ordinal", search())) detach(package:ordinal)

  m7 <- lme(yield ~ nitrof + gen, random = ~ 1|block/gen, data=dat)
  lme4::VarCorr(m7)
}
