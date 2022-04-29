
###########################################################################
###########################################################################
###                                                                     ###
###                     HOW TO UNGRADE R ON WINDOWS                     ###
###                                                                     ###
###########################################################################
###########################################################################


# Upgrade R on Windows
mypkgs<-pak::pkg_list()
saveRDS(mypkgs,'mypkgs.rds')

## Now Upgrade R (R4.2)

# After Ugrade
mypkgs <- readRDS('mypkgs.rds')
inst <- mypkgs$package[mypkgs$repository=='CRAN']
install.packages(inst,type = 'binary')

## Most packages should be back let's chaeck what we have now

now <- pak::pkg_list()

# Are there pacakages missing (because they was not installed from CRAN)
missing <- mypkgs[!mypkgs$package %in% now$package, ]

# If pkgs were installed from github we can create the name/repo vector here
add <- paste0(missing$remoteusername, '/', missing$remoterepo)
add <- add[!add %in% 'NA/NA']

# Now install the github pkgs
pak::pak(add)

## After Github installation there should't be much left (most likely pkgs)
## That were installed locally. Check what is missing and think about how to ## install them

now <- pak::pkg_list()

missing <- mykgs[!mypkgs$package %in% now$package,]


