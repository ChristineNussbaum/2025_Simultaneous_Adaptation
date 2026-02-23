########### Poweranalysis for the Simultaneous opposite aftereffect project ####
## Bachelorpropädeutikum
## Lehrstuhl für kognitive Psychologie und kognitive Neurowissenschaften

## Author: Christine Nussbaum
## Date: 01.2021
## Help: https://cran.r-project.org/web/packages/Superpower/vignettes/intro_to_superpower.html


#----------------------------------------------------------------------------------------------------
##### Preparation ######

#1 set working directory
setwd()

#2 clear environment
rm(list = ls())

#3 load the relevant package
library(Superpower)


#----------------------------------------------------------------------------------------------------
#### Some helpful functions ####

# 1 from mu and sd to to effect size f
mu <- c(-0.25, 0.25, 0.25, -0.25)
sd <- 1
f <- sqrt(sum((mu-mean(mu))^2)/length(mu))/sd #Cohen (1988)

#2 from f to eta2
eta2 = (f^2/ (1+f^2))

#3 from eta2 to f
f = sqrt(eta2 / ( 1 - eta2 ))

#4 get mu from ES (eta2). CAVE: for one factor ANOVA and SD = 1 only!!!
ngroups = 2 # number of groups
mu <- mu_from_ES(ngroups, eta2)


#----------------------------------------------------------------------------------------------------
#(7) #### Two-way ANOVA with a 2 level within-subject factor and a 2 factor within-subject


# specify parameters
design <- "2w*2w"                           # specify design
n <- 30                                     # n per group
mu <- c(0.6, 0.4, 0.4, 0.6)                 # enter means (mu), I used values around the PSE
sd <- 0.6                                     # enter sd   
r <- 0.5
f <- sqrt(sum((mu-mean(mu))^2)/length(mu))/sd  # calculate Cohens f for the interaction only! 
labelnames <- c("SpSex", "f", "m", "AdaptCond", "fang,mfea", "mang,ffea")  #labelnames are optional

# feed parameters into the design
design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames,
                              plot = TRUE)

# Check design results to see of means were entered correctly
design_result

# calculate power analysis
ANOVA_exact(design_result)
#CAVE: ONLY look at the power of the interaction, nothing else!

# plot power as the function of sample size per condition
plot_power(design_result, min_n = 7, max_n = 40, desired_power = 80)
#we need 38 participants

## End of Script