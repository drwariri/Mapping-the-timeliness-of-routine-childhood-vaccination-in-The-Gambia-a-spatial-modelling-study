
#load packages#####################################################################################################################
library(foreign)
library(survey)
library(dplyr)


#Set working directory
setwd("C:/Users/owariri/OneDrive - London School of Hygiene and Tropical Medicine/Desktop/Old Laptop/Desktop/Daddy's files/PhD Application/PhD/DATA/DHS_data/4_Direct survey methods")


#Read in file######################################################################################################################
data 	<- read.csv(paste0("obj_2_censored_final.csv"), head = T) # data for processing direct survey output (crude, timeliness)


## Reclassify 'bcg_age_vac' into new categorical variable (bcg_age_vac_cat) using 7 days as cutoff 'category'
data <- data %>% 
  mutate(bcg_age_vac_cat = cut(as.numeric(bcg_age_vac), breaks = c(-Inf, 7, Inf),
                               labels = c("timely","delayed")))


#Create new vax variables with 0's and 1's########################################################################################
data$bcg_vac <- rep(1, nrow(data))
ndk <- which(data$BCg_health_card=="Don't know" | data$BCg_health_card=="No")
data$bcg_vac[ndk] <- 0 

data$hepB0_vac <- rep(1, nrow(data))
ndk <- which(data$hepB0_health_card=="Don't know" | data$hepB0_health_card=="No")
data$hepB0_vac[ndk] <- 0 

data$penta1_vac <- rep(1, nrow(data))
ndk <- which(data$penta1_health_card=="Don't know" | data$penta1_health_card=="No")
data$penta1_vac[ndk] <- 0 

data$penta2_vac <- rep(1, nrow(data))
ndk <- which(data$penta2_health_card=="Don't know" | data$penta2_health_card=="No")
data$penta2_vac[ndk] <- 0

data$penta3_vac <- rep(1, nrow(data))
ndk <- which(data$penta3_health_card=="Don't know" | data$penta3_health_card=="No")
data$penta3_vac[ndk] <- 0

data$opv1_vac <- rep(1, nrow(data))
ndk <- which(data$opv1_health_card=="Don't know" | data$opv1_health_card=="No")
data$opv1_vac[ndk] <- 0 

data$opv2_vac <- rep(1, nrow(data))
ndk <- which(data$opv2_health_card=="Don't know" | data$opv2_health_card=="No")
data$opv2_vac[ndk] <- 0

data$opv3_vac <- rep(1, nrow(data))
ndk <- which(data$opv3_health_card=="Don't know" | data$opv3_health_card=="No")
data$opv3_vac[ndk] <- 0

data$mcv1_vac <- rep(1, nrow(data))
ndk <- which(data$mcv1_health_card=="Don't know" | data$mcv1_health_card=="No")
data$mcv1_vac[ndk] <- 0



#subset data to two age cohorts#################################################################################################### 
##(12-23 m)##
data.1 <- subset(data, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
data.2 <- subset(data, age_in_month > 23)

##(12-35 m)##
data.3 <- subset(data, age_in_month > 11)



## Survey-weighted calculations####################################################################################################
## apply survey design (id is cluster id, i.e the unique id of the PSUs)
##(12-23 m)##
DHSdesign.1 <- svydesign(id = data.1$cluster, strata = data.1$strata, weights = data.1$sample_wt, data = data.1)

##(24-35 m)##
DHSdesign.2 <- svydesign(id = data.2$cluster, strata = data.2$strata, weights = data.2$sample_wt, data = data.2) 

##(12-35 m)##
DHSdesign.3 <- svydesign(id = data.3$cluster, strata = data.3$strata, weights = data.3$sample_wt, data = data.3)

options("survey.lonely.psu" = "remove") ## To avoid errors from lonely PSUs


##Calculate National-level (adm0) vaccination coverage#############################################################################
bcg12_23 <- svymean(~bcg_vac, DHSdesign.1, na.rm = T)
bcg12_23 <- c(bcg12_23, confint(bcg12_23)); bcg12_23
bcg24_35 <- svymean(~bcg_vac, DHSdesign.2, na.rm = T)
bcg24_35 <- c(bcg24_35, confint(bcg24_35)); bcg24_35

hepB012_23 <- svymean(~hepB0_vac, DHSdesign.1, na.rm = T)
hepB012_23 <- c(hepB012_23, confint(hepB012_23)); hepB012_23
hepB024_35 <- svymean(~hepB0_vac, DHSdesign.2, na.rm = T)
hepB024_35 <- c(hepB024_35, confint(hepB024_35)); hepB024_35

opv1_12_23 <- svymean(~opv1_vac, DHSdesign.1, na.rm = T)
opv1_12_23 <- c(opv1_12_23, confint(opv1_12_23)); opv1_12_23
opv1_24_35 <- svymean(~opv1_vac, DHSdesign.2, na.rm = T)
opv1_24_35 <- c(opv1_24_35, confint(opv1_24_35)); opv1_24_35

opv2_12_23 <- svymean(~opv2_vac, DHSdesign.1, na.rm = T)
opv2_12_23 <- c(opv2_12_23, confint(opv2_12_23)); opv2_12_23
opv2_24_35 <- svymean(~opv2_vac, DHSdesign.2, na.rm = T)
opv2_24_35 <- c(opv2_24_35, confint(opv2_24_35)); opv2_24_35

opv3_12_23 <- svymean(~opv3_vac, DHSdesign.1, na.rm = T)
opv3_12_23 <- c(opv3_12_23, confint(opv3_12_23)); opv3_12_23
opv3_24_35 <- svymean(~opv3_vac, DHSdesign.2, na.rm = T)
opv3_24_35 <- c(opv3_24_35, confint(opv3_24_35)); opv3_24_35

penta1_12_23 <- svymean(~penta1_vac, DHSdesign.1, na.rm = T)
penta1_12_23 <- c(penta1_12_23, confint(penta1_12_23)); penta1_12_23
penta1_24_35 <- svymean(~penta1_vac, DHSdesign.2, na.rm = T)
penta1_24_35 <- c(penta1_24_35, confint(penta1_24_35)); penta1_24_35

penta2_12_23 <- svymean(~penta2_vac, DHSdesign.1, na.rm = T)
penta2_12_23 <- c(penta2_12_23, confint(penta2_12_23)); penta2_12_23
penta2_24_35 <- svymean(~penta2_vac, DHSdesign.2, na.rm = T)
penta2_24_35 <- c(penta2_24_35, confint(penta2_24_35)); penta2_24_35

penta3_12_23 <- svymean(~penta3_vac, DHSdesign.1, na.rm = T)
penta3_12_23 <- c(penta3_12_23, confint(penta3_12_23)); penta3_12_23
penta3_24_35 <- svymean(~penta3_vac, DHSdesign.2, na.rm = T)
penta3_24_35 <- c(penta3_24_35, confint(penta3_24_35)); penta3_24_35

mcv1_12_23 <- svymean(~mcv1_vac, DHSdesign.1, na.rm = T)
mcv1_12_23 <- c(mcv1_12_23, confint(mcv1_12_23)); mcv1_12_23
mcv1_24_35 <- svymean(~mcv1_vac, DHSdesign.2, na.rm = T)
mcv1_24_35 <- c(mcv1_24_35, confint(mcv1_24_35)); mcv1_24_35




#Calculate Regional-level (adm1) vaccination coverage##############################################################################
R.bcg12_23 <- svyby(~bcg_vac, ~region, DHSdesign.1, svymean, vartype = "ci", na.rm=TRUE); R.bcg12_23
R.bcg24_35 <- svyby(~bcg_vac, ~region, DHSdesign.2, svymean, vartype = "ci", na.rm=TRUE); R.bcg24_35

R.hepB012_23 <- svyby(~hepB0_vac, ~region, DHSdesign.1, svymean, vartype = "ci", na.rm=TRUE); R.hepB012_23
R.hepB024_35 <- svyby(~hepB0_vac, ~region, DHSdesign.2, svymean, vartype = "ci", na.rm=TRUE); R.hepB024_35

R.opv1_12_23 <- svyby(~opv1_vac, ~region, DHSdesign.1, svymean, vartype = "ci", na.rm=TRUE); R.opv1_12_23
R.opv1_24_35 <- svyby(~opv1_vac, ~region, DHSdesign.2, svymean, vartype = "ci", na.rm=TRUE); R.opv1_24_35

R.opv2_12_23 <- svyby(~opv2_vac, ~region, DHSdesign.1, svymean, vartype = "ci", na.rm=TRUE); R.opv2_12_23
R.opv2_24_35 <- svyby(~opv2_vac, ~region, DHSdesign.2, svymean, vartype = "ci", na.rm=TRUE); R.opv2_24_35

R.opv3_12_23 <- svyby(~opv3_vac, ~region, DHSdesign.1, svymean, vartype = "ci", na.rm=TRUE); R.opv3_12_23
R.opv3_24_35 <- svyby(~opv3_vac, ~region, DHSdesign.2, svymean, vartype = "ci", na.rm=TRUE); R.opv3_24_35

R.penta1_12_23 <- svyby(~penta1_vac, ~region, DHSdesign.1, svymean, vartype = "ci", na.rm=TRUE); R.penta1_12_23
R.penta1_24_35 <- svyby(~penta1_vac, ~region, DHSdesign.2, svymean, vartype = "ci", na.rm=TRUE); R.penta1_24_35

R.penta2_12_23 <- svyby(~penta2_vac, ~region, DHSdesign.1, svymean, vartype = "ci", na.rm=TRUE); R.penta2_12_23
R.penta2_24_35 <- svyby(~penta2_vac, ~region, DHSdesign.2, svymean, vartype = "ci", na.rm=TRUE); R.penta2_24_35

R.penta3_12_23 <- svyby(~penta3_vac, ~region, DHSdesign.1, svymean, vartype = "ci", na.rm=TRUE); R.penta3_12_23
R.penta3_24_35 <- svyby(~penta3_vac, ~region, DHSdesign.2, svymean, vartype = "ci", na.rm=TRUE); R.penta3_24_35

R.mcv1_12_23 <- svyby(~mcv1_vac, ~region, DHSdesign.1, svymean, vartype = "ci", na.rm=TRUE); R.mcv1_12_23
R.mcv1_24_35 <- svyby(~mcv1_vac, ~region, DHSdesign.2, svymean, vartype = "ci", na.rm=TRUE); R.mcv1_24_35



#Timeliness analysis###############################################################################################################
##Create new vax variables with 0's and 1's########################################################################################

###categorical#####

##hepB0##
data$hepB0_timely <- rep(0, nrow(data))
data$hepB0_timely[is.na(data$hepB0_age_vac_cat)] <- NA
ndk <- which(data$hepB0_age_vac_cat=="0-1")
data$hepB0_timely[ndk] <- 1 

data$hepB0_delayed <- rep(0, nrow(data))
data$hepB0_delayed[is.na(data$hepB0_age_vac_cat)] <- NA
ndk <- which(data$hepB0_age_vac_cat=="Feb-14" | data$hepB0_age_vac_cat=="delayed")
data$hepB0_delayed[ndk] <- 1 

##bcg##
data$bcg_timely <- rep(0, nrow(data))
data$bcg_timely[is.na(data$bcg_age_vac_cat)] <- NA
ndk <- which(data$bcg_age_vac_cat=="timely")
data$bcg_timely[ndk] <- 1

data$bcg_delayed <- rep(0, nrow(data))
data$bcg_delayed[is.na(data$bcg_age_vac_cat)] <- NA
ndk <- which(data$bcg_age_vac_cat=="delayed")
data$bcg_delayed[ndk] <- 1

##opv1##
data$opv1_timely <- rep(0, nrow(data))
data$opv1_timely[is.na(data$opv1_age_vac_cat)] <- NA
ndk <- which(data$opv1_age_vac_cat=="61-90")
data$opv1_timely[ndk] <- 1

data$opv1_early <- rep(0, nrow(data))
data$opv1_early[is.na(data$opv1_age_vac_cat)] <- NA
ndk <- which(data$opv1_age_vac_cat=="early")
data$opv1_early[ndk] <- 1

data$opv1_delayed <- rep(0, nrow(data))
data$opv1_delayed[is.na(data$opv1_age_vac_cat)] <- NA
ndk <- which(data$opv1_age_vac_cat=="delayed")
data$opv1_delayed[ndk] <- 1

##opv2##
data$opv2_timely <- rep(0, nrow(data))
data$opv2_timely[is.na(data$opv2_age_vac_cat)] <- NA
ndk <- which(data$opv2_age_vac_cat=="91-120")
data$opv2_timely[ndk] <- 1

data$opv2_early <- rep(0, nrow(data))
data$opv2_early[is.na(data$opv2_age_vac_cat)] <- NA
ndk <- which(data$opv2_age_vac_cat=="early")
data$opv2_early[ndk] <- 1

data$opv2_delayed <- rep(0, nrow(data))
data$opv2_delayed[is.na(data$opv2_age_vac_cat)] <- NA
ndk <- which(data$opv2_age_vac_cat=="delayed")
data$opv2_delayed[ndk] <- 1

##opv3##
data$opv3_timely <- rep(0, nrow(data))
data$opv3_timely[is.na(data$opv3_age_vac_cat)] <- NA
ndk <- which(data$opv3_age_vac_cat=="121-150")
data$opv3_timely[ndk] <- 1

data$opv3_early <- rep(0, nrow(data))
data$opv3_early[is.na(data$opv3_age_vac_cat)] <- NA
ndk <- which(data$opv3_age_vac_cat=="early")
data$opv3_early[ndk] <- 1

data$opv3_delayed <- rep(0, nrow(data))
data$opv3_delayed[is.na(data$opv3_age_vac_cat)] <- NA
ndk <- which(data$opv3_age_vac_cat=="delayed")
data$opv3_delayed[ndk] <- 1

##penta1##
data$penta1_timely <- rep(0, nrow(data))
data$penta1_timely[is.na(data$penta1_age_vac_cat)] <- NA
ndk <- which(data$penta1_age_vac_cat=="61-90")
data$penta1_timely[ndk] <- 1

data$penta1_early <- rep(0, nrow(data))
data$penta1_early[is.na(data$penta1_age_vac_cat)] <- NA
ndk <- which(data$penta1_age_vac_cat=="early")
data$penta1_early[ndk] <- 1

data$penta1_delayed <- rep(0, nrow(data))
data$penta1_delayed[is.na(data$penta1_age_vac_cat)] <- NA
ndk <- which(data$penta1_age_vac_cat=="delayed")
data$penta1_delayed[ndk] <- 1

##penta2##
data$penta2_timely <- rep(0, nrow(data))
data$penta2_timely[is.na(data$penta2_age_vac_cat)] <- NA
ndk <- which(data$penta2_age_vac_cat=="91-120")
data$penta2_timely[ndk] <- 1

data$penta2_early <- rep(0, nrow(data))
data$penta2_early[is.na(data$penta2_age_vac_cat)] <- NA
ndk <- which(data$penta2_age_vac_cat=="early")
data$penta2_early[ndk] <- 1

data$penta2_delayed <- rep(0, nrow(data))
data$penta2_delayed[is.na(data$penta2_age_vac_cat)] <- NA
ndk <- which(data$penta2_age_vac_cat=="delayed")
data$penta2_delayed[ndk] <- 1

##penta3##
data$penta3_timely <- rep(0, nrow(data))
data$penta3_timely[is.na(data$penta3_age_vac_cat)] <- NA
ndk <- which(data$penta3_age_vac_cat=="121-150")
data$penta3_timely[ndk] <- 1

data$penta3_early <- rep(0, nrow(data))
data$penta3_early[is.na(data$penta3_age_vac_cat)] <- NA
ndk <- which(data$penta3_age_vac_cat=="early")
data$penta3_early[ndk] <- 1

data$penta3_delayed <- rep(0, nrow(data))
data$penta3_delayed[is.na(data$penta3_age_vac_cat)] <- NA
ndk <- which(data$penta3_age_vac_cat=="delayed")
data$penta3_delayed[ndk] <- 1

##mcv1##
data$mcv1_timely <- rep(0, nrow(data))
data$mcv1_timely[is.na(data$mcv1_age_vac_cat)] <- NA
ndk <- which(data$mcv1_age_vac_cat=="271-300")
data$mcv1_timely[ndk] <- 1

data$mcv1_early <- rep(0, nrow(data))
data$mcv1_early[is.na(data$mcv1_age_vac_cat)] <- NA
ndk <- which(data$mcv1_age_vac_cat=="early")
data$mcv1_early[ndk] <- 1

data$mcv1_delayed <- rep(0, nrow(data))
data$mcv1_delayed[is.na(data$mcv1_age_vac_cat)] <- NA
ndk <- which(data$mcv1_age_vac_cat=="delayed")
data$mcv1_delayed[ndk] <- 1


##penta1_penta2##
data$p1_p2_timely <- rep(0, nrow(data))
data$p1_p2_timely[is.na(data$penta1_penta2_interval_cat)] <- NA
ndk <- which(data$penta1_penta2_interval_cat=="28-56")
data$p1_p2_timely[ndk] <- 1 

data$p1_p2_untimely_interval <- rep(0, nrow(data))
data$p1_p2_untimely_interval[is.na(data$penta1_penta2_interval_cat)] <- NA
ndk <- which(data$penta1_penta2_interval_cat=="interval_early" | data$penta1_penta2_interval_cat=="interval_delayed")
data$p1_p2_untimely_interval[ndk] <- 1 

data$p1_p2_interval_early <- rep(0, nrow(data))
data$p1_p2_interval_early[is.na(data$penta1_penta2_interval_cat)] <- NA
ndk <- which(data$penta1_penta2_interval_cat=="interval_early")
data$p1_p2_interval_early[ndk] <- 1 

data$p1_p2_interval_delayed <- rep(0, nrow(data))
data$p1_p2_interval_delayed[is.na(data$penta1_penta2_interval_cat)] <- NA
ndk <- which(data$penta1_penta2_interval_cat=="interval_delayed")
data$p1_p2_interval_delayed[ndk] <- 1 


##penta2_penta3##
data$p2_p3_timely <- rep(0, nrow(data))
data$p2_p3_timely[is.na(data$penta2_penta3_interval_cat)] <- NA
ndk <- which(data$penta2_penta3_interval_cat=="28-56")
data$p2_p3_timely[ndk] <- 1 

data$p2_p3_untimely_interval <- rep(0, nrow(data))
data$p2_p3_untimely_interval[is.na(data$penta2_penta3_interval_cat)] <- NA
ndk <- which(data$penta2_penta3_interval_cat=="interval_early" | data$penta2_penta3_interval_cat=="interval_delayed")
data$p2_p3_untimely_interval[ndk] <- 1 

data$p2_p3_interval_early <- rep(0, nrow(data))
data$p2_p3_interval_early[is.na(data$penta2_penta3_interval_cat)] <- NA
ndk <- which(data$penta2_penta3_interval_cat=="interval_early")
data$p2_p3_interval_early[ndk] <- 1 

data$p2_p3_interval_delayed <- rep(0, nrow(data))
data$p2_p3_interval_delayed[is.na(data$penta2_penta3_interval_cat)] <- NA
ndk <- which(data$penta2_penta3_interval_cat=="interval_delayed")
data$p2_p3_interval_delayed[ndk] <- 1 


##opv1_opv2##
data$op1_op2_timely <- rep(0, nrow(data))
data$op1_op2_timely[is.na(data$opv1_opv2_interval_cat)] <- NA
ndk <- which(data$opv1_opv2_interval_cat=="28-56")
data$op1_op2_timely[ndk] <- 1 

data$op1_op2_untimely_interval <- rep(0, nrow(data))
data$op1_op2_untimely_interval[is.na(data$opv1_opv2_interval_cat)] <- NA
ndk <- which(data$opv1_opv2_interval_cat=="interval_early" | data$opv1_opv2_interval_cat=="interval_delayed")
data$op1_op2_untimely_interval[ndk] <- 1 

data$op1_op2_interval_early <- rep(0, nrow(data))
data$op1_op2_interval_early[is.na(data$opv1_opv2_interval_cat)] <- NA
ndk <- which(data$opv1_opv2_interval_cat=="interval_early")
data$op1_op2_interval_early[ndk] <- 1 

data$op1_op2_interval_delayed <- rep(0, nrow(data))
data$op1_op2_interval_delayed[is.na(data$opv1_opv2_interval_cat)] <- NA
ndk <- which(data$opv1_opv2_interval_cat=="interval_delayed")
data$op1_op2_interval_delayed[ndk] <- 1 



##opv2_opv3##
data$op2_op3_timely <- rep(0, nrow(data))
data$op2_op3_timely[is.na(data$opv2_opv3_interval_cat)] <- NA
ndk <- which(data$opv2_opv3_interval_cat=="28-56")
data$op2_op3_timely[ndk] <- 1 

data$op2_op3_untimely_interval <- rep(0, nrow(data))
data$op2_op3_untimely_interval[is.na(data$opv2_opv3_interval_cat)] <- NA
ndk <- which(data$opv2_opv3_interval_cat=="interval_early" | data$opv2_opv3_interval_cat=="interval_delayed")
data$op2_op3_untimely_interval[ndk] <- 1 

data$op2_op3_interval_early <- rep(0, nrow(data))
data$op2_op3_interval_early[is.na(data$opv2_opv3_interval_cat)] <- NA
ndk <- which(data$opv2_opv3_interval_cat=="interval_early")
data$op2_op3_interval_early[ndk] <- 1 

data$op2_op3_interval_delayed <- rep(0, nrow(data))
data$op2_op3_interval_delayed[is.na(data$opv2_opv3_interval_cat)] <- NA
ndk <- which(data$opv2_opv3_interval_cat=="interval_delayed")
data$op2_op3_interval_delayed[ndk] <- 1 


#subset data to two age cohorts#################################################################################################### 
##(12-23 m)##
data.1 <- subset(data, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
data.2 <- subset(data, age_in_month > 23)

##(12-35 m)##
data.3 <- subset(data, age_in_month > 11)


## Survey-weighted calculations####################################################################################################
## apply survey design (id is cluster id, i.e the unique id of the PSUs)
##(12-23 m)##
DHSdesign.1 <- svydesign(id = data.1$cluster, strata = data.1$strata, weights = data.1$sample_wt, data = data.1)

##(24-35 m)##
DHSdesign.2 <- svydesign(id = data.2$cluster, strata = data.2$strata, weights = data.2$sample_wt, data = data.2) 

##(12-35 m)##
DHSdesign.3 <- svydesign(id = data.3$cluster, strata = data.3$strata, weights = data.3$sample_wt, data = data.3)

##(overall)##
DHSdesign <- svydesign(id = data$cluster, strata = data$strata, weights = data$sample_wt, data = data)

options("survey.lonely.psu" = "remove") ## To avoid errors from lonely PSUs


##Calculate National-level (adm0) vaccination timeliness (categorical)########################################################################
##hepB0##
hepB0_timely_12_23 <- svymean(~hepB0_timely, DHSdesign.1, na.rm = T)
hepB0_timely_12_23 <- c(hepB0_timely_12_23, confint(hepB0_timely_12_23)); hepB0_timely_12_23
hepB0_timely_24_35 <- svymean(~hepB0_timely, DHSdesign.2, na.rm = T)
hepB0_timely_24_35 <- c(hepB0_timely_24_35, confint(hepB0_timely_24_35)); hepB0_timely_24_35

hepB0_delayed_12_23 <- svymean(~hepB0_delayed, DHSdesign.1, na.rm = T)
hepB0_delayed_12_23 <- c(hepB0_delayed_12_23, confint(hepB0_delayed_12_23)); hepB0_delayed_12_23
hepB0_delayed_24_35 <- svymean(~hepB0_delayed, DHSdesign.2, na.rm = T)
hepB0_delayed_24_35 <- c(hepB0_delayed_24_35, confint(hepB0_delayed_24_35)); hepB0_delayed_24_35


##bcg##
bcg_timely_12_23 <- svymean(~bcg_timely, DHSdesign.1, na.rm = T)
bcg_timely_12_23 <- c(bcg_timely_12_23, confint(bcg_timely_12_23)); bcg_timely_12_23
bcg_timely_24_35 <- svymean(~bcg_timely, DHSdesign.2, na.rm = T)
bcg_timely_24_35 <- c(bcg_timely_24_35, confint(bcg_timely_24_35)); bcg_timely_24_35

bcg_delayed_12_23 <- svymean(~bcg_delayed, DHSdesign.1, na.rm = T)
bcg_delayed_12_23 <- c(bcg_delayed_12_23, confint(bcg_delayed_12_23)); bcg_delayed_12_23
bcg_delayed_24_35 <- svymean(~bcg_delayed, DHSdesign.2, na.rm = T)
bcg_delayed_24_35 <- c(bcg_delayed_24_35, confint(bcg_delayed_24_35)); bcg_delayed_24_35

##opv1##
opv1_timely_12_23 <- svymean(~opv1_timely, DHSdesign.1, na.rm = T)
opv1_timely_12_23 <- c(opv1_timely_12_23, confint(opv1_timely_12_23)); opv1_timely_12_23
opv1_timely_24_35 <- svymean(~opv1_timely, DHSdesign.2, na.rm = T)
opv1_timely_24_35 <- c(opv1_timely_24_35, confint(opv1_timely_24_35)); opv1_timely_24_35

opv1_early_12_23 <- svymean(~opv1_early, DHSdesign.1, na.rm = T)
opv1_early_12_23 <- c(opv1_early_12_23, confint(opv1_early_12_23)); opv1_early_12_23
opv1_early_24_35 <- svymean(~opv1_early, DHSdesign.2, na.rm = T)
opv1_early_24_35 <- c(opv1_early_24_35, confint(opv1_early_24_35)); opv1_early_24_35

opv1_delayed_12_23 <- svymean(~opv1_delayed, DHSdesign.1, na.rm = T)
opv1_delayed_12_23 <- c(opv1_delayed_12_23, confint(opv1_delayed_12_23)); opv1_delayed_12_23
opv1_delayed_24_35 <- svymean(~opv1_delayed, DHSdesign.2, na.rm = T)
opv1_delayed_24_35 <- c(opv1_delayed_24_35, confint(opv1_delayed_24_35))

##opv2##
opv2_timely_12_23 <- svymean(~opv2_timely, DHSdesign.1, na.rm = T)
opv2_timely_12_23 <- c(opv2_timely_12_23, confint(opv2_timely_12_23)); opv2_timely_12_23
opv2_timely_24_35 <- svymean(~opv2_timely, DHSdesign.2, na.rm = T)
opv2_timely_24_35 <- c(opv2_timely_24_35, confint(opv2_timely_24_35)); opv2_timely_24_35

opv2_early_12_23 <- svymean(~opv2_early, DHSdesign.1, na.rm = T)
opv2_early_12_23 <- c(opv2_early_12_23, confint(opv2_early_12_23)); opv2_early_12_23
opv2_early_24_35 <- svymean(~opv2_early, DHSdesign.2, na.rm = T)
opv2_early_24_35 <- c(opv2_early_24_35, confint(opv2_early_24_35)); opv2_early_24_35

opv2_delayed_12_23 <- svymean(~opv2_delayed, DHSdesign.1, na.rm = T)
opv2_delayed_12_23 <- c(opv2_delayed_12_23, confint(opv2_delayed_12_23)); opv2_delayed_12_23
opv2_delayed_24_35 <- svymean(~opv2_delayed, DHSdesign.2, na.rm = T)
opv2_delayed_24_35 <- c(opv2_delayed_24_35, confint(opv2_delayed_24_35)); opv2_delayed_24_35

##opv3##
opv3_timely_12_23 <- svymean(~opv3_timely, DHSdesign.1, na.rm = T)
opv3_timely_12_23 <- c(opv3_timely_12_23, confint(opv3_timely_12_23)); opv3_timely_12_23
opv3_timely_24_35 <- svymean(~opv3_timely, DHSdesign.2, na.rm = T)
opv3_timely_24_35 <- c(opv3_timely_24_35, confint(opv3_timely_24_35)); opv3_timely_24_35

opv3_early_12_23 <- svymean(~opv3_early, DHSdesign.1, na.rm = T)
opv3_early_12_23 <- c(opv3_early_12_23, confint(opv3_early_12_23)); opv3_early_12_23
opv3_early_24_35 <- svymean(~opv3_early, DHSdesign.2, na.rm = T)
opv3_early_24_35 <- c(opv3_early_24_35, confint(opv3_early_24_35)); opv3_early_24_35

opv3_delayed_12_23 <- svymean(~opv3_delayed, DHSdesign.1, na.rm = T)
opv3_delayed_12_23 <- c(opv3_delayed_12_23, confint(opv3_delayed_12_23)); opv3_delayed_12_23
opv3_delayed_24_35 <- svymean(~opv3_delayed, DHSdesign.2, na.rm = T)
opv3_delayed_24_35 <- c(opv3_delayed_24_35, confint(opv3_delayed_24_35)); opv3_delayed_24_35

##penta1##
penta1_timely_12_23 <- svymean(~penta1_timely, DHSdesign.1, na.rm = T)
penta1_timely_12_23 <- c(penta1_timely_12_23, confint(penta1_timely_12_23)); penta1_timely_12_23
penta1_timely_24_35 <- svymean(~penta1_timely, DHSdesign.2, na.rm = T)
penta1_timely_24_35 <- c(penta1_timely_24_35, confint(penta1_timely_24_35)); penta1_timely_24_35

penta1_early_12_23 <- svymean(~penta1_early, DHSdesign.1, na.rm = T)
penta1_early_12_23 <- c(penta1_early_12_23, confint(penta1_early_12_23)); penta1_early_12_23
penta1_early_24_35 <- svymean(~penta1_early, DHSdesign.2, na.rm = T)
penta1_early_24_35 <- c(penta1_early_24_35, confint(penta1_early_24_35)); penta1_early_24_35

penta1_delayed_12_23 <- svymean(~penta1_delayed, DHSdesign.1, na.rm = T)
penta1_delayed_12_23 <- c(penta1_delayed_12_23, confint(penta1_delayed_12_23)); penta1_delayed_12_23
penta1_delayed_24_35 <- svymean(~penta1_delayed, DHSdesign.2, na.rm = T)
penta1_delayed_24_35 <- c(penta1_delayed_24_35, confint(penta1_delayed_24_35)); penta1_delayed_24_35

##penta2##
penta2_timely_12_23 <- svymean(~penta2_timely, DHSdesign.1, na.rm = T)
penta2_timely_12_23 <- c(penta2_timely_12_23, confint(penta2_timely_12_23)); penta2_timely_12_23
penta2_timely_24_35 <- svymean(~penta2_timely, DHSdesign.2, na.rm = T)
penta2_timely_24_35 <- c(penta2_timely_24_35, confint(penta2_timely_24_35)); penta2_timely_24_35

penta2_early_12_23 <- svymean(~penta2_early, DHSdesign.1, na.rm = T)
penta2_early_12_23 <- c(penta2_early_12_23, confint(penta2_early_12_23)); penta2_early_12_23
penta2_early_24_35 <- svymean(~penta2_early, DHSdesign.2, na.rm = T)
penta2_early_24_35 <- c(penta2_early_24_35, confint(penta2_early_24_35)); penta2_early_24_35

penta2_delayed_12_23 <- svymean(~penta2_delayed, DHSdesign.1, na.rm = T)
penta2_delayed_12_23 <- c(penta2_delayed_12_23, confint(penta2_delayed_12_23)); penta2_delayed_12_23
penta2_delayed_24_35 <- svymean(~penta2_delayed, DHSdesign.2, na.rm = T)
penta2_delayed_24_35 <- c(penta2_delayed_24_35, confint(penta2_delayed_24_35)); penta2_delayed_24_35

##penta3##
penta3_timely_12_23 <- svymean(~penta3_timely, DHSdesign.1, na.rm = T)
penta3_timely_12_23 <- c(penta3_timely_12_23, confint(penta3_timely_12_23)); penta3_timely_12_23
penta3_timely_24_35 <- svymean(~penta3_timely, DHSdesign.2, na.rm = T)
penta3_timely_24_35 <- c(penta3_timely_24_35, confint(penta3_timely_24_35)); penta3_timely_24_35

penta3_early_12_23 <- svymean(~penta3_early, DHSdesign.1, na.rm = T)
penta3_early_12_23 <- c(penta3_early_12_23, confint(penta3_early_12_23)); penta3_early_12_23
penta3_early_24_35 <- svymean(~penta3_early, DHSdesign.2, na.rm = T)
penta3_early_24_35 <- c(penta3_early_24_35, confint(penta3_early_24_35)); penta3_early_24_35

penta3_delayed_12_23 <- svymean(~penta3_delayed, DHSdesign.1, na.rm = T)
penta3_delayed_12_23 <- c(penta3_delayed_12_23, confint(penta3_delayed_12_23)); penta3_delayed_12_23
penta3_delayed_24_35 <- svymean(~penta3_delayed, DHSdesign.2, na.rm = T)
penta3_delayed_24_35 <- c(penta3_delayed_24_35, confint(penta3_delayed_24_35)); penta3_delayed_24_35

##mcv1##
mcv1_timely_12_23 <- svymean(~mcv1_timely, DHSdesign.1, na.rm = T)
mcv1_timely_12_23 <- c(mcv1_timely_12_23, confint(mcv1_timely_12_23)); mcv1_timely_12_23
mcv1_timely_24_35 <- svymean(~mcv1_timely, DHSdesign.2, na.rm = T)
mcv1_timely_24_35 <- c(mcv1_timely_24_35, confint(mcv1_timely_24_35)); mcv1_timely_24_35

mcv1_early_12_23 <- svymean(~mcv1_early, DHSdesign.1, na.rm = T)
mcv1_early_12_23 <- c(mcv1_early_12_23, confint(mcv1_early_12_23)); mcv1_early_12_23
mcv1_early_24_35 <- svymean(~mcv1_early, DHSdesign.2, na.rm = T)
mcv1_early_24_35 <- c(mcv1_early_24_35, confint(mcv1_early_24_35)); mcv1_early_24_35

mcv1_delayed_12_23 <- svymean(~mcv1_delayed, DHSdesign.1, na.rm = T)
mcv1_delayed_12_23 <- c(mcv1_delayed_12_23, confint(mcv1_delayed_12_23)); mcv1_delayed_12_23
mcv1_delayed_24_35 <- svymean(~mcv1_delayed, DHSdesign.2, na.rm = T)
mcv1_delayed_24_35 <- c(mcv1_delayed_24_35, confint(mcv1_delayed_24_35)); mcv1_delayed_24_35

##penta1_penta2##
p1_p2_timely_12_23 <- svymean(~p1_p2_timely, DHSdesign.1, na.rm = T)
p1_p2_timely_12_23 <- c(p1_p2_timely_12_23, confint(p1_p2_timely_12_23)); p1_p2_timely_12_23
p1_p2_timely_24_35 <- svymean(~p1_p2_timely, DHSdesign.2, na.rm = T)
p1_p2_timely_24_35 <- c(p1_p2_timely_24_35, confint(p1_p2_timely_24_35)); p1_p2_timely_24_35

p1_p2_untimely_interval_12_23 <- svymean(~p1_p2_untimely_interval, DHSdesign.1, na.rm = T)
p1_p2_untimely_interval_12_23 <- c(p1_p2_untimely_interval_12_23, confint(p1_p2_untimely_interval_12_23)); p1_p2_untimely_interval_12_23
p1_p2_untimely_interval_24_35 <- svymean(~p1_p2_untimely_interval, DHSdesign.2, na.rm = T)
p1_p2_untimely_interval_24_35 <- c(p1_p2_untimely_interval_24_35, confint(p1_p2_untimely_interval_24_35)); p1_p2_untimely_interval_24_35

p1_p2_interval_early_12_23 <- svymean(~p1_p2_interval_early, DHSdesign.1, na.rm = T)
p1_p2_interval_early_12_23 <- c(p1_p2_interval_early_12_23, confint(p1_p2_interval_early_12_23)); p1_p2_interval_early_12_23
p1_p2_interval_early_24_35 <- svymean(~p1_p2_interval_early, DHSdesign.2, na.rm = T)
p1_p2_interval_early_24_35 <- c(p1_p2_interval_early_24_35, confint(p1_p2_interval_early_24_35)); p1_p2_interval_early_24_35

p1_p2_interval_delayed_12_23 <- svymean(~p1_p2_interval_delayed, DHSdesign.1, na.rm = T)
p1_p2_interval_delayed_12_23 <- c(p1_p2_interval_delayed_12_23, confint(p1_p2_interval_delayed_12_23)); p1_p2_interval_delayed_12_23
p1_p2_interval_delayed_24_35 <- svymean(~p1_p2_interval_delayed, DHSdesign.2, na.rm = T)
p1_p2_interval_delayed_24_35 <- c(p1_p2_interval_delayed_24_35, confint(p1_p2_interval_delayed_24_35)); p1_p2_interval_delayed_24_35

##penta2_penta3##
p2_p3_timely_12_23 <- svymean(~p2_p3_timely, DHSdesign.1, na.rm = T)
p2_p3_timely_12_23 <- c(p2_p3_timely_12_23, confint(p2_p3_timely_12_23)); p2_p3_timely_12_23
p2_p3_timely_24_35 <- svymean(~p2_p3_timely, DHSdesign.2, na.rm = T)
p2_p3_timely_24_35 <- c(p2_p3_timely_24_35, confint(p2_p3_timely_24_35)); p2_p3_timely_24_35

p2_p3_untimely_interval_12_23 <- svymean(~p2_p3_untimely_interval, DHSdesign.1, na.rm = T)
p2_p3_untimely_interval_12_23 <- c(p2_p3_untimely_interval_12_23, confint(p2_p3_untimely_interval_12_23)); p2_p3_untimely_interval_12_23
p2_p3_untimely_interval_24_35 <- svymean(~p2_p3_untimely_interval, DHSdesign.2, na.rm = T)
p2_p3_untimely_interval_24_35 <- c(p2_p3_untimely_interval_24_35, confint(p2_p3_untimely_interval_24_35)); p2_p3_untimely_interval_24_35

p2_p3_interval_early_12_23 <- svymean(~p2_p3_interval_early, DHSdesign.1, na.rm = T)
p2_p3_interval_early_12_23 <- c(p2_p3_interval_early_12_23, confint(p2_p3_interval_early_12_23)); p2_p3_interval_early_12_23
p2_p3_interval_early_24_35 <- svymean(~p2_p3_interval_early, DHSdesign.2, na.rm = T)
p2_p3_interval_early_24_35 <- c(p2_p3_interval_early_24_35, confint(p2_p3_interval_early_24_35)); p2_p3_interval_early_24_35

p2_p3_interval_delayed_12_23 <- svymean(~p2_p3_interval_delayed, DHSdesign.1, na.rm = T)
p2_p3_interval_delayed_12_23 <- c(p2_p3_interval_delayed_12_23, confint(p2_p3_interval_delayed_12_23)); p2_p3_interval_delayed_12_23
p2_p3_interval_delayed_24_35 <- svymean(~p2_p3_interval_delayed, DHSdesign.2, na.rm = T)
p2_p3_interval_delayed_24_35 <- c(p2_p3_interval_delayed_24_35, confint(p2_p3_interval_delayed_24_35)); p2_p3_interval_delayed_24_35

##opv1_opv2##
op1_op2_timely_12_23 <- svymean(~op1_op2_timely, DHSdesign.1, na.rm = T)
op1_op2_timely_12_23 <- c(op1_op2_timely_12_23, confint(op1_op2_timely_12_23)); op1_op2_timely_12_23
op1_op2_timely_24_35 <- svymean(~op1_op2_timely, DHSdesign.2, na.rm = T)
op1_op2_timely_24_35 <- c(op1_op2_timely_24_35, confint(op1_op2_timely_24_35)); op1_op2_timely_24_35

op1_op2_untimely_interval_12_23 <- svymean(~op1_op2_untimely_interval, DHSdesign.1, na.rm = T)
op1_op2_untimely_interval_12_23 <- c(op1_op2_untimely_interval_12_23, confint(op1_op2_untimely_interval_12_23)); op1_op2_untimely_interval_12_23
op1_op2_untimely_interval_24_35 <- svymean(~op1_op2_untimely_interval, DHSdesign.2, na.rm = T)
op1_op2_untimely_interval_24_35 <- c(op1_op2_untimely_interval_24_35, confint(op1_op2_untimely_interval_24_35)); op1_op2_untimely_interval_24_35

op1_op2_interval_early_12_23 <- svymean(~op1_op2_interval_early, DHSdesign.1, na.rm = T)
op1_op2_interval_early_12_23 <- c(op1_op2_interval_early_12_23, confint(op1_op2_interval_early_12_23)); op1_op2_interval_early_12_23
op1_op2_interval_early_24_35 <- svymean(~op1_op2_interval_early, DHSdesign.2, na.rm = T)
op1_op2_interval_early_24_35 <- c(op1_op2_interval_early_24_35, confint(op1_op2_interval_early_24_35)); op1_op2_interval_early_24_35

op1_op2_interval_delayed_12_23 <- svymean(~op1_op2_interval_delayed, DHSdesign.1, na.rm = T)
op1_op2_interval_delayed_12_23 <- c(op1_op2_interval_delayed_12_23, confint(op1_op2_interval_delayed_12_23)); op1_op2_interval_delayed_12_23
op1_op2_interval_delayed_24_35 <- svymean(~op1_op2_interval_delayed, DHSdesign.2, na.rm = T)
op1_op2_interval_delayed_24_35 <- c(op1_op2_interval_delayed_24_35, confint(op1_op2_interval_delayed_24_35)); op1_op2_interval_delayed_24_35

##opv2_opv3##
op2_op3_timely_12_23 <- svymean(~op2_op3_timely, DHSdesign.1, na.rm = T)
op2_op3_timely_12_23 <- c(op2_op3_timely_12_23, confint(op2_op3_timely_12_23)); op2_op3_timely_12_23
op2_op3_timely_24_35 <- svymean(~op2_op3_timely, DHSdesign.2, na.rm = T)
op2_op3_timely_24_35 <- c(op2_op3_timely_24_35, confint(op2_op3_timely_24_35)); op2_op3_timely_24_35

op2_op3_untimely_interval_12_23 <- svymean(~op2_op3_untimely_interval, DHSdesign.1, na.rm = T)
op2_op3_untimely_interval_12_23 <- c(op2_op3_untimely_interval_12_23, confint(op2_op3_untimely_interval_12_23)); op2_op3_untimely_interval_12_23
op2_op3_untimely_interval_24_35 <- svymean(~op2_op3_untimely_interval, DHSdesign.2, na.rm = T)
op2_op3_untimely_interval_24_35 <- c(op2_op3_untimely_interval_24_35, confint(op2_op3_untimely_interval_24_35)); op2_op3_untimely_interval_24_35

op2_op3_interval_early_12_23 <- svymean(~op2_op3_interval_early, DHSdesign.1, na.rm = T)
op2_op3_interval_early_12_23 <- c(op2_op3_interval_early_12_23, confint(op2_op3_interval_early_12_23)); op2_op3_interval_early_12_23
op2_op3_interval_early_24_35 <- svymean(~op2_op3_interval_early, DHSdesign.2, na.rm = T)
op2_op3_interval_early_24_35 <- c(op2_op3_interval_early_24_35, confint(op2_op3_interval_early_24_35)); op2_op3_interval_early_24_35

op2_op3_interval_delayed_12_23 <- svymean(~op2_op3_interval_delayed, DHSdesign.1, na.rm = T)
op2_op3_interval_delayed_12_23 <- c(op2_op3_interval_delayed_12_23, confint(op2_op3_interval_delayed_12_23)); op2_op3_interval_delayed_12_23
op2_op3_interval_delayed_24_35 <- svymean(~op2_op3_interval_delayed, DHSdesign.2, na.rm = T)
op2_op3_interval_delayed_24_35 <- c(op2_op3_interval_delayed_24_35, confint(op2_op3_interval_delayed_24_35)); op2_op3_interval_delayed_24_35


##Calculate Regional-level (adm1) vaccination timeliness (categorical)########################################################################

##hepB0##
R.hepB0_timely_12_23 <- svyby(~hepB0_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.hepB0_timely_12_23
R.hepB0_timely_24_35 <- svyby(~hepB0_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.hepB0_timely_24_35

R.hepB0_delayed_12_23 <- svyby(~hepB0_delayed, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.hepB0_delayed_12_23
R.hepB0_delayed_24_35 <- svyby(~hepB0_delayed, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.hepB0_delayed_24_35
R.hepB0_delayed_overall <- svyby(~hepB0_delayed, ~region, DHSdesign, svymean, vartype=c("se","ci"), na.rm=TRUE); R.hepB0_delayed_overall


##bcg##
R.bcg_timely_12_23 <- svyby(~bcg_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.bcg_timely_12_23
R.bcg_timely_24_35 <- svyby(~bcg_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.bcg_timely_24_35

R.bcg_delayed_12_23 <- svyby(~bcg_delayed, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.bcg_delayed_12_23
R.bcg_delayed_24_35 <- svyby(~bcg_delayed, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.bcg_delayed_24_35

##opv1##
R.opv1_timely_12_23 <- svyby(~opv1_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv1_timely_12_23
R.opv1_timely_24_35 <- svyby(~opv1_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv1_timely_24_35

R.opv1_early_12_23 <- svyby(~opv1_early, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv1_early_12_23
R.opv1_early_24_35 <- svyby(~opv1_early, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv1_early_24_35

R.opv1_delayed_12_23 <- svyby(~opv1_delayed, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv1_delayed_12_23
R.opv1_delayed_24_35 <- svyby(~opv1_delayed, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv1_delayed_24_35

##opv2##
R.opv2_timely_12_23 <- svyby(~opv2_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv2_timely_12_23
R.opv2_timely_24_35 <- svyby(~opv2_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv2_timely_24_35

R.opv2_early_12_23 <- svyby(~opv2_early, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv2_early_12_23
R.opv2_early_24_35 <- svyby(~opv2_early, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv2_early_24_35

R.opv2_delayed_12_23 <- svyby(~opv2_delayed, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv2_delayed_12_23
R.opv2_delayed_24_35 <- svyby(~opv2_delayed, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv2_delayed_24_35

##opv3##
R.opv3_timely_12_23 <- svyby(~opv3_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv3_timely_12_23
R.opv3_timely_24_35 <- svyby(~opv3_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv3_timely_24_35

R.opv3_early_12_23 <- svyby(~opv3_early, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv3_early_12_23
R.opv3_early_24_35 <- svyby(~opv3_early, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv3_early_24_35

R.opv3_delayed_12_23 <- svyby(~opv3_delayed, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv3_delayed_12_23
R.opv3_delayed_24_35 <- svyby(~opv3_delayed, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.opv3_delayed_24_35

##penta1##
R.penta1_timely_12_23 <- svyby(~penta1_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta1_timely_12_23
R.penta1_timely_24_35 <- svyby(~penta1_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta1_timely_24_35

R.penta1_early_12_23 <- svyby(~penta1_early, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta1_early_12_23
R.penta1_early_24_35 <- svyby(~penta1_early, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta1_early_24_35

R.penta1_delayed_12_23 <- svyby(~penta1_delayed, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta1_delayed_12_23
R.penta1_delayed_24_35 <- svyby(~penta1_delayed, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta1_delayed_24_35

##penta2##
R.penta2_timely_12_23 <- svyby(~penta2_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta2_timely_12_23
R.penta2_timely_24_35 <- svyby(~penta2_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta2_timely_24_35

R.penta2_early_12_23 <- svyby(~penta2_early, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta2_early_12_23
R.penta2_early_24_35 <- svyby(~penta2_early, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta2_early_24_35

R.penta2_delayed_12_23 <- svyby(~penta2_delayed, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta2_delayed_12_23
R.penta2_delayed_24_35 <- svyby(~penta2_delayed, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta2_delayed_24_35

##penta3##
R.penta3_timely_12_23 <- svyby(~penta3_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta3_timely_12_23
R.penta3_timely_24_35 <- svyby(~penta3_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta3_timely_24_35

R.penta3_early_12_23 <- svyby(~penta3_early, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta3_early_12_23
R.penta3_early_24_35 <- svyby(~penta3_early, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta3_early_24_35

R.penta3_delayed_12_23 <- svyby(~penta3_delayed, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta3_delayed_12_23
R.penta3_delayed_24_35 <- svyby(~penta3_delayed, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta3_delayed_24_35
R.penta3_delayed_overall <- svyby(~penta3_delayed, ~region, DHSdesign, svymean, vartype=c("se","ci"), na.rm=TRUE); R.penta3_delayed_overall

##mcv1##
R.mcv1_timely_12_23 <- svyby(~mcv1_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.mcv1_timely_12_23
R.mcv1_timely_24_35 <- svyby(~mcv1_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.mcv1_timely_24_35

R.mcv1_early_12_23 <- svyby(~mcv1_early, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.mcv1_early_12_23
R.mcv1_early_24_35 <- svyby(~mcv1_early, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.mcv1_early_24_35

R.mcv1_delayed_12_23 <- svyby(~mcv1_delayed, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.mcv1_delayed_12_23
R.mcv1_delayed_24_35 <- svyby(~mcv1_delayed, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.mcv1_delayed_24_35
R.mcv1_delayed_12_35 <- svyby(~mcv1_delayed, ~region, DHSdesign.3, svymean, vartype=c("se","ci"), na.rm=TRUE); R.mcv1_delayed_12_35
R.mcv1_delayed_overall <- svyby(~mcv1_delayed, ~region, DHSdesign, svymean, vartype=c("se","ci"), na.rm=TRUE); R.mcv1_delayed_overall

##penta1_penta2##
R.p1_p2_timely_12_23 <- svyby(~p1_p2_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.p1_p2_timely_12_23
R.p1_p2_timely_24_35 <- svyby(~p1_p2_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.p1_p2_timely_24_35

R.p1_p2_untimely_interval_12_23 <- svyby(~p1_p2_untimely_interval, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.p1_p2_untimely_interval_12_23
R.p1_p2_untimely_interval_24_35 <- svyby(~p1_p2_untimely_interval, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.p1_p2_untimely_interval_24_35

##penta2_penta3##
R.p2_p3_timely_12_23 <- svyby(~p2_p3_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.p2_p3_timely_12_23
R.p2_p3_timely_24_35 <- svyby(~p2_p3_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.p2_p3_timely_24_35

R.p2_p3_untimely_interval_12_23 <- svyby(~p2_p3_untimely_interval, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.p2_p3_untimely_interval_12_23
R.p2_p3_untimely_interval_24_35 <- svyby(~p2_p3_untimely_interval, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.p2_p3_untimely_interval_24_35

##opv1_opv2##
R.op1_op2_timely_12_23 <- svyby(~op1_op2_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.op1_op2_timely_12_23
R.op1_op2_timely_24_35 <- svyby(~op1_op2_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.op1_op2_timely_24_35

R.op1_op2_untimely_interval_12_23 <- svyby(~op1_op2_untimely_interval, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.op1_op2_untimely_interval_12_23
R.op1_op2_untimely_interval_24_35 <- svyby(~op1_op2_untimely_interval, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.op1_op2_untimely_interval_24_35

##opv2_opv3##
R.op2_op3_timely_12_23 <- svyby(~op2_op3_timely, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.op2_op3_timely_12_23
R.op2_op3_timely_24_35 <- svyby(~op2_op3_timely, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.op2_op3_timely_24_35

R.op2_op3_untimely_interval_12_23 <- svyby(~op2_op3_untimely_interval, ~region, DHSdesign.1, svymean, vartype=c("se","ci"), na.rm=TRUE); R.op2_op3_untimely_interval_12_23
R.op2_op3_untimely_interval_24_35 <- svyby(~op2_op3_untimely_interval, ~region, DHSdesign.2, svymean, vartype=c("se","ci"), na.rm=TRUE); R.op2_op3_untimely_interval_24_35



###continuous timeliness (Weighted mean days delayed or early)#################################################################################
##Create new vax variables (i.e. subset data based on timeliness category for each vaccine)######################################

##hepB0##
data$hepB0_mean_delay <- data$hepB0_age_vac  
del <- which(data$hepB0_mean_delay < 2)
data$hepB0_mean_delay[del] <- NA 
data$hepB0_mean_delay = data$hepB0_mean_delay - 1


##bcg##
data$bcg_mean_delay <- data$bcg_age_vac  
del <- which(data$bcg_mean_delay < 8)
data$bcg_mean_delay[del] <- NA 
data$bcg_mean_delay = data$bcg_mean_delay - 7


##opv1##
data$opv1_mean_delay <- data$opv1_age_vac  
del <- which(data$opv1_mean_delay < 91)
data$opv1_mean_delay[del] <- NA
data$opv1_mean_delay = data$opv1_mean_delay - 90


data$opv1_mean_early <- data$opv1_age_vac  
del <- which(data$opv1_mean_early > 60)
data$opv1_mean_early[del] <- NA
data$opv1_mean_early = 61 - data$opv1_mean_early


##opv2##
data$opv2_mean_delay <- data$opv2_age_vac  
del <- which(data$opv2_mean_delay < 121)
data$opv2_mean_delay[del] <- NA
data$opv2_mean_delay = data$opv2_mean_delay - 120


data$opv2_mean_early <- data$opv2_age_vac  
del <- which(data$opv2_mean_early > 90)
data$opv2_mean_early[del] <- NA
data$opv2_mean_early = 91 - data$opv2_mean_early


##opv3##
data$opv3_mean_delay <- data$opv3_age_vac  
del <- which(data$opv3_mean_delay < 151)
data$opv3_mean_delay[del] <- NA
data$opv3_mean_delay = data$opv3_mean_delay - 150


data$opv3_mean_early <- data$opv3_age_vac  
del <- which(data$opv3_mean_early > 120)
data$opv3_mean_early[del] <- NA
data$opv3_mean_early = 121 - data$opv3_mean_early


##penta1##
data$penta1_mean_delay <- data$penta1_age_vac  
del <- which(data$penta1_mean_delay < 91)
data$penta1_mean_delay[del] <- NA
data$penta1_mean_delay = data$penta1_mean_delay - 90


data$penta1_mean_early <- data$penta1_age_vac  
del <- which(data$penta1_mean_early > 60)
data$penta1_mean_early[del] <- NA
data$penta1_mean_early = 61 - data$penta1_mean_early


##penta2##
data$penta2_mean_delay <- data$penta2_age_vac  
del <- which(data$penta2_mean_delay < 121)
data$penta2_mean_delay[del] <- NA
data$penta2_mean_delay = data$penta2_mean_delay - 120


data$penta2_mean_early <- data$penta2_age_vac  
del <- which(data$penta2_mean_early > 90)
data$penta2_mean_early[del] <- NA
data$penta2_mean_early = 91 - data$penta2_mean_early


##penta3##
data$penta3_mean_delay <- data$penta3_age_vac  
del <- which(data$penta3_mean_delay < 151)
data$penta3_mean_delay[del] <- NA
data$penta3_mean_delay = data$penta3_mean_delay - 150


data$penta3_mean_early <- data$penta3_age_vac  
del <- which(data$penta3_mean_early > 120)
data$penta3_mean_early[del] <- NA
data$penta3_mean_early = 121 - data$penta3_mean_early


##mcv1##
data$mcv1_mean_delay <- data$mcv1_age_vac  
del <- which(data$mcv1_mean_delay < 301)
data$mcv1_mean_delay[del] <- NA
data$mcv1_mean_delay = data$mcv1_mean_delay - 300


data$mcv1_mean_early <- data$mcv1_age_vac  
del <- which(data$mcv1_mean_early > 270)
data$mcv1_mean_early[del] <- NA
data$mcv1_mean_early = 271 - data$mcv1_mean_early



#subset data to two age cohorts#################################################################################################### 
##(12-23 m)##
data.1 <- subset(data, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
data.2 <- subset(data, age_in_month > 23)


## Survey-weighted calculations####################################################################################################
## apply survey design (id is cluster id, i.e the unique id of the PSUs)
##(12-23 m)##
DHSdesign.1 <- svydesign(id = data.1$cluster, strata = data.1$strata, weights = data.1$sample_wt, data = data.1)

##(24-35 m)##
DHSdesign.2 <- svydesign(id = data.2$cluster, strata = data.2$strata, weights = data.2$sample_wt, data = data.2) 

options("survey.lonely.psu" = "remove") ## To avoid errors from lonely PSUs



##Calculate National-level (adm0) vaccination timeliness (continuous)########################################################################
##hepB0##
hepB0_mean_delayed_12_23 <- svymean(~hepB0_mean_delay, DHSdesign.1, na.rm = T); hepB0_mean_delayed_12_23; confint(hepB0_mean_delayed_12_23)
hepB0_mean_delayed_24_35 <- svymean(~hepB0_mean_delay, DHSdesign.2, na.rm = T); hepB0_mean_delayed_24_35; confint(hepB0_mean_delayed_24_35)

##bcg
bcg_mean_delayed_12_23 <- svymean(~bcg_mean_delay, DHSdesign.1, na.rm = T); bcg_mean_delayed_12_23; confint(bcg_mean_delayed_12_23)
bcg_mean_delayed_24_35 <- svymean(~bcg_mean_delay, DHSdesign.2, na.rm = T); bcg_mean_delayed_24_35; confint(bcg_mean_delayed_24_35)

##opv1##
opv1_mean_delayed_12_23 <- svymean(~opv1_mean_delay, DHSdesign.1, na.rm = T); opv1_mean_delayed_12_23; confint(opv1_mean_delayed_12_23)
opv1_mean_delayed_24_35 <- svymean(~opv1_mean_delay, DHSdesign.2, na.rm = T); opv1_mean_delayed_24_35; confint(opv1_mean_delayed_24_35)

opv1_mean_early_12_23 <- svymean(~opv1_mean_early, DHSdesign.1, na.rm = T); opv1_mean_early_12_23; confint(opv1_mean_early_12_23)
opv1_mean_early_24_35 <- svymean(~opv1_mean_early, DHSdesign.2, na.rm = T); opv1_mean_early_24_35; confint(opv1_mean_early_24_35)

##opv2##
opv2_mean_delayed_12_23 <- svymean(~opv2_mean_delay, DHSdesign.1, na.rm = T); opv2_mean_delayed_12_23; confint(opv2_mean_delayed_12_23)
opv2_mean_delayed_24_35 <- svymean(~opv2_mean_delay, DHSdesign.2, na.rm = T); opv2_mean_delayed_24_35; confint(opv2_mean_delayed_24_35)

opv2_mean_early_12_23 <- svymean(~opv2_mean_early, DHSdesign.1, na.rm = T); opv2_mean_early_12_23; confint(opv2_mean_early_12_23)
opv2_mean_early_24_35 <- svymean(~opv2_mean_early, DHSdesign.2, na.rm = T); opv2_mean_early_24_35; confint(opv2_mean_early_24_35)

##opv3##
opv3_mean_delayed_12_23 <- svymean(~opv3_mean_delay, DHSdesign.1, na.rm = T); opv3_mean_delayed_12_23; confint(opv3_mean_delayed_12_23)
opv3_mean_delayed_24_35 <- svymean(~opv3_mean_delay, DHSdesign.2, na.rm = T); opv3_mean_delayed_24_35; confint(opv3_mean_delayed_24_35)

opv3_mean_early_12_23 <- svymean(~opv3_mean_early, DHSdesign.1, na.rm = T); opv3_mean_early_12_23; confint(opv3_mean_early_12_23)
opv3_mean_early_24_35 <- svymean(~opv3_mean_early, DHSdesign.2, na.rm = T); opv3_mean_early_24_35; confint(opv3_mean_early_24_35)

##penta1##
penta1_mean_delayed_12_23 <- svymean(~penta1_mean_delay, DHSdesign.1, na.rm = T); penta1_mean_delayed_12_23; confint(penta1_mean_delayed_12_23)
penta1_mean_delayed_24_35 <- svymean(~penta1_mean_delay, DHSdesign.2, na.rm = T); penta1_mean_delayed_24_35; confint(penta1_mean_delayed_24_35)

penta1_mean_early_12_23 <- svymean(~penta1_mean_early, DHSdesign.1, na.rm = T); penta1_mean_early_12_23; confint(penta1_mean_early_12_23)
penta1_mean_early_24_35 <- svymean(~penta1_mean_early, DHSdesign.2, na.rm = T); penta1_mean_early_24_35; confint(penta1_mean_early_24_35)

##penta2##
penta2_mean_delayed_12_23 <- svymean(~penta2_mean_delay, DHSdesign.1, na.rm = T); penta2_mean_delayed_12_23; confint(penta2_mean_delayed_12_23)
penta2_mean_delayed_24_35 <- svymean(~penta2_mean_delay, DHSdesign.2, na.rm = T); penta2_mean_delayed_24_35; confint(penta2_mean_delayed_24_35)

penta2_mean_early_12_23 <- svymean(~penta2_mean_early, DHSdesign.1, na.rm = T); penta2_mean_early_12_23; confint(penta2_mean_early_12_23)
penta2_mean_early_24_35 <- svymean(~penta2_mean_early, DHSdesign.2, na.rm = T); penta2_mean_early_24_35; confint(penta2_mean_early_24_35)

##penta3##
penta3_mean_delayed_12_23 <- svymean(~penta3_mean_delay, DHSdesign.1, na.rm = T); penta3_mean_delayed_12_23; confint(penta3_mean_delayed_12_23)
penta3_mean_delayed_24_35 <- svymean(~penta3_mean_delay, DHSdesign.2, na.rm = T); penta3_mean_delayed_24_35; confint(penta3_mean_delayed_24_35)

penta3_mean_early_12_23 <- svymean(~penta3_mean_early, DHSdesign.1, na.rm = T); penta3_mean_early_12_23; confint(penta3_mean_early_12_23)
penta3_mean_early_24_35 <- svymean(~penta3_mean_early, DHSdesign.2, na.rm = T); penta3_mean_early_24_35; confint(penta3_mean_early_24_35)

##mcv1##
mcv1_mean_delayed_12_23 <- svymean(~mcv1_mean_delay, DHSdesign.1, na.rm = T); mcv1_mean_delayed_12_23; confint(mcv1_mean_delayed_12_23)
mcv1_mean_delayed_24_35 <- svymean(~mcv1_mean_delay, DHSdesign.2, na.rm = T); mcv1_mean_delayed_24_35; confint(mcv1_mean_delayed_24_35)

mcv1_mean_early_12_23 <- svymean(~mcv1_mean_early, DHSdesign.1, na.rm = T); mcv1_mean_early_12_23; confint(mcv1_mean_early_12_23)
mcv1_mean_early_24_35 <- svymean(~mcv1_mean_early, DHSdesign.2, na.rm = T); mcv1_mean_early_24_35; confint(mcv1_mean_early_24_35)


##Calculate Regional-level (adm1) vaccination timeliness (continuous)###########################################################################
##hepB0##
R.hepB0_mean_delayed_12_23 <- svyby(~hepB0_mean_delay, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.hepB0_mean_delayed_12_23
R.hepB0_mean_delayed_24_35 <- svyby(~hepB0_mean_delay, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.hepB0_mean_delayed_24_35

##bcg##
R.bcg_mean_delayed_12_23 <- svyby(~bcg_mean_delay, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.bcg_mean_delayed_12_23
R.bcg_mean_delayed_24_35 <- svyby(~bcg_mean_delay, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.bcg_mean_delayed_24_35

##opv1##
R.opv1_mean_delayed_12_23 <- svyby(~opv1_mean_delay, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv1_mean_delayed_12_23
R.opv1_mean_delayed_24_35 <- svyby(~opv1_mean_delay, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv1_mean_delayed_24_35

R.opv1_mean_early_12_23 <- svyby(~opv1_mean_early, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv1_mean_early_12_23
R.opv1_mean_early_24_35 <- svyby(~opv1_mean_early, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv1_mean_early_24_35

##opv2##
R.opv2_mean_delayed_12_23 <- svyby(~opv2_mean_delay, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv2_mean_delayed_12_23
R.opv2_mean_delayed_24_35 <- svyby(~opv2_mean_delay, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv2_mean_delayed_24_35

R.opv2_mean_early_12_23 <- svyby(~opv2_mean_early, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv2_mean_early_12_23
R.opv2_mean_early_24_35 <- svyby(~opv2_mean_early, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv2_mean_early_24_35

##opv3##
R.opv3_mean_delayed_12_23 <- svyby(~opv3_mean_delay, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv3_mean_delayed_12_23
R.opv3_mean_delayed_24_35 <- svyby(~opv3_mean_delay, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv3_mean_delayed_24_35

R.opv3_mean_early_12_23 <- svyby(~opv3_mean_early, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv3_mean_early_12_23
R.opv3_mean_early_24_35 <- svyby(~opv3_mean_early, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.opv3_mean_early_24_35

##penta1##
R.penta1_mean_delayed_12_23 <- svyby(~penta1_mean_delay, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta1_mean_delayed_12_23
R.penta1_mean_delayed_24_35 <- svyby(~penta1_mean_delay, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta1_mean_delayed_24_35

R.penta1_mean_early_12_23 <- svyby(~penta1_mean_early, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta1_mean_early_12_23
R.penta1_mean_early_24_35 <- svyby(~penta1_mean_early, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta1_mean_early_24_35

##penta2##
R.penta2_mean_delayed_12_23 <- svyby(~penta2_mean_delay, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta2_mean_delayed_12_23
R.penta2_mean_delayed_24_35 <- svyby(~penta2_mean_delay, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta2_mean_delayed_24_35

R.penta2_mean_early_12_23 <- svyby(~penta2_mean_early, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta2_mean_early_12_23
R.penta2_mean_early_24_35 <- svyby(~penta2_mean_early, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta2_mean_early_24_35

##penta3##
R.penta3_mean_delayed_12_23 <- svyby(~penta3_mean_delay, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta3_mean_delayed_12_23
R.penta3_mean_delayed_24_35 <- svyby(~penta3_mean_delay, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta3_mean_delayed_24_35

R.penta3_mean_early_12_23 <- svyby(~penta3_mean_early, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta3_mean_early_12_23
R.penta3_mean_early_24_35 <- svyby(~penta3_mean_early, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.penta3_mean_early_24_35

##mcv1##
R.mcv1_mean_delayed_12_23 <- svyby(~mcv1_mean_delay, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.mcv1_mean_delayed_12_23
R.mcv1_mean_delayed_24_35 <- svyby(~mcv1_mean_delay, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.mcv1_mean_delayed_24_35

R.mcv1_mean_early_12_23 <- svyby(~mcv1_mean_early, ~region, DHSdesign.1, svymean, vartype = c("se","ci"), na.rm=TRUE); R.mcv1_mean_early_12_23
R.mcv1_mean_early_24_35 <- svyby(~mcv1_mean_early, ~region, DHSdesign.2, svymean, vartype = c("se","ci"), na.rm=TRUE); R.mcv1_mean_early_24_35

##write final dataset to file##
write.csv(data, "data.csv")



#############################################################################################################################################
#############################################################################################################################################
#CREATE DATA FRAME FOR CATEGORICAL TIMELINESS FIGURES

###EARLY_CATEGORICAL_NATIONAL

1 #combine outputs for 12-23 | 24-35

T_early_12_23 <- rbind(opv1_early_12_23, opv2_early_12_23, opv3_early_12_23, penta1_early_12_23, penta2_early_12_23, penta3_early_12_23, mcv1_early_12_23); T_early_12_23
T_early_24_35 <- rbind(opv1_early_24_35, opv2_early_24_35, opv3_early_24_35, penta1_early_24_35, penta2_early_24_35, penta3_early_24_35, mcv1_early_24_35); T_early_24_35

write.csv(T_early_12_23, "T_early_12_23.csv")
write.csv(T_early_24_35, "T_early_24_35.csv") #manually edit data to include headings and "age_group"

2. #read in both data above after the manual edit
T_early_12_23_edit	<- read.csv(paste0("T_early_12_23_edit.csv"), head = T)
T_early_24_35_edit	<- read.csv(paste0("T_early_24_35_edit.csv"), head = T)

3. #combine updated datasets above into same dataframe
T_early_categorical <- rbind(T_early_12_23_edit, T_early_24_35_edit); T_early_categorical

4. #convert outcome, CI_lower, and CI_upper to percentages by multiplying by 100
T_early_categorical <- T_early_categorical %>% 
  mutate(outcome = outcome*100, CI_lower = CI_lower*100, CI_upper = CI_upper*100); T_early_categorical

5. #write combine early_categorical timeliness data for file
write.csv(T_early_categorical, "T_early_categorical.csv")


###DELAYED_CATEGORICAL_NATIONAL

1 #combine outputs for 12-23 | 24-35

T_delayed_12_23 <- rbind(hepB0_delayed_12_23, bcg_delayed_12_23, opv1_delayed_12_23, opv2_delayed_12_23, opv3_delayed_12_23, penta1_delayed_12_23, penta2_delayed_12_23, penta3_delayed_12_23, mcv1_delayed_12_23); T_delayed_12_23
T_delayed_24_35 <- rbind(hepB0_delayed_24_35, bcg_delayed_24_35, opv1_delayed_24_35, opv2_delayed_24_35, opv3_delayed_24_35, penta1_delayed_24_35, penta2_delayed_24_35, penta3_delayed_24_35, mcv1_delayed_24_35); T_delayed_24_35

write.csv(T_delayed_12_23, "T_delayed_12_23.csv")
write.csv(T_delayed_24_35, "T_delayed_24_35.csv") #manually edit data to include headings and "age_group"

2. #read in both data above after the manual edit
T_delayed_12_23_edit	<- read.csv(paste0("T_delayed_12_23_edit.csv"), head = T)
T_delayed_24_35_edit	<- read.csv(paste0("T_delayed_24_35_edit.csv"), head = T)

3. #combine updated datasets above into same dataframe
T_delayed_categorical <- rbind(T_delayed_12_23_edit, T_delayed_24_35_edit); T_delayed_categorical

4. #convert outcome, CI_lower, and CI_upper to percentages by multiplying by 100
T_delayed_categorical <- T_delayed_categorical %>% 
  mutate(outcome = outcome*100, CI_lower = CI_lower*100, CI_upper = CI_upper*100); T_delayed_categorical

5. #write combine early_categorical timeliness data for file
write.csv(T_delayed_categorical, "T_delayed_categorical.csv")


###UNTIMELY_INTERVAL_CATEGORICAL_NATIONAL

1 #combine outputs for 12-23 | 24-35

T_untimely_unterval_12_23 <- rbind(p1_p2_untimely_interval_12_23, p2_p3_untimely_interval_12_23, op1_op2_untimely_interval_12_23, 
                                   op2_op3_untimely_interval_12_23); T_untimely_unterval_12_23
T_untimely_unterval_23_35 <- rbind(p1_p2_untimely_interval_24_35, p2_p3_untimely_interval_24_35, op1_op2_untimely_interval_24_35, 
                                   op2_op3_untimely_interval_24_35); T_untimely_unterval_23_35


T_interval_eary_delayed_12_23 <- rbind(p1_p2_interval_early_12_23, p1_p2_interval_delayed_12_23, p2_p3_interval_early_12_23, p2_p3_interval_delayed_12_23,
                                       op1_op2_interval_early_12_23, op1_op2_interval_delayed_12_23, op2_op3_interval_early_12_23, op2_op3_interval_delayed_12_23);
                                      T_interval_eary_delayed_12_23

T_interval_eary_delayed_24_35 <- rbind(p1_p2_interval_early_24_35, p1_p2_interval_delayed_24_35, p2_p3_interval_early_24_35, p2_p3_interval_delayed_24_35,
                                       op1_op2_interval_early_24_35, op1_op2_interval_delayed_24_35, op2_op3_interval_early_24_35, op2_op3_interval_delayed_24_35);
                                      T_interval_eary_delayed_24_35



write.csv(T_untimely_unterval_12_23, "T_untimely_unterval_12_23.csv")
write.csv(T_untimely_unterval_23_35, "T_untimely_unterval_23_35.csv") #manually edit data to include headings and "age_group"

write.csv(T_interval_eary_delayed_12_23, "T_interval_eary_delayed_12_23.csv")
write.csv(T_interval_eary_delayed_24_35, "T_interval_eary_delayed_24_35.csv") #manually edit data to include headings and "age_group"

2. #read in both data above after the manual edit
T_untimely_unterval_12_23_edit	<- read.csv(paste0("T_untimely_unterval_12_23_edit.csv"), head = T)
T_untimely_unterval_23_35_edit	<- read.csv(paste0("T_untimely_unterval_23_35_edit.csv"), head = T)

T_interval_eary_delayed_12_23_edit	<- read.csv(paste0("T_interval_eary_delayed_12_23_edit.csv"), head = T)
T_interval_eary_delayed_24_35_edit	<- read.csv(paste0("T_interval_eary_delayed_24_35_edit.csv"), head = T)

3. #combine updated datasets above into same dataframe
T_untimely_interval_categorical <- rbind(T_untimely_unterval_12_23_edit, T_untimely_unterval_23_35_edit); T_untimely_interval_categorical
T_interval_eary_delayed_combined <- rbind(T_interval_eary_delayed_12_23_edit, T_interval_eary_delayed_24_35_edit); T_interval_eary_delayed_combined


4. #convert outcome, CI_lower, and CI_upper to percentages by multiplying by 100
T_untimely_interval_categorical <- T_untimely_interval_categorical %>% 
  mutate(outcome = outcome*100, CI_lower = CI_lower*100, CI_upper = CI_upper*100); T_untimely_interval_categorical

T_interval_eary_delayed_combined <- T_interval_eary_delayed_combined %>% 
  mutate(outcome = outcome*100, CI_lower = CI_lower*100, CI_upper = CI_upper*100); T_interval_eary_delayed_combined

5. #write combine early_categorical timeliness data for file
write.csv(T_untimely_interval_categorical, "T_untimely_interval_categorical.csv")
write.csv(T_interval_eary_delayed_combined, "T_interval_eary_delayed_combined.csv")


##############################################################################################################################################
#CREATE PLOTS FOR CATEGORICAL TIMELINESS USING POINT-RANGE IN GGPLOT2

#load packages#####################################################################################################################
library(ggplot2)
library(foreign)
library(survey)
library(plyr)
library(tidyverse)
library(sf)
library(rio)
library(magrittr)
library(ggmap)
library(hexbin)
library(ggthemes)
library(classInt)
library(viridis)
library(ggbreak)
library(patchwork)
library(reshape2)
library(MASS)

set.seed(500) 

##create plot comparing crude coverage with timely coverage (two groups 12-23 months and 24 -35 months)
##NOTE: dataframe for this was created manually by combining the direct survey estimates for crude and timely vaccination

#Read in file######################################################################################################################
fig1a 	<- read.csv(paste0("figure1_12_23.csv"), head = T) #12-23 months
fig1b 	<- read.csv(paste0("figure1_23_35.csv"), head = T) #24-35 months


#make plots one after the other and combine

f1 <- ggplot(fig1a, aes(x = group, y = value, fill = subgroup)) +
  geom_bar(stat = "identity", position = "dodge", color="black", width = 0.6, alpha=0.7)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.4, position=position_dodge(.6), alpha=0.9, size=0.4)+
  theme_bw()+
  scale_y_continuous(name = "Percentage of children vaccinated", breaks = seq(0,100,10))+
  scale_x_discrete(name = "Childhood vaccines", limits=c("HepB0","BCG","OPV1", "PENTA1","OPV2","PENTA2", "OPV3", "PENTA3", "MCV1"))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), legend.position = "none")+
  scale_fill_manual(values=c('grey','#30a0b4'))+
  ggtitle("12 - 23 Months")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)+
  annotate("text", x=8.7, y=95, label="WHO target")


f2 <- ggplot(fig1b, aes(x = group, y = value, fill = subgroup)) +
  geom_bar(stat = "identity", position = "dodge", color="black", width = 0.6, alpha=0.7)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.4, position=position_dodge(.6), alpha=0.9, size=0.4)+
  theme_bw()+
  scale_y_continuous(name = "Percentage of children vaccinated", breaks = seq(0,100,10))+
  scale_x_discrete(name = "Childhood vaccines", limits=c("HepB0","BCG","OPV1", "PENTA1","OPV2","PENTA2", "OPV3", "PENTA3", "MCV1"))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  scale_fill_manual(values=c('grey','#30a0b4'), name=NULL, labels=c("Crude coverage", "Timely coverage"))+
  ggtitle("24 - 35 Months")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)+
  annotate("text", x=8.7, y=95, label="WHO target")

#stitch plot together
(f1 | f2) + plot_layout(widths = c(1,1.1))

#save plot at 1200 x 700

###P1b_EARLY_CATEGORICAL_NATIONAL

ggplot(data=T_early_categorical, mapping=aes(x=Vaccine_early, colour=Vaccine_early))+
  geom_pointrange(aes(y=outcome, ymin=CI_lower, ymax=CI_upper), size=0.7, shape=15)+
  theme_bw()+
  facet_wrap(facets = vars(age_group))+
  scale_x_discrete(name = NULL, limits=c("opv1","opv2","opv3", "penta1","penta2","penta3", "mcv1"))+
  scale_y_continuous(name = "Proportion (%)", breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13))+
  scale_colour_viridis_d(limits=c("opv1","opv2","opv3", "penta1","penta2","penta3", "mcv1"))+
  labs(colour = "Vaccines")+
  expand_limits(y = 1)

###P1_EARLY_CATEGORICAL_NATIONAL

p1 <- ggplot(data=T_early_categorical, mapping=aes(x=Vaccine_early, fill=Vaccine_early))+
  geom_col(aes(y=outcome), size=0.5, width = 0.3, colour = "black", fill="#92d0bb", alpha=0.6)+
  geom_errorbar( aes(x=Vaccine_early, ymin=CI_lower, ymax=CI_upper), width=0.2, position=position_dodge(.6), alpha=0.9, size=0.3)+
  theme_bw()+
  facet_grid(age_group~.)+
  scale_x_discrete(name = NULL, limits=c("mcv1", "penta3", "opv3", "penta1", "opv1", "penta2", "opv2"))+
  scale_y_continuous(name = "Proportion of children with early vaccination (%)", breaks = seq(0,100, 10))+
  labs(fill= "Vaccines")+
  coord_cartesian(ylim = c(0, 100))+
  theme(legend.position = "none")+
  theme(strip.text.y = element_text(angle = 90), panel.grid.minor = element_blank(), panel.grid.major = element_blank())


###P3b_DELAYED_CATEGORICAL_NATIONAL

ggplot(data=T_delayed_categorical, mapping=aes(x=Vaccine.delayed, colour=Vaccine.delayed))+
  geom_pointrange(aes(y=outcome, ymin=CI_lower, ymax=CI_upper), size=0.7, shape=15)+
  theme_bw()+
  facet_wrap(facets = vars(age_group))+
  scale_x_discrete(name = NULL, limits=c("hepB0", "bcg", "opv1","opv2","opv3", "penta1","penta2","penta3", "mcv1"))+
  scale_y_continuous(name = "Proportion (%)", breaks = seq(10,90,5))+
  scale_colour_viridis_d(limits=c("hepB0", "bcg", "opv1","opv2","opv3", "penta1","penta2","penta3", "mcv1"))+
  labs(colour = "Vaccines")+
  expand_limits(y = 15)

###P3_DELAYED_CATEGORICAL_NATIONAL

p3 <- ggplot(data=T_delayed_categorical, mapping=aes(x=Vaccine.delayed, fill=Vaccine.delayed))+
  geom_col(aes(y=outcome), size=0.8, width = 0.5, colour = "#972638", fill="#e0a69f")+
  geom_errorbar( aes(x=Vaccine.delayed, ymin=CI_lower, ymax=CI_upper), width=0.2, position=position_dodge(.6), alpha=0.9, size=0.3)+
  theme_bw()+
  facet_grid(age_group~.)+
  scale_x_discrete(name = NULL, limits=c("opv1", "penta1", "opv2", "penta2", "mcv1", "opv3", "penta3", "bcg", "hepB0"))+
  scale_y_continuous(name = "Proportion of children with delayed vaccination (%)", breaks = seq(0,90,5))+
  theme(legend.position = "none")+
  expand_limits(y = 0)+
  theme(strip.text.y = element_text(angle = 90), panel.grid.minor = element_blank(), panel.grid.major = element_blank())


###P5_UNTIMELY_INTERVAL_CATEGORICAL_NATIONAL
#read in manually updated dataset (EDIT FILE WITH CORRECT NAMES BEFORE READING IN)
T_untimely_interval_categorical	<- read.csv(paste0("T_untimely_interval_categorical_edit.csv"), head = T)
T_interval_eary_delayed_combined	<- read.csv(paste0("T_interval_eary_delayed_combined.csv"), head = T)

#commence actual plot(non-disaggregated untimely)
ggplot(T_untimely_interval_categorical, aes(outcome, Intervals))+
  geom_col(aes(fill="tomato2"), width = 0.2, colour = "black")+
  scale_x_continuous(name = "Proportion with untimely interval between multi-dose vaccines (%)", breaks = seq(0,100,20))+
  scale_y_discrete(name = "Vaccination intervals", labels=c("penta1_penta2 interval", "penta2_penta3 interval", "opv1_opv2 interval","opv2_opv3 interval"))+
  facet_grid(age_group~.)+
  theme(legend.position = "none") + 
  expand_limits(x = c(0,100))+
  theme_bw()+
  theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())


#commence actual plot (disaggregated untimely)
ggplot(T_interval_eary_delayed_combined, aes(x = vaccine_interval, y = outcome, fill = category)) +
  geom_col(width = 0.3, colour = "black")+
  coord_flip() +
  scale_y_continuous(name = "Proportion with too early or too late intervals between multi-dose vaccines (%)", breaks = seq(0,100,5))+
  scale_x_discrete(name = "Vaccination intervals")+
  facet_grid(age_group~.)+
  expand_limits(y = c(0,100))+
  theme_bw()+
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  scale_fill_manual(values = c("#fbb4ae", "#b2e1d6"))

  
##save at 1200 x 600


##############################################################################################################################################
#CREATE PLOTS FOR CONTINUOUS TIMELINESS USING POINT-RANGE IN GGPLOT2

1. #Read in data. This dataset is reduced version of the output after the direct survey method of creating 0s and 1s was saved
FIGURES <- read.csv(paste0("obj_2_censored_final_direct survey_FIGURES.csv"), head = T)



2. #subset data to two age cohorts#################################################################################################### 
##(12-23 m)##
FIGURES_12_23 <- subset(FIGURES, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
FIGURES_24_35 <- subset(FIGURES, age_in_month > 23)



3. #create dataset with only early OR delayed in each category
##(12-23 m)##
FIGURES_12_23_early <- FIGURES_12_23 %>% 
  dplyr::select(age_in_month, opv1_mean_early, opv2_mean_early, opv3_mean_early, penta1_mean_early, penta2_mean_early, penta3_mean_early, mcv1_mean_early)

FIGURES_12_23_delayed <- FIGURES_12_23 %>% 
  dplyr::select(age_in_month, hepB0_mean_delay, bcg_mean_delay, opv1_mean_delay, opv2_mean_delay, opv3_mean_delay, penta1_mean_delay, 
         penta2_mean_delay, penta3_mean_delay, mcv1_mean_delay)


##(24-35 m)##
FIGURES_24_35_early <- FIGURES_24_35 %>% 
  dplyr::select(age_in_month, opv1_mean_early, opv2_mean_early, opv3_mean_early, penta1_mean_early, penta2_mean_early, penta3_mean_early, mcv1_mean_early)

FIGURES_24_35_delayed <- FIGURES_24_35 %>% 
  dplyr::select(age_in_month, hepB0_mean_delay, bcg_mean_delay, opv1_mean_delay, opv2_mean_delay, opv3_mean_delay, penta1_mean_delay, 
         penta2_mean_delay, penta3_mean_delay, mcv1_mean_delay)



4. #melt each dataset into the long form
##(12-23 m)##
FIGURES_12_23_early_melt <- melt(FIGURES_12_23_early, na.rm = FALSE, id = c("age_in_month"))

FIGURES_12_23_delayed_melt <- melt(FIGURES_12_23_delayed, na.rm = FALSE, id = c("age_in_month"))

##(24-35 m)##
FIGURES_24_35_early_melt <- melt(FIGURES_24_35_early, na.rm = FALSE, id = c("age_in_month"))

FIGURES_24_35_delayed_melt <- melt(FIGURES_24_35_delayed, na.rm = FALSE, id = c("age_in_month"))


5. #write melted data to file, edit the age groups (by changing individual age to 12_23 Months and 24_35 Months AND editing names of vaccines to actual names)
write.csv(FIGURES_12_23_early_melt, "FIGURES_12_23_early_melt.csv")
write.csv(FIGURES_12_23_delayed_melt, "FIGURES_12_23_delayed_melt.csv")
write.csv(FIGURES_24_35_early_melt, "FIGURES_24_35_early_melt.csv")
write.csv(FIGURES_24_35_delayed_melt, "FIGURES_24_35_delayed_melt.csv")


6. #read in both data above after the manual edit as suggested above
FIGURES_12_23_early_melt.1	<- read.csv(paste0("FIGURES_12_23_early_melt_edit.csv"), head = T)
FIGURES_24_35_early_melt.1	<- read.csv(paste0("FIGURES_24_35_early_melt_edit.csv"), head = T)
FIGURES_12_23_delayed_melt.2	<- read.csv(paste0("FIGURES_12_23_delayed_melt_edit.csv"), head = T)
FIGURES_24_35_delayed_melt.2	<- read.csv(paste0("FIGURES_24_35_delayed_melt_edit.csv"), head = T)

7. #combine updated datasets above into same dataframe
FIGURES_early_melt.1 <- rbind(FIGURES_12_23_early_melt.1, FIGURES_24_35_early_melt.1); FIGURES_early_melt.1
FIGURES_delayed_melt.2 <- rbind(FIGURES_12_23_delayed_melt.2, FIGURES_24_35_delayed_melt.2); FIGURES_delayed_melt.2



###################################################################

###P2_EARLY_CONTINUOUS_NATIONAL
#Commence plot (violin, jitters, facet)
p2 <- ggplot(data = FIGURES_early_melt.1, mapping = aes(x = variable, y = value, fill=NULL)) +
  geom_jitter(colour="black", width = 0.2, size=0.9, alpha = 0.7)+
  geom_violin(width=0.95, alpha = 0.2) +
  geom_boxplot(aes(fill=variable), colour="black", width = 0.3, alpha=0.01)+
  theme_bw() +
  facet_grid(age_in_month~.)+
  scale_x_discrete(name = NULL, limits=c("MCV1","PENTA3","OPV3", "PENTA1","OPV1","PENTA2", "OPV2"))+
  scale_y_continuous(name = "Number of days vaccinated too early outside accepted national window", breaks = seq(0,120,10))+
  scale_fill_brewer(palette = "YlGnBu")+
  coord_cartesian(ylim = c(0, 120))+
  theme(legend.position = "none")+
  stat_summary(fun.y = median, geom = "point", fill = "#30a0b4", shape = 23, size = 4, alpha=0.8)+
  theme(strip.text.y = element_text(angle = 90), panel.grid.minor = element_blank(), panel.grid.major = element_blank())


####get summary statistics (range, median, iqr) of continuous early vaccination###
summary_12_23 <- data.frame(unclass(summary(FIGURES_12_23_early)), check.names = FALSE, stringsAsFactors = FALSE)
summary_24_35 <- data.frame(unclass(summary(FIGURES_24_35_early)), check.names = FALSE, stringsAsFactors = FALSE)

#write dataframe to file
write.csv(summary_12_23, paste0("summary_12_23.csv"))
write.csv(summary_24_35, paste0("summary_24_35.csv"))


###P4_DELAYED_CONTINUOUS_NATIONAL
#Commence plot (violin, jitter, facet)
p4 <- ggplot(data = FIGURES_delayed_melt.2, mapping = aes(x = variable, y = value, fill=NULL)) + 
  geom_jitter(colour="#e0a69f", width = 0.4, size=0.9, alpha = 0.5)+
  geom_violin(width=1.3, alpha = 0.2) +
  geom_boxplot(fill="NA", colour="black", width = 0.3, outlier.shape = NA)+
  theme_bw() +
  facet_grid(age_in_month~.)+
  scale_x_discrete(name = NULL, limits=c("OPV1","PENTA1","OPV2", "PENTA2","MCV1","OPV3", "PENTA3", "BCG", "hepB0"))+
  scale_y_continuous(name = "Number of days delayed outside accepted national vaccination window", breaks = seq(0,300,50))+
  scale_fill_brewer(palette = "YlOrRd")+
  labs(fill= "Vaccines")+
  coord_cartesian(ylim = c(0, 300))+
  theme(legend.position = "none")+
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 23, size = 3)+
  theme(strip.text.y = element_text(angle = 90), panel.grid.minor = element_blank(), panel.grid.major = element_blank())


####get summary statistics (range, median, iqr) of continuous delayed vaccination###
summary_12_23_delayed <- data.frame(unclass(summary(FIGURES_12_23_delayed)), check.names = FALSE, stringsAsFactors = FALSE)
summary_24_35_delayed <- data.frame(unclass(summary(FIGURES_24_35_delayed)), check.names = FALSE, stringsAsFactors = FALSE)

#write dataframe to file
write.csv(summary_12_23_delayed, paste0("summary_12_23_delayed.csv"))
write.csv(summary_24_35_delayed, paste0("summary_24_35_delayed.csv"))


#stitch plots together
(p3)+plot_annotation(tag_levels = "A")+(p4)+plot_annotation(tag_levels = "A")+plot_layout(widths = c(1,2))

#stitch plots together
(p1)+plot_annotation(tag_levels = "A")+(p2)+plot_annotation(tag_levels = "A")+plot_layout(widths = c(1,2))

##width = 1200, height = 700


############################################################################################################################
############################################################################################################################

###MAKE TABLE CONTAINING INFORMATION ABOUT CRUDE COVERAGE AND TIMELY VACCINATION
##This table would cover two birth cohorts and provide confidence intervals

1. ##Aggregate crude vaccination coverage data
##12 - 23 Months
crude_coverage_12_23 <- rbind(bcg12_23, hepB012_23, opv1_12_23, opv2_12_23, opv3_12_23,
                              penta1_12_23, penta2_12_23, penta3_12_23, mcv1_12_23); crude_coverage_12_23

##24 - 35 Months
crude_coverage_24_35 <- rbind(bcg24_35, hepB024_35, opv1_24_35, opv2_24_35, opv3_24_35,
                              penta1_24_35, penta2_24_35, penta3_24_35, mcv1_24_35); crude_coverage_24_35



#write combine crude vaccination coverage data data file
write.csv(crude_coverage_12_23, "crude_coverage_12_23.csv")

write.csv(crude_coverage_24_35, "crude_coverage_24_35.csv")


2. ##Aggregate timely/age-appropriate vaccinations

Timely_coverage_12_23 <- rbind(bcg_timely_12_23, hepB0_timely_12_23, opv1_timely_12_23, opv2_timely_12_23, opv3_timely_12_23,
                               penta1_timely_12_23, penta2_timely_12_23, penta3_timely_12_23, mcv1_timely_12_23); Timely_coverage_12_23

##24 - 35 Months
Timely_coverage_24_35 <- rbind(bcg_timely_24_35, hepB0_timely_24_35, opv1_timely_24_35, opv2_timely_24_35, opv2_timely_24_35,
                               penta1_timely_24_35, penta2_timely_24_35, penta3_timely_24_35, mcv1_timely_24_35); Timely_coverage_24_35


#write combined Timely vaccination coverage data data file
write.csv(Timely_coverage_12_23, "Timely_coverage_12_23.csv")

write.csv(Timely_coverage_24_35, "Timely_coverage_24_35.csv")


3. ##Aggregate timely interval vaccination

Timely_interval_12_23 <- rbind(p1_p2_timely_12_23, p2_p3_timely_12_23, op1_op2_timely_12_23, op2_op3_timely_12_23); Timely_interval_12_23

Timely_interval_24_35 <- rbind(p1_p2_timely_24_35, p2_p3_timely_24_35, op1_op2_timely_24_35, op2_op3_timely_24_35); Timely_interval_24_35


#write combined Timely vaccination coverage data data file
write.csv(Timely_interval_12_23, "Timely_interval_12_23.csv")

write.csv(Timely_interval_24_35, "Timely_interval_24_35.csv")



########################################################################################################################
########################################################################################################################
##GENERATE CUMMULATIVE FREQUENCY CURVE OF CRUDE COVERAGE FOR THE NINE VACCINES

##Read in data
CumulCurv <- read_csv("obj_2_censored_final.csv")


1. ##HepB0
####selects the variables that we need####
CumulCurv_hepB0 <- CumulCurv %>% 
  dplyr::select(sex, age_in_month, age_in_days, hepB0_age_vac) %>% 
  drop_na() 

####create dataset with unique hepB0_age_vac values####
hepB0 <- plyr::count(CumulCurv_hepB0, "hepB0_age_vac") 

####compute the cumulative counts and percentages####
cumcount <- cumsum(hepB0$freq) 
cumpercent <- (cumcount/nrow(CumulCurv_hepB0))*100

####add the cummulative percent to the dataset with unique hepB0_age_vac values####
hepB0 <- cbind(hepB0, cumpercent)

####plot the cummulative frequency curve using ggplot2####
p1_hepB0 <- ggplot(data = hepB0, mapping = aes(x=hepB0_age_vac, y=cumpercent)) + 
  geom_step() +  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(name = "Age at vaccination (days)", breaks = seq(0,90,5)) + 
  scale_y_continuous(name = "Children vaccinated (%)", breaks = seq(0,100,10)) + 
  geom_vline(xintercept = 1.9, col = "green3", linetype="dashed", size=1) + 
  coord_cartesian(xlim = c(0, 90))+
  labs(title = "HepB0 vaccination")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)


2. ##bcg
####selects the variables that we need####
CumulCurv_bcg <- CumulCurv %>% 
  dplyr::select(sex, age_in_month, age_in_days, bcg_age_vac) %>% 
  drop_na() 

####create dataset with unique hepB0_age_vac values####
bcg <- plyr::count(CumulCurv_bcg, "bcg_age_vac") 

####compute the cumulative counts and percentages####
cumcount <- cumsum(bcg$freq) 
cumpercent <- (cumcount/nrow(CumulCurv_bcg))*100

####add the cummulative percent to the dataset with unique hepB0_age_vac values####
bcg <- cbind(bcg, cumpercent)

####plot the cummulative frequency curve using ggplot2####
p2_bcg <- ggplot(data = bcg, mapping = aes(x=bcg_age_vac, y=cumpercent)) + 
  geom_step() +  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(name = "Age at vaccination (days)", breaks = seq(0,90,5)) + 
  scale_y_continuous(name = "Children vaccinated (%)", breaks = seq(0,100,10)) + 
  geom_vline(xintercept = 7.9, col = "green3", linetype="dashed", size=1) + 
  coord_cartesian(xlim = c(0, 90))+
  labs(title = "BCG vaccination")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)


3. ##opv1
####selects the variables that we need####
CumulCurv_opv1 <- CumulCurv %>% 
  dplyr::select(sex, age_in_month, age_in_days, opv1_age_vac) %>% 
  drop_na() 

####create dataset with unique hepB0_age_vac values####
opv1 <- plyr::count(CumulCurv_opv1, "opv1_age_vac") 

####compute the cumulative counts and percentages####
cumcount <- cumsum(opv1$freq) 
cumpercent <- (cumcount/nrow(CumulCurv_opv1))*100

####add the cummulative percent to the dataset with unique hepB0_age_vac values####
opv1 <- cbind(opv1, cumpercent)

####plot the cummulative frequency curve using ggplot2####
p3_opv1 <- ggplot(data = opv1, mapping = aes(x=opv1_age_vac, y=cumpercent)) + 
  geom_step() +  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(name = "Age at vaccination (days)", breaks = seq(0,165,15)) + 
  scale_y_continuous(name = "Children vaccinated (%)", breaks = seq(0,100,10)) + 
  geom_vline(xintercept = c(61, 90), col = "green3", linetype="dashed", size=1) + 
  coord_cartesian(xlim = c(0, 165))+
  labs(title = "OPV1 vaccination")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)


4. ##penta1
####selects the variables that we need####
CumulCurv_penta1 <- CumulCurv %>% 
  dplyr::select(sex, age_in_month, age_in_days, penta1_age_vac) %>% 
  drop_na() 

####create dataset with unique hepB0_age_vac values####
penta1 <- plyr::count(CumulCurv_penta1, "penta1_age_vac") 

####compute the cumulative counts and percentages####
cumcount <- cumsum(penta1$freq) 
cumpercent <- (cumcount/nrow(CumulCurv_penta1))*100

####add the cummulative percent to the dataset with unique hepB0_age_vac values####
penta1 <- cbind(penta1, cumpercent)

####plot the cummulative frequency curve using ggplot2####
p4_penta1 <- ggplot(data = penta1, mapping = aes(x=penta1_age_vac, y=cumpercent)) + 
  geom_step() +  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(name = "Age at vaccination (days)", breaks = seq(0,165,15)) + 
  scale_y_continuous(name = "Children vaccinated (%)", breaks = seq(0,100,10)) + 
  geom_vline(xintercept = c(61, 90), col = "green3", linetype="dashed", size=1) + 
  coord_cartesian(xlim = c(0, 165))+
  labs(title = "PENTA1 vaccination")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)


5. ##opv2
####selects the variables that we need####
CumulCurv_opv2 <- CumulCurv %>% 
  dplyr::select(sex, age_in_month, age_in_days, opv2_age_vac) %>% 
  drop_na() 

####create dataset with unique hepB0_age_vac values####
opv2 <- plyr::count(CumulCurv_opv2, "opv2_age_vac") 

####compute the cumulative counts and percentages####
cumcount <- cumsum(opv2$freq) 
cumpercent <- (cumcount/nrow(CumulCurv_opv2))*100

####add the cummulative percent to the dataset with unique hepB0_age_vac values####
opv2 <- cbind(opv2, cumpercent)

####plot the cummulative frequency curve using ggplot2####
p5_opv2 <- ggplot(data = opv2, mapping = aes(x=opv2_age_vac, y=cumpercent)) + 
  geom_step() +  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(name = "Age at vaccination (days)", breaks = seq(0,300,30)) + 
  scale_y_continuous(name = "Children vaccinated (%)", breaks = seq(0,100,10)) + 
  geom_vline(xintercept = c(91, 120), col = "green3", linetype="dashed", size=1) + 
  coord_cartesian(xlim = c(0, 300))+
  labs(title = "OPV2 vaccination")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)


6. ##penta2
####selects the variables that we need####
CumulCurv_penta2 <- CumulCurv %>% 
  dplyr::select(sex, age_in_month, age_in_days, penta2_age_vac) %>% 
  drop_na() 

####create dataset with unique hepB0_age_vac values####
penta2 <- plyr::count(CumulCurv_penta2, "penta2_age_vac") 

####compute the cumulative counts and percentages####
cumcount <- cumsum(penta2$freq) 
cumpercent <- (cumcount/nrow(CumulCurv_penta2))*100

####add the cummulative percent to the dataset with unique hepB0_age_vac values####
penta2 <- cbind(penta2, cumpercent)

####plot the cummulative frequency curve using ggplot2####
p6_penta2 <- ggplot(data = penta2, mapping = aes(x=penta2_age_vac, y=cumpercent)) + 
  geom_step() +  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(name = "Age at vaccination (days)", breaks = seq(0,300,30)) + 
  scale_y_continuous(name = "Children vaccinated (%)", breaks = seq(0,100,10)) + 
  geom_vline(xintercept = c(91, 120), col = "green3", linetype="dashed", size=1) + 
  coord_cartesian(xlim = c(0, 300))+
  labs(title = "PENTA2 vaccination")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)


7. ##opv3
####selects the variables that we need####
CumulCurv_opv3 <- CumulCurv %>% 
  dplyr::select(sex, age_in_month, age_in_days, opv3_age_vac) %>% 
  drop_na() 

####create dataset with unique hepB0_age_vac values####
opv3 <- plyr::count(CumulCurv_opv3, "opv3_age_vac") 

####compute the cumulative counts and percentages####
cumcount <- cumsum(opv3$freq) 
cumpercent <- (cumcount/nrow(CumulCurv_opv3))*100

####add the cummulative percent to the dataset with unique hepB0_age_vac values####
opv3 <- cbind(opv3, cumpercent)

####plot the cummulative frequency curve using ggplot2####
p7_opv3 <- ggplot(data = opv3, mapping = aes(x=opv3_age_vac, y=cumpercent)) + 
  geom_step() +  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(name = "Age at vaccination (days)", breaks = seq(0,330,30)) + 
  scale_y_continuous(name = "Children vaccinated (%)", breaks = seq(0,100,10)) + 
  geom_vline(xintercept = c(121, 150), col = "green3", linetype="dashed", size=1) + 
  coord_cartesian(xlim = c(0, 330))+
  labs(title = "OPV3 vaccination")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)


8. ##penta3
####selects the variables that we need####
CumulCurv_penta3 <- CumulCurv %>% 
  dplyr::select(sex, age_in_month, age_in_days, penta3_age_vac) %>% 
  drop_na() 

####create dataset with unique hepB0_age_vac values####
penta3 <- plyr::count(CumulCurv_penta3, "penta3_age_vac") 

####compute the cumulative counts and percentages####
cumcount <- cumsum(penta3$freq) 
cumpercent <- (cumcount/nrow(CumulCurv_penta3))*100

####add the cummulative percent to the dataset with unique hepB0_age_vac values####
penta3 <- cbind(penta3, cumpercent)

####plot the cummulative frequency curve using ggplot2####
p8_penta3 <- ggplot(data = penta3, mapping = aes(x=penta3_age_vac, y=cumpercent)) + 
  geom_step() +  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(name = "Age at vaccination (days)", breaks = seq(0,330,30)) + 
  scale_y_continuous(name = "Children vaccinated (%)", breaks = seq(0,100,10)) + 
  geom_vline(xintercept = c(121, 150), col = "green3", linetype="dashed", size=1) + 
  coord_cartesian(xlim = c(0, 330))+
  labs(title = "PENTA3 vaccination")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)


9. ##mcv1
####selects the variables that we need####
CumulCurv_mcv1 <- CumulCurv %>% 
  dplyr::select(sex, age_in_month, age_in_days, mcv1_age_vac) %>% 
  drop_na() 

####create dataset with unique hepB0_age_vac values####
mcv1 <- plyr::count(CumulCurv_mcv1, "mcv1_age_vac") 

####compute the cumulative counts and percentages####
cumcount <- cumsum(mcv1$freq) 
cumpercent <- (cumcount/nrow(CumulCurv_mcv1))*100

####add the cummulative percent to the dataset with unique hepB0_age_vac values####
mcv1 <- cbind(mcv1, cumpercent)

####plot the cummulative frequency curve using ggplot2####
p9_mcv1 <- ggplot(data = mcv1, mapping = aes(x=mcv1_age_vac, y=cumpercent)) + 
  geom_step(size=1) +  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(name = "Age at vaccination (days)", breaks = seq(90,480,30)) + 
  scale_y_continuous(name = "Children vaccinated (%)", breaks = seq(0, 100,10)) + 
  geom_vline(xintercept = c(271, 300), col = "green3", linetype="dashed", size=1) + 
  coord_cartesian(xlim = c(90, 480))+
  labs(title = "MCV1 vaccination")+
  geom_hline(yintercept=90, linetype='dashed', col = 'red', size=0.72)


#stitch plots together
(p1_hepB0 + p2_bcg + p3_opv1)/(p4_penta1 + p5_opv2 + p6_penta2)/(p7_opv3 + p8_penta3 + p9_mcv1)

#save 1200 x 800


##############################################################################################################################################

##Summarize all vaccines to determine how many people had complete records (i.e. cards with age of vaccination) for timeliness
#run the codes below and copy the number from the "number of obs" per vaccine per age group


#1. measles with complete dataset used for timeliness
##(12-23 m)##
measles_12_23 <- subset(CumulCurv_mcv1, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
measles_24_35 <- subset(CumulCurv_mcv1, age_in_month > 23)


#2. hepb0 with complete dataset used for timeliness
##(12-23 m)##
hepatitis_12_23 <- subset(CumulCurv_hepB0, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
hepatitis_24_35 <- subset(CumulCurv_hepB0, age_in_month > 23)

#3. bcg with complete dataset used for timeliness
##(12-23 m)##
bcg1_12_23 <- subset(CumulCurv_bcg, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
bcg2_24_35 <- subset(CumulCurv_bcg, age_in_month > 23)

#4. polio1 with complete dataset used for timeliness
##(12-23 m)##
polio1_12_23 <- subset(CumulCurv_opv1, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
polio1_24_35 <- subset(CumulCurv_opv1, age_in_month > 23)

#5. polio2 with complete dataset used for timeliness
##(12-23 m)##
polio2_12_23 <- subset(CumulCurv_opv2, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
polio2_24_35 <- subset(CumulCurv_opv2, age_in_month > 23)

#6. polio3 with complete dataset used for timeliness
##(12-23 m)##
polio3_12_23 <- subset(CumulCurv_opv3, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
polio3_24_35 <- subset(CumulCurv_opv3, age_in_month > 23)


#7. pentavelent1 with complete dataset used for timeliness
##(12-23 m)##
pentavalent1_12_23 <- subset(CumulCurv_penta1, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
pentavalent1_24_35 <- subset(CumulCurv_penta1, age_in_month > 23)

#8. pentavalent2 with complete dataset used for timeliness
##(12-23 m)##
pentavalent2_12_23 <- subset(CumulCurv_penta2, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
pentavalent2_24_35 <- subset(CumulCurv_penta2, age_in_month > 23)

#9. polio3 with complete dataset used for timeliness
##(12-23 m)##
pentavalent3_12_23 <- subset(CumulCurv_penta3, age_in_month > 11 & age_in_month < 24)

##(24-35 m)##
pentavalent3_24_35 <- subset(CumulCurv_penta3, age_in_month > 23)
