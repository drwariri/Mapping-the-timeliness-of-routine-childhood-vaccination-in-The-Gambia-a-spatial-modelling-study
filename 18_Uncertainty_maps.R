##########Load packages################################
library(ggplot2)
library(dplyr)
library(rgdal)
library(sf)
library(ggsci)
library(wesanderson)

####set working directory##############################
setwd("filepath")


###create filepath to read in csv outputs from########
filePathData <- "filepath0"


####Read in all shape files##########################
adm2 <- st_read(paste0("gmb_admbnda_adm2_2022.shp"))
adm3 <- st_read(paste0("gmb_admbnda_adm3_2022.shp"))

#1. HepB0_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for HepB0
HepB0.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_HepB0_delayed.csv"), header=TRUE)
HepB0.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_HepB0_delayed.csv"), header=TRUE)

#compute the credible interval width (CI) 
HepB0.adm2$CI <- HepB0.adm2$X0.975quant - HepB0.adm2$X0.025quant
HepB0.adm3$CI <- HepB0.adm3$X0.975quant - HepB0.adm3$X0.025quant

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_HepB0.adm2 <- left_join(adm2, HepB0.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_HepB0.adm3 <- left_join(adm3, HepB0.adm3, by = "ID")

# Define the color scale you want to use, ranging from blue to red
pal <- wes_palette("Zissou1", 100, type = "continuous")

# Plot the chloropleth map
#adm2
ggplot() +
  geom_sf(data = joined_HepB0.adm2, aes(fill = CI), color = "white") +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Uncertainty estimates (95% credible interval width) at 2nd admin-level")

#adm3
ggplot() +
  geom_sf(data = joined_HepB0.adm3, aes(fill = CI), color = "white") +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Uncertainty estimates (95% credible interval width) at 3rd admin-level")

#Stitch maps and legend together using patchwork##################
uncert.hepb.adm2 / uncert.hepb.adm3 

#bivariate_hepb <- bi.hepb.adm2 + bi.hepb.adm3 + legend.hepb +  plot_layout(widths = c(5, 5, 1))

###save plot as png at 1000 x 550 using (uncertainty_delayed_hepb0)
#######################################################################################################################################################

#2. penta3_early##################################################################################################################################

#read in all data (adm2, adm2) for penta3
penta3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_penta3_early.csv"), header=TRUE)
penta3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_penta3_early.csv"), header=TRUE)

#compute the credible interval width (CI) 
penta3.adm2$CI <- penta3.adm2$X0.975quant - penta3.adm2$X0.025quant
penta3.adm3$CI <- penta3.adm3$X0.975quant - penta3.adm3$X0.025quant

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_penta3.adm2 <- left_join(adm2, penta3.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_penta3.adm3 <- left_join(adm3, penta3.adm3, by = "ID")

# Define the color scale you want to use, ranging from blue to red
pal <- wes_palette("Zissou1", 100, type = "continuous")

# Plot the chloropleth map
#adm2
ggplot() +
  geom_sf(data = joined_penta3.adm2, aes(fill = CI)) +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Uncertainty estimates (95% credible interval width) at 2nd admin-level")

#adm3
ggplot() +
  geom_sf(data = joined_penta3.adm3, aes(fill = CI)) +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Uncertainty estimates (95% credible interval width) at 3rd admin-level")

#Stitch maps and legend together using patchwork##################
uncert.penta3.adm2 / uncert.penta3.adm3 

#bivariate_hepb <- bi.hepb.adm2 + bi.hepb.adm3 + legend.hepb +  plot_layout(widths = c(5, 5, 1))

###save plot as png at 1000 x 550 using (uncertainty_early_penta3)
#######################################################################################################################################################

#3. penta3_delayed##################################################################################################################################

#read in all data (adm2, adm2) for penta3
penta3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_penta3_delayed.csv"), header=TRUE)
penta3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_penta3_delayed.csv"), header=TRUE)

#compute the credible interval width (CI) 
penta3.adm2$CI <- penta3.adm2$X0.975quant - penta3.adm2$X0.025quant
penta3.adm3$CI <- penta3.adm3$X0.975quant - penta3.adm3$X0.025quant

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_penta3.adm2 <- left_join(adm2, penta3.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_penta3.adm3 <- left_join(adm3, penta3.adm3, by = "ID")


# Define the color scale you want to use, ranging from blue to red
pal <- wes_palette("Zissou1", 100, type = "continuous")



# Plot the chloropleth map
#adm2
ggplot() +
  geom_sf(data = joined_penta3.adm2, aes(fill = CI), color="white") +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Uncertainty estimates (95% credible interval width) at 2nd admin-level")

#adm3
ggplot() +
  geom_sf(data = joined_penta3.adm3, aes(fill = CI), color="white") +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Uncertainty estimates (95% credible interval width) at 3rd admin-level")

#Stitch maps and legend together using patchwork##################
uncert.penta3.adm2 / uncert.penta3.adm3 

#bivariate_hepb <- bi.hepb.adm2 + bi.hepb.adm3 + legend.hepb +  plot_layout(widths = c(5, 5, 1))

###save plot as png at 1000 x 400 using (uncertainty_delayed_penta3)
#######################################################################################################################################################

#4. mcv1_early##################################################################################################################################

#read in all data (adm2, adm2) for mcv1
mcv1.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_mcv1_early.csv"), header=TRUE)
mcv1.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_mcv1_early.csv"), header=TRUE)

#compute the credible interval width (CI) 
mcv1.adm2$CI <- mcv1.adm2$X0.975quant - mcv1.adm2$X0.025quant
mcv1.adm3$CI <- mcv1.adm3$X0.975quant - mcv1.adm3$X0.025quant

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_mcv1.adm2 <- left_join(adm2, mcv1.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_mcv1.adm3 <- left_join(adm3, mcv1.adm3, by = "ID")


# Define the color scale you want to use, ranging from blue to red
pal <- wes_palette("Zissou1", 100, type = "continuous")



# Plot the chloropleth map
#adm2
ggplot() +
  geom_sf(data = joined_mcv1.adm2, aes(fill = CI)) +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Uncertainty estimates (95% credible interval width) at 2nd admin-level")

#adm3
ggplot() +
  geom_sf(data = joined_mcv1.adm3, aes(fill = CI)) +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Uncertainty estimates (95% credible interval width) at 3rd admin-level")

#Stitch maps and legend together using patchwork##################
uncert.mcv1.adm2 / uncert.mcv1.adm3 
#bivariate_hepb <- bi.hepb.adm2 + bi.hepb.adm3 + legend.hepb +  plot_layout(widths = c(5, 5, 1))

###save plot as png at 1000 x 400 using (uncertainty_early_mcv1)
#######################################################################################################################################################

#5. mcv1_delayed##################################################################################################################################

#read in all data (adm2, adm2) for mcv1
mcv1.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_mcv1_delayed.csv"), header=TRUE)
mcv1.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_mcv1_delayed.csv"), header=TRUE)

#compute the credible interval width (CI) 
mcv1.adm2$CI <- mcv1.adm2$X0.975quant - mcv1.adm2$X0.025quant
mcv1.adm3$CI <- mcv1.adm3$X0.975quant - mcv1.adm3$X0.025quant

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_mcv1.adm2 <- left_join(adm2, mcv1.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_mcv1.adm3 <- left_join(adm3, mcv1.adm3, by = "ID")


# Define the color scale you want to use, ranging from blue to red
pal <- wes_palette("Zissou1", 100, type = "continuous")



# Plot the chloropleth map
#adm2
ggplot() +
  geom_sf(data = joined_mcv1.adm2, aes(fill = CI), color="white") +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Uncertainty estimates (95% credible interval width) at 2nd admin-level")

#adm3
ggplot() +
  geom_sf(data = joined_mcv1.adm3, aes(fill = CI), color="white") +
  scale_fill_gradientn(colors = pal, na.value = "grey90", name="Width (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Uncertainty estimates (95% credible interval width) at 3rd admin-level")

#Stitch maps and legend together using patchwork##################
uncert.mcv1.adm2 / uncert.mcv1.adm3 
#bivariate_hepb <- bi.hepb.adm2 + bi.hepb.adm3 + legend.hepb +  plot_layout(widths = c(5, 5, 1))

###save plot as png at 1000 x 400 using (uncertainty_delayed_mcv1)

#######################################################################################################################################################
######################################################################################################################################################












