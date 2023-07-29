##########Load packages################################
library(ggplot2)
library(tidyr)
library(dplyr)
library(rgdal)
library(sf)
library(ggsci)
library(ggmagnify)
library(ggrepel)

####set working directory##############################
setwd("filepath")


###create filepath to read in csv outputs from########
filePathData <- "filepath0"


####Read in all shape files##########################
adm1 <- st_read(paste0("gmb_admbnda_adm1_2022.shp"))
adm2 <- st_read(paste0("gmb_admbnda_adm2_2022.shp"))
adm3 <- st_read(paste0("gmb_admbnda_adm3_2022.shp"))


#1. HepB0_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for HepB0
HepB0.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_HepB0_delayed.csv"), header=TRUE)
HepB0.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_HepB0_delayed.csv"), header=TRUE)
HepB0.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_HepB0_delayed.csv"), header=TRUE)
clinics <- read.csv(paste0("Gambia_Immunization clinics_2019_updated_2023.csv"), header=TRUE)

############################
# Convert both shapefiles to sf objects with the same geometry column name
gmb_shp_1_sf <- st_as_sf(adm1, "newname")
gmb_shp_sf <- st_as_sf(adm3, "newname")

# Combine shapefiles
gmb_combined <- bind_rows(gmb_shp_1_sf, gmb_shp_sf)

# Create a variable to distinguish between admin 1 and admin 2 boundaries
gmb_combined$type <- ifelse(is.na(gmb_combined$ADM2_EN), "ADM1_EN", "ADM3_EN")
##################


# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_HepB0.adm1 <- left_join(adm1, HepB0.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_HepB0.adm2 <- left_join(adm2, HepB0.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_HepB0.adm3 <- left_join(adm3, HepB0.adm3, by = "ID")

##write adm2/adm3 files as csv for writing results
joined_HepB0.adm3_nogeom <- st_drop_geometry(joined_HepB0.adm3)
write.csv(joined_HepB0.adm3_nogeom, paste0("HepB0.delayed.adm3.csv"))



# Plot the chloropleth map
#adm1
ggplot(joined_HepB0.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="HepB0")

#adm2
hepB_adm2 <- ggplot(joined_HepB0.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="Delayed (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Modelled delayed HepB0 at 2nd admin-level")

# Zoom in on a specific region of the map
hepB_adm2 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_hepB0_adm2 (750 x 450)


#adm3
hepB_adm3 <- ggplot(joined_HepB0.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="Delayed (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Modelled delayed HepB0 at 3rd admin-level")

#adm3_90 or more
hepB_adm3_90 <- ggplot(joined_HepB0.adm3) +
  geom_sf(aes(fill = ifelse(mean >= 90, mean, NA)), color = "white") +
  scale_fill_viridis_c(option = "inferno", direction = -1, name = "Delayed (%)") +
  theme_bw() +
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Modelled delayed HepB0 at 3rd admin-level where delay is 90% or more")


# Zoom in on a specific region of the map
hepB_adm3 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))
hepB_adm3_90 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_hepB0_adm3 (750 x 450)

###save plot at 1000 by 400 (delayed_HepB0.adm1, delayed_HepB0.adm2, and delayed_HepB0.adm3)
#######################################################################################################################################################


#2. bcg_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for bcg
bcg.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_bcg_delayed.csv"), header=TRUE)
bcg.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_bcg_delayed.csv"), header=TRUE)
bcg.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_bcg_delayed.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_bcg.adm1 <- left_join(adm1, bcg.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_bcg.adm2 <- left_join(adm2, bcg.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_bcg.adm3 <- left_join(adm3, bcg.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_bcg.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed BCG")

#adm2
ggplot(joined_bcg.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed BCG")

#adm3
ggplot(joined_bcg.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed BCG")

###save plot at 1000 by 400 (delayed_bcg.adm1, delayed_bcg.adm2, and delayed_bcg.adm3)
#######################################################################################################################################################

#3. PENTA1_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for penta1
penta1.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_penta1_delayed.csv"), header=TRUE)
penta1.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_penta1_delayed.csv"), header=TRUE)
penta1.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_penta1_delayed.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_penta1.adm1 <- left_join(adm1, penta1.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_penta1.adm2 <- left_join(adm2, penta1.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_penta1.adm3 <- left_join(adm3, penta1.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_penta1.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Penta1")

#adm2
ggplot(joined_penta1.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed Penta1")

#adm3
ggplot(joined_penta1.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed Penta1")

###save plot at 1000 by 400 (delayed_penta1.adm1, delayed_penta1.adm2, and delayed_penta1.adm3)
#####################################################################################################################################################


#4. PENTA2_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for penta1
penta2.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_penta2_delayed.csv"), header=TRUE)
penta2.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_penta2_delayed.csv"), header=TRUE)
penta2.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_penta2_delayed.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_penta2.adm1 <- left_join(adm1, penta2.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_penta2.adm2 <- left_join(adm2, penta2.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_penta2.adm3 <- left_join(adm3, penta2.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_penta2.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Penta2")

#adm2
ggplot(joined_penta2.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed Penta2")

#adm3
ggplot(joined_penta2.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed Penta2")

###save plot at 1000 by 400 (delayed_penta2.adm1, delayed_penta2.adm2, and delayed_penta2.adm3)
#####################################################################################################################################################


#5. PENTA3_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for penta1
penta3.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_penta3_delayed.csv"), header=TRUE)
penta3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_penta3_delayed.csv"), header=TRUE)
penta3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_penta3_delayed.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_penta3.adm1 <- left_join(adm1, penta3.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_penta3.adm2 <- left_join(adm2, penta3.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_penta3.adm3 <- left_join(adm3, penta3.adm3, by = "ID")

##write adm2/adm3 files as csv for writing results
joined_penta3.adm2_nogeom <- st_drop_geometry(joined_penta3.adm2)
joined_penta3.adm3_nogeom <- st_drop_geometry(joined_penta3.adm3)

write.csv(joined_penta3.adm2_nogeom, paste0("p3.delayed.adm2.csv"))
write.csv(joined_penta3.adm3_nogeom, paste0("p3.delayed.adm3.csv"))


# Plot the chloropleth map
#adm1
ggplot(joined_penta3.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Penta3")

#adm2
p3_adm2 <- ggplot(joined_penta3.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Modelled delayed PENTA3 at 2nd admin-level")

# Zoom in on a specific region of the map
p3_adm2 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_p3_adm2 (650 x 400)

#adm3
p3_adm3 <- ggplot(joined_penta3.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Modelled delayed PENTA3 at 3rd admin-level")

#adm3_45 or more
p3_adm3_45 <- ggplot(joined_penta3.adm3) +
  geom_sf(aes(fill = ifelse(mean >= 45, mean, NA)), color = "white") +
  scale_fill_viridis_c(option = "inferno", direction = -1, name = "Delayed (%)", limits = c(45, NA)) +
  theme_bw() +
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Modelled delayed PENTA3 at 3rd admin-level where delay is 45% or more")

# Zoom in on a specific region of the map
p3_adm3 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))
p3_adm3_45 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_p3_adm3 (650 x 400)

###save plot at 1000 by 400 (delayed_penta3.adm1, delayed_penta3.adm2, and delayed_penta3.adm3)
####################################################################################################################################################


#6. opv1_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for opv1
opv1.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_opv1_delayed.csv"), header=TRUE)
opv1.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_opv1_delayed.csv"), header=TRUE)
opv1.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_opv1_delayed.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_opv1.adm1 <- left_join(adm1, opv1.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_opv1.adm2 <- left_join(adm2, opv1.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_opv1.adm3 <- left_join(adm3, opv1.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_opv1.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed OPV1")

#adm2
ggplot(joined_opv1.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed OPV1")

#adm3
ggplot(joined_opv1.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed OPV1")

###save plot at 1000 by 400 (delayed_opv1.adm1, delayed_opv1.adm2, and delayed_opv1.adm3)
#####################################################################################################################################################


#7. OPV2_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for opv2
opv2.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_opv2_delayed.csv"), header=TRUE)
opv2.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_opv2_delayed.csv"), header=TRUE)
opv2.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_opv2_delayed.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_opv2.adm1 <- left_join(adm1, opv2.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_opv2.adm2 <- left_join(adm2, opv2.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_opv2.adm3 <- left_join(adm3, opv2.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_opv2.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed OPV2")

#adm2
ggplot(joined_opv2.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed OPV2")

#adm3
ggplot(joined_opv2.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed OPV2")

###save plot at 1000 by 400 (delayed_opv2.adm1, delayed_opv2.adm2, and delayed_opv2.adm3)
#####################################################################################################################################################


#8. opv3_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for opv3
opv3.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_opv3_delayed.csv"), header=TRUE)
opv3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_opv3_delayed.csv"), header=TRUE)
opv3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_opv3_delayed.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_opv3.adm1 <- left_join(adm1, opv3.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_opv3.adm2 <- left_join(adm2, opv3.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_opv3.adm3 <- left_join(adm3, opv3.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_opv3.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed OPV3")

#adm2
ggplot(joined_opv3.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed OPV3")

#adm3
ggplot(joined_opv3.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Delayed OPV3")

###save plot at 1000 by 400 (delayed_opv3.adm1, delayed_opv3.adm2, and delayed_opv3.adm3)
####################################################################################################################################################

#9. MCV1_delayed##################################################################################################################################

#read in all data (adm1, adm2, adm2) for mcv1
mcv1.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_mcv1_delayed.csv"), header=TRUE)
mcv1.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_mcv1_delayed.csv"), header=TRUE)
mcv1.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_mcv1_delayed.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_mcv1.adm1 <- left_join(adm1, mcv1.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_mcv1.adm2 <- left_join(adm2, mcv1.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_mcv1.adm3 <- left_join(adm3, mcv1.adm3, by = "ID")

##write adm2/adm3 files as csv for writing results
joined_mcv1.adm2_nogeom <- st_drop_geometry(joined_mcv1.adm2)
joined_mcv1.adm3_nogeom <- st_drop_geometry(joined_mcv1.adm3)

write.csv(joined_mcv1.adm2_nogeom, paste0("mcv1.delayed.adm2.csv"))
write.csv(joined_mcv1.adm3_nogeom, paste0("mcv1.delayed.adm3.csv"))


# Plot the chloropleth map
#adm1
ggplot(joined_mcv1.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="MCV1")

#adm2
mcv1_adm2 <- ggplot(joined_mcv1.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="Delayed (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Modelled delayed MCV1 at 2nd admin-level")

# Zoom in on a specific region (Banjul) of the map
mcv1_adm2 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_mcv1_adm2 (650 x 400)

#adm3
mcv1_adm3 <- ggplot(joined_mcv1.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="Delayed(%)")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Modelled delayed MCV1 at 3rd admin-level")

#adm3_35 or more
p3_adm3_35 <- ggplot(joined_mcv1.adm3) +
  geom_sf(aes(fill = ifelse(mean >= 35, mean, NA)), color = "white") +
  scale_fill_viridis_c(option = "inferno", direction = -1, name = "Delayed (%)", limits = c(35, NA)) +
  theme_bw() +
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Modelled delayed MCV1 at 3rd admin-level where delay is 35% or more")

# Zoom in on a specific region of the map
mcv1_adm3 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))
p3_adm3_35 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_mcv1_adm3 (650 x 400)

###save plot at 1000 by 400 (delayed_mcv1.adm1, delayed_mcv1.adm2, and delayed_mcv1.adm3)
#####################################################################################################################################################


#10. op1op2_untimely_interval##################################################################################################################################

#read in all data (adm1, adm2, adm2) for mcv1
op1op2.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_op1op2_untimely.csv"), header=TRUE)
op1op2.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_op1op2_untimely.csv"), header=TRUE)
op1op2.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_op1op2_untimely.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_op1op2.adm1 <- left_join(adm1, op1op2.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_op1op2.adm2 <- left_join(adm2, op1op2.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_op1op2.adm3 <- left_join(adm3, op1op2.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_op1op2.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely OPV1-OPV2 interval2")

#adm2
ggplot(joined_op1op2.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely OPV1-OPV2 interval")

#adm3
ggplot(joined_op1op2.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely OPV1-OPV2 interval")

###save plot at 1000 by 400 (untimely_op1op2.adm1, untimely_op1op2.adm2, and untimely_op1op2.adm3)
#####################################################################################################################################################


#11. op2op3_untimely_interval##################################################################################################################################

#read in all data (adm1, adm2, adm2) for mcv1
op2op3.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_op2op3_untimely.csv"), header=TRUE)
op2op3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_op2op3_untimely.csv"), header=TRUE)
op2op3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_op2op3_untimely.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_op2op3.adm1 <- left_join(adm1, op2op3.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_op2op3.adm2 <- left_join(adm2, op2op3.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_op2op3.adm3 <- left_join(adm3, op2op3.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_op2op3.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely OPV2-OPV3 interval2")

#adm2
ggplot(joined_op2op3.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely OPV2-OPV3 interval")

#adm3
ggplot(joined_op2op3.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely OPV2-OPV3 interval")

###save plot at 1000 by 400 (untimely_op2op3.adm1, untimely_op2op3.adm2, and untimely_op2op3.adm3)
#####################################################################################################################################################

#12. p1p2_untimely_interval##################################################################################################################################

#read in all data (adm1, adm2, adm2) for p1p2
p1p2.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_p1p2_untimely.csv"), header=TRUE)
p1p2.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_p1p2_untimely.csv"), header=TRUE)
p1p2.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_p1p2_untimely.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_p1p2.adm1 <- left_join(adm1, p1p2.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_p1p2.adm2 <- left_join(adm2, p1p2.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_p1p2.adm3 <- left_join(adm3, p1p2.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_p1p2.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely Penta1-Penta2 interval")

#adm2
ggplot(joined_p1p2.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely Penta1-Penta2 interval")

#adm3
ggplot(joined_p1p2.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely Penta1-Penta2 interval")

###save plot at 1000 by 400 (untimely_p1p2.adm1, untimely_p1p2.adm2, and untimely_p1p2.adm3)
#####################################################################################################################################################


#13. p2p3_untimely_interval##################################################################################################################################

#read in all data (adm1, adm2, adm2) for p2p3
p2p3.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_p2p3_untimely.csv"), header=TRUE)
p2p3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_p2p3_untimely.csv"), header=TRUE)
p2p3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_p2p3_untimely.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_p2p3.adm1 <- left_join(adm1, p2p3.adm1, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_p2p3.adm2 <- left_join(adm2, p2p3.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_p2p3.adm3 <- left_join(adm3, p2p3.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_p2p3.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely Penta2-Penta3 interval")

#adm2
ggplot(joined_p2p3.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely Penta2-Penta3 interval")

#adm3
ggplot(joined_p2p3.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(option = "inferno", direction = -1, name="% Delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Untimely OPV2-OPV3 interval")

###save plot at 1000 by 400 (untimely_p2p3.adm1, untimely_p2p3.adm2, and untimely_p2p3.adm3)
#####################################################################################################################################################


####create cross validation map##########################################################################################################

#1. hepB0
#read in all data
hepB0_validation <- read.csv(paste0("cross_validation_hepB0.csv"), header=TRUE)

# create scatterplot with a 45-degree line of best fit using linear regression
ggplot(hepB0_validation, aes(x = direct_survey, y = mean_modelled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_bw()+
  labs(title="Delayed HepB0 (modelled vs direct estimates)")+
  scale_y_continuous(name = "Modelled Estimates (ADM1)", breaks = seq(80,95,1))+ 
  scale_x_continuous(name = "Design-based direct survey estimates (ADM1)", breaks = seq(80,95,1))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

###save as 450x 600 (hepB0_cross_validation)
####################################################
#2. mcv1
#read in all data
mcv1_validation <- read.csv(paste0("cross_validation.csv"), header=TRUE)

# create scatterplot with a 45-degree line of best fit using linear regression
ggplot(mcv1_validation, aes(x = direct_survey, y = mean_modelled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_bw()+
  labs(title="Delayed MCV1 (modelled vs direct estimates)")+
  scale_y_continuous(name = "Modelled Estimates (ADM1)", breaks = seq(20,39,1))+ 
  scale_x_continuous(name = "Design-based direct survey estimates (ADM1)", breaks = seq(20,39,1))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

###save as 450x 600 (mcv1_cross_validation)
####################################################

#3. delayed PENTA3
#read in all data 
p3_validation <- read.csv(paste0("p3_cross_validation.csv"), header=TRUE)

# create scatterplot with a 45-degree line of best fit using linear regression
ggplot(p3_validation, aes(x = direct_survey, y = mean_modelled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_bw()+
  labs(title="Delayed PENTA3 (modelled vs direct estimates)")+
  scale_y_continuous(name = "Modelled Estimates (ADM1)", breaks = seq(30,52,1))+ 
  scale_x_continuous(name = "Design-based direct survey estimates (ADM1)", breaks = seq(30,52,1))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())


###save as 450x 600 (penta3_cross_validation)
###################################################################################################################################################


##############create heatmap for vaccines showing categories#################################################################################################

####Make heatmap showing 49 wards and their hepb0, penta3, and mcv1 categories.

#read in data "heatmap_all_adm2"
heatmap <- read.csv(paste0( "heatmap_all_adm2.csv"), header=TRUE)

# Calculate tertiles
tertiles_h <- quantile(heatmap$hepb0_dd, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
tertiles_p <- quantile(heatmap$penta3_dd, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
tertiles_m <- quantile(heatmap$mcv1_dd, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# Add new categories (tertiles) t the dataset as new varibales
heatmap$HepB0 <- cut(heatmap$hepb0_dd, breaks = tertiles_h, labels = c("Low", "Medium", "High"), include.lowest = TRUE)
heatmap$PENTA3 <- cut(heatmap$penta3_dd, breaks = tertiles_p, labels = c("Low", "Medium", "High"), include.lowest = TRUE)
heatmap$MCV1 <- cut(heatmap$mcv1_dd, breaks = tertiles_m, labels = c("Low", "Medium", "High"), include.lowest = TRUE)


# Reshape the data into the long form needed to make the plots
heatmap_reshaped <- heatmap %>%
  pivot_longer(cols = c(HepB0, PENTA3, MCV1), names_to = "Variable", values_to = "Value")

# Define the color palette
color_palette <- c("#FFCCCB", "#FF0000", "#800000")  # Three shades of red

# Convert Value to a factor variable so it can treat is as a discrete variable
heatmap_reshaped$Value <- factor(heatmap_reshaped$Value)

# Convert the ward variable to a factor so that they can appear with the desired order in the dataframe when plotted
heatmap_reshaped$ward <- factor(heatmap_reshaped$ward, levels = unique(heatmap_reshaped$ward))
heatmap_reshaped$Variable <- factor(heatmap_reshaped$Variable, levels = unique(heatmap_reshaped$Variable))

# Create the heatmap
##without LGA
ggplot(heatmap_reshaped, aes(x = ward, y = Variable, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = color_palette, guide = guide_legend(title = "Category (tertile) of delayed vaccination"),
                    breaks = c("Low", "Medium", "High"), labels = c("Low", "Medium", "High")) +
  labs(x = "Second administrative levels (Wards) in The Gambia", y = "Delayed vaccination") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        strip.text = element_text(size = 8, angle = 0, hjust = 0.5),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0))+
  geom_vline(xintercept = c(3.5, 8.5, 14.5, 20.5, 26.5, 33.5, 40.5),
             color = "white", linetype = "solid", size = 3.5)



##with LGA
# Create a new variable that combines the ward and lga names
heatmap_reshaped$lga_ward <- paste(heatmap_reshaped$ward, heatmap_reshaped$lga, sep = " - ")

# Convert the ward variable to a factor so that they can appear with the desired order in the dataframe when plotted
heatmap_reshaped$lga_ward <- factor(heatmap_reshaped$lga_ward, levels = unique(heatmap_reshaped$lga_ward))
heatmap_reshaped$Variable <- factor(heatmap_reshaped$Variable, levels = unique(heatmap_reshaped$Variable))

# Plot the heatmap
ggplot(heatmap_reshaped, aes(x = lga_ward, y = Variable, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = color_palette, guide = guide_legend(title = "Category (tertile) of delayed vaccination"),
                    breaks = c("Low", "Medium", "High"), labels = c("Low", "Medium", "High")) +
  labs(x = "Second administrative levels (Wards) and their corresponding Local Government Area in The Gambia", y = "Delayed vaccination") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5, color = "black"),
        strip.text = element_text(size = 8, angle = 0, hjust = 0.5),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0))+
  geom_vline(xintercept = c(3.5, 8.5, 14.5, 20.5, 26.5, 33.5, 40.5),
             color = "white", linetype = "solid", size = 3.5)

#####Save pdf, figure 6 with dimensions 9 inches by 5 inches 
##############################################################################################################################################









