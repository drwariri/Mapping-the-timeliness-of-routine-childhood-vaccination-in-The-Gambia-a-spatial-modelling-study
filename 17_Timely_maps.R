##########Load packages################################
library(ggplot2)
library(dplyr)
library(rgdal)
library(sf)
library("ggsci")

####set working directory##############################
setwd("filepath")


###create filepath to read in csv outputs from########
filePathData <- "filepath0"


####Read in all shape files##########################
adm1 <- st_read(paste0("gmb_admbnda_adm1_2022.shp"))
adm2 <- st_read(paste0("gmb_admbnda_adm2_2022.shp"))
adm3 <- st_read(paste0("gmb_admbnda_adm3_2022.shp"))


#1. HepB0_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for HepB0
HepB0.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_HepB0_timely.csv"), header=TRUE)
HepB0.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_HepB0_timely.csv"), header=TRUE)
HepB0.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_HepB0_timely.csv"), header=TRUE)

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


# Plot the chloropleth map
#adm1
ggplot(joined_HepB0.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely HepB0")

#adm2
ggplot(joined_HepB0.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely HepB0")

#adm3
ggplot(joined_HepB0.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely HepB0")
  
###save plot at 1000 by 400 (timely_HepB0.adm1, timely_HepB0.adm2, and timely_HepB0.adm3)

########################################################################################################################################################

#2. bcg_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for bcg
bcg.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_bcg_timely.csv"), header=TRUE)
bcg.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_bcg_timely.csv"), header=TRUE)
bcg.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_bcg_timely.csv"), header=TRUE)

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely BCG")

#adm2
ggplot(joined_bcg.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely BCG")

#adm3
ggplot(joined_bcg.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely BCG")

###save plot at 1000 by 400 (timely_bcg.adm1, timely_bcg.adm2, and timely_bcg.adm3)

########################################################################################################################################################

#3. PENTA1_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for penta1
penta1.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_penta1_timely.csv"), header=TRUE)
penta1.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_penta1_timely.csv"), header=TRUE)
penta1.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_penta1_timely.csv"), header=TRUE)

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta1")

#adm2
ggplot(joined_penta1.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta1")

#adm3
ggplot(joined_penta1.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta1")

###save plot at 1000 by 400 (timely_penta1.adm1, timely_penta1.adm2, and timely_penta1.adm3)

########################################################################################################################################################

#4. PENTA2_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for penta2
penta2.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_penta2_timely.csv"), header=TRUE)
penta2.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_penta2_timely.csv"), header=TRUE)
penta2.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_penta2_timely.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_penta2.adm1 <- left_join(adm1, penta2.adm2, by = "ID")

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta2")

#adm2
ggplot(joined_penta2.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta2")

#adm3
ggplot(joined_penta2.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta2")

###save plot at 1000 by 400 (timely_penta2.adm1, timely_penta2.adm2, and timely_penta2.adm3)

########################################################################################################################################################

#5. PENTA3_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for penta3
penta3.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_penta3_timely.csv"), header=TRUE)
penta3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_penta3_timely.csv"), header=TRUE)
penta3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_penta3_timely.csv"), header=TRUE)

# Join the data and each shape file
#adm1
ID = 1:nrow(adm1)
adm1$ID = ID
joined_penta3.adm1 <- left_join(adm1, penta3.adm2, by = "ID")

#adm2
ID = 1:nrow(adm2)
adm2$ID = ID
joined_penta3.adm2 <- left_join(adm2, penta3.adm2, by = "ID")

#adm3
ID = 1:nrow(adm3)
adm3$ID = ID
joined_penta3.adm3 <- left_join(adm3, penta3.adm3, by = "ID")


# Plot the chloropleth map
#adm1
ggplot(joined_penta3.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta3")

#adm2
ggplot(joined_penta3.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta3")

#adm3
ggplot(joined_penta3.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta3")

###save plot at 1000 by 400 (timely_penta3.adm1, timely_penta3.adm2, and timely_penta3.adm3)

########################################################################################################################################################

#6. opv1_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for opv1
opv1.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_opv1_timely.csv"), header=TRUE)
opv1.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_opv1_timely.csv"), header=TRUE)
opv1.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_opv1_timely.csv"), header=TRUE)

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV1")

#adm2
ggplot(joined_opv1.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV1")

#adm3
ggplot(joined_opv1.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV1")

###save plot at 1000 by 400 (timely_opv1.adm1, timely_opv1.adm2, and timely_opv1.adm3)

########################################################################################################################################################

#7. opv2_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for opv2
opv2.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_opv2_timely.csv"), header=TRUE)
opv2.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_opv2_timely.csv"), header=TRUE)
opv2.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_opv2_timely.csv"), header=TRUE)

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV2")

#adm2
ggplot(joined_opv2.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV2")

#adm3
ggplot(joined_opv2.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV2")

###save plot at 1000 by 400 (timely_opv2.adm1, timely_opv2.adm2, and timely_opv2.adm3)

########################################################################################################################################################

#8. opv3_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for opv3
opv3.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_opv3_timely.csv"), header=TRUE)
opv3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_opv3_timely.csv"), header=TRUE)
opv3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_opv3_timely.csv"), header=TRUE)

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV3")

#adm2
ggplot(joined_opv3.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV3")

#adm3
ggplot(joined_opv3.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV3")

###save plot at 1000 by 400 (timely_opv3.adm1, timely_opv3.adm2, and timely_opv3.adm3)

########################################################################################################################################################

#9. mcv1_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for mcv1
mcv1.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_mcv1_timely.csv"), header=TRUE)
mcv1.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_mcv1_timely.csv"), header=TRUE)
mcv1.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_mcv1_timely.csv"), header=TRUE)

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


# Plot the chloropleth map
#adm1
ggplot(joined_mcv1.adm1)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely MCV1")

#adm2
ggplot(joined_mcv1.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely MCV1")

#adm3
ggplot(joined_mcv1.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely MCV1")

###save plot at 1000 by 400 (timely_mcv1.adm1, timely_mcv1.adm2, and timely_mcv1.adm3)

########################################################################################################################################################


#10. op1op2_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for op1op2
op1op2.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_op1op2_timely.csv"), header=TRUE)
op1op2.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_op1op2_timely.csv"), header=TRUE)
op1op2.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_op1op2_timely.csv"), header=TRUE)

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV1-OPV2 interval")

#adm2
ggplot(joined_op1op2.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV1-OPV2 interval")

#adm3
ggplot(joined_op1op2.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV1-OPV2 interval")

###save plot at 1000 by 400 (timely_op1op2.adm1, timely_op1op2.adm2, and timely_op1op2.adm3)

########################################################################################################################################################


#11. op2op3_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for op2op3
op2op3.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_op2op3_timely.csv"), header=TRUE)
op2op3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_op2op3_timely.csv"), header=TRUE)
op2op3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_op2op3_timely.csv"), header=TRUE)

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV2-OPV3 interval")

#adm2
ggplot(joined_op2op3.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV2-OPV3 interval")

#adm3
ggplot(joined_op2op3.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely OPV2-OPV3 interval")

###save plot at 1000 by 400 (timely_op2op3.adm1, timely_op2op3.adm2, and timely_op2op3.adm3)

########################################################################################################################################################

#12. p1p2_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for p1p2
p1p2.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_p1p2_timely.csv"), header=TRUE)
p1p2.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_p1p2_timely.csv"), header=TRUE)
p1p2.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_p1p2_timely.csv"), header=TRUE)

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta1-Penta2 interval")

#adm2
ggplot(joined_p1p2.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta1-Penta2 interval")

#adm3
ggplot(joined_p1p2.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta1-Penta2 interval")

###save plot at 1000 by 400 (timely_p1p2.adm1, timely_p1p2.adm2, and timely_p1p2.adm3)

########################################################################################################################################################

#13. p2p3_timely##################################################################################################################################

#read in all data (adm1, adm2, adm2) for p2p3
p2p3.adm1 <- read.csv(paste0(filePathData, "adm1_estimates_p2p3_timely.csv"), header=TRUE)
p2p3.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_p2p3_timely.csv"), header=TRUE)
p2p3.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_p2p3_timely.csv"), header=TRUE)

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
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta2-Penta3 interval")

#adm2
ggplot(joined_p2p3.adm2)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta2-Penta3 interval")

#adm3
ggplot(joined_p2p3.adm3)+
  geom_sf(aes(fill= mean), color = "white")+
  scale_fill_viridis_c(direction = -1, name="% Timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="Timely Penta2-Penta3 interval")

###save plot at 1000 by 400 (timely_p2p3.adm1, timely_p2p3.adm2, and timely_p2p3.adm3)

########################################################################################################################################################

