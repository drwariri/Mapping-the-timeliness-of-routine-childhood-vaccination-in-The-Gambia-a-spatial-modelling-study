##########Load packages################################
library(ggplot2)
library(dplyr)
library(rgdal)
library(sf)
library(ggsci)
library(cowplot)
library(biscale)
library(maptools)
library(patchwork)


####set working directory##############################
setwd("filepath")

###create filepath to read in csv outputs from########
filePathData <- "filepath0"

# Read in the shapefile for the districts:
spol.adm2 <- readShapePoly(paste0("gmb_admbnda_adm2_2022.shp"))
spol.adm3 <- readShapePoly(paste0("gmb_admbnda_adm3_2022.shp"))


#1. %HepB0_delayed and number of children delayed #####################################################################################################

#adm2#######################################################################################
# Load your data for the % delayed and the distribution of delay:

delayed_data.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_HepB0_delayed.csv"), header=TRUE)
distribution_data.adm2 <- read.csv(paste0(filePathData, "est_num_delayed_HepB0_adm2.csv"), header=TRUE)

# Merge the two datasets by district ID, select the variables needed (ID, mean.x, and num.delayed.adm2) and mutate num.delayed (i.e. rename and /100)
#adm2
merged_data.adm2 <- merge(delayed_data.adm2, distribution_data.adm2, by = "ID") %>% 
  select("ID", "mean.x", "num.delayed.adm2")%>% 
  mutate(num.delayed = num.delayed.adm2/100)

#Add proportion with delayed vaccination (mean) and number of children (num.delayed) to the shape file
spol.adm2$mean.x <- merged_data.adm2$mean.x
spol.adm2$num.delayed <- merged_data.adm2$num.delayed

#Coordinates
coord <- coordinates(spol.adm2)
spol.adm2$long <- coord[, 1]
spol.adm2$lat <- coord[, 2]

#Convert to sf object
spolsf <- st_as_sf(spol.adm2)

# create classes
data.adm2 <- bi_class(spolsf, x = mean.x, y = num.delayed, style = "quantile", dim = 3)


##write adm2/adm3 files as csv for writing results
HepB0.data.adm2_nogeom <- st_drop_geometry(data.adm2)
write.csv(HepB0.data.adm2_nogeom, paste0("HepB0.bivariate.adm2.csv"))


# create map
bi.hepb.adm2 <- ggplot() +
  geom_sf(data = data.adm2, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3, na.value="gray48") +
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  ggtitle("Bivariate distribution (number of children vs delayed HepB0) at 2nd admin-level")

# Zoom in on a specific region of the map
bi.hepb.adm2 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_bi.hepB0.adm2 (750 x 450)


#adm3#######################################################################################################
# Load your data for the % delayed and the distribution of delay
delayed_data.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_HepB0_delayed.csv"), header=TRUE)
distribution_data.adm3 <- read.csv(paste0(filePathData, "est_num_delayed_HepB0_adm3.csv"), header=TRUE)

# Merge the two datasets by district ID, select the varivales needed (ID, mean.x, and num.delayed.adm2) and mutate num.delayed (i.e. rename and /100)
merged_data.adm3 <- merge(delayed_data.adm3, distribution_data.adm3, by = "ID") %>% 
  select("ID", "mean.x", "num.delayed.adm3")%>% 
  mutate(num.delayed = num.delayed.adm3/100)

#create new data object with 115 rows
IDx <- 1:115

# Create data frame with ID and IDx values
df2 <- data.frame(ID = 1:115, IDx = IDx)

# Merge df1 and df2 based on id column
merged_data.adm3 <- merge(merged_data.adm3, df2, by = "ID", all = TRUE)

#Add proportion with delayed vaccination (mean) and number of children (num.delayed) to the shape file
spol.adm3$mean.x <- merged_data.adm3$mean.x
spol.adm3$num.delayed <- merged_data.adm3$num.delayed


#Coordinates
coord <- coordinates(spol.adm3)
spol.adm3$long <- coord[, 1]
spol.adm3$lat <- coord[, 2]

#Convert to sf object
spolsf <- st_as_sf(spol.adm3)

# create classes
data.adm3 <- bi_class(spolsf, x = mean.x, y = num.delayed, style = "quantile", dim = 3)
##Remove NAs from the adm3 data
data.adm3 <- na.omit(data.adm3)

##write adm2/adm3 files as csv for writing results
HepB0.data.adm3_nogeom <- st_drop_geometry(data.adm3)
write.csv(HepB0.data.adm3_nogeom, paste0("HepB0.bivariate.adm3.csv"))

# create map
bi.hepb.adm3 <- ggplot() +
  geom_sf(data = data.adm3, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3, na.value="gray48") +
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  ggtitle("Bivariate distribution (number of children vs delayed HepB0) at 3rd admin-level")

# Zoom in on a specific region of the map
bi.hepb.adm3 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_bi.hepB0.adm3 (750 x 450)

#create legend############################################################################################

legend.hepb <- bi_legend(pal = "DkViolet",
                          dim = 3,
                          xlab = "Delayed HepB0 (%)",
                          ylab = "Number of children",
                          size = 12)

#Stitch maps and legend together using patchwork##################
bi.hepb.adm2 + legend.hepb + bi.hepb.adm3 + 
  plot_layout(widths = c(7, 1))

bi.hepb.adm2 + legend.hepb + plot_layout(widths = c(7, 1.5))
bi.hepb.adm3 + legend.hepb + plot_layout(widths = c(7, 1.5))

#bivariate_hepb <- bi.hepb.adm2 + bi.hepb.adm3 + legend.hepb +  plot_layout(widths = c(5, 5, 1))

###save plot as png at 1100 x 600 using (bivariate_hepb0)

#######################################################################################################################################################

#2. penta3_delayed and number of children delayed #####################################################################################################

#adm2############################################################
# Load your data for the % delayed and the distribution of delay:

delayed_data.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_penta3_delayed.csv"), header=TRUE)
distribution_data.adm2 <- read.csv(paste0(filePathData, "est_num_delayed_penta3_adm2.csv"), header=TRUE)

# Merge the two datasets by district ID, select the varivales needed (ID, mean.x, and num.delayed.adm2) and mutate num.delayed (i.e. rename and /100)
#adm2
merged_data.adm2 <- merge(delayed_data.adm2, distribution_data.adm2, by = "ID") %>% 
  select("ID", "mean.x", "num.delayed.adm2")%>% 
  mutate(num.delayed = num.delayed.adm2)

#Add proportion with delayed vaccination (mean) and number of children (num.delayed) to the shape file
spol.adm2$mean.x <- merged_data.adm2$mean.x
spol.adm2$num.delayed <- merged_data.adm2$num.delayed

#Coordinates
coord <- coordinates(spol.adm2)
spol.adm2$long <- coord[, 1]
spol.adm2$lat <- coord[, 2]

#Convert to sf object
spolsf <- st_as_sf(spol.adm2)

# create classes
p3.data.adm2 <- bi_class(spolsf, x = mean.x, y = num.delayed, style = "quantile", dim = 3)


##write adm2/adm3 files as csv for writing results
p3.data.adm2_nogeom <- st_drop_geometry(p3.data.adm2)
write.csv(p3.data.adm2_nogeom, paste0("p3.bivariate.adm2.csv"))



# create map
bi.p3.adm2 <- ggplot() +
  geom_sf(data = data.adm2, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3, na.value="gray48") +
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Bivariate distribution (number of children vs delayed PENTA3) at 2nd admin-level")

# Zoom in on a specific region of the map
bi.p3.adm2 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_bi.p3.adm2 (750 x 450)


#adm3###########################################################
# Load your data for the % delayed and the distribution of delay
delayed_data.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_penta3_delayed.csv"), header=TRUE)
distribution_data.adm3 <- read.csv(paste0(filePathData, "est_num_delayed_penta3_adm3.csv"), header=TRUE)

# Merge the two datasets by district ID, select the varivales needed (ID, mean.x, and num.delayed.adm2) and mutate num.delayed (i.e. rename and /100)
merged_data.adm3 <- merge(delayed_data.adm3, distribution_data.adm3, by = "ID") %>% 
  select("ID", "mean.x", "num.delayed.adm3")%>% 
  mutate(num.delayed = num.delayed.adm3)

#create new data object with 115 rows
IDx <- 1:115

# Create data frame with ID and IDx values
df2 <- data.frame(ID = 1:115, IDx = IDx)

# Merge df1 and df2 based on id column
merged_data.adm3 <- merge(merged_data.adm3, df2, by = "ID", all = TRUE)

#Add proportion with delayed vaccination (mean) and number of children (num.delayed) to the shape file
spol.adm3$mean.x <- merged_data.adm3$mean.x
spol.adm3$num.delayed <- merged_data.adm3$num.delayed


#Coordinates
coord <- coordinates(spol.adm3)
spol.adm3$long <- coord[, 1]
spol.adm3$lat <- coord[, 2]

#Convert to sf object
spolsf <- st_as_sf(spol.adm3)

# create classes
data.adm3 <- bi_class(spolsf, x = mean.x, y = num.delayed, style = "quantile", dim = 3)
##Remove NAs from the adm3 data
data.adm3 <- na.omit(data.adm3)


##write adm2/adm3 files as csv for writing results
data.adm3_nogeom <- st_drop_geometry(data.adm3)
write.csv(data.adm3_nogeom, paste0("p3.bivariate.adm3.csv"))


# create map
bi.p3.adm3 <- ggplot() +
  geom_sf(data = data.adm3, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3, na.value="gray48") +
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs( title = "Bivariate distribution (number of children vs delayed PENTA3) at 3rd admin-level")

# Zoom in on a specific region (greater Banjul) of the map
bi.p3.adm3 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_bi.p3.adm3 (750 x 450)

#create legend###############################################

legend.p3 <- bi_legend(pal = "DkViolet",
                          dim = 3,
                          xlab = "Delayed Penta3 (%)",
                          ylab = "Number of children",
                          size = 9)

#Stitch maps and legend together using patchwork###########
bi.p3.adm2 + legend.p3 + plot_layout(widths = c(7, 1.5))
bi.p3.adm3 + legend.p3 + plot_layout(widths = c(7, 1.5))

#bivariate_penta3 <- bi.p3.adm2 + bi.p3.adm3 + legend.p3 +  plot_layout(widths = c(5, 5, 1))

###save plot as png at 1100 x 600 using (bivariate_penta3)
#######################################################################################################################################################

#3. %mcv1_delayed and number of children delayed #####################################################################################################

#adm2############################################################
# Load your data for the % delayed and the distribution of delay:

delayed_data.adm2 <- read.csv(paste0(filePathData, "adm2_estimates_mcv1_delayed.csv"), header=TRUE)
distribution_data.adm2 <- read.csv(paste0(filePathData, "est_num_delayed_mcv1_adm2.csv"), header=TRUE)

# Merge the two datasets by district ID, select the varivales needed (ID, mean.x, and num.delayed.adm2) and mutate num.delayed (i.e. rename and /100)
#adm2
merged_data.adm2 <- merge(delayed_data.adm2, distribution_data.adm2, by = "ID") %>% 
  select("ID", "mean.x", "num.delayed.adm2")%>% 
  mutate(num.delayed = num.delayed.adm2)

#Add proportion with delayed vaccination (mean) and number of children (num.delayed) to the shape file
spol.adm2$mean.x <- merged_data.adm2$mean.x
spol.adm2$num.delayed <- merged_data.adm2$num.delayed

#Coordinates
coord <- coordinates(spol.adm2)
spol.adm2$long <- coord[, 1]
spol.adm2$lat <- coord[, 2]

#Convert to sf object
spolsf <- st_as_sf(spol.adm2)

# create classes
data.adm2 <- bi_class(spolsf, x = mean.x, y = num.delayed, style = "quantile", dim = 3)



##write adm2/adm3 files as csv for writing results
data.adm2_nogeom <- st_drop_geometry(data.adm2)
write.csv(data.adm2_nogeom, paste0("mcv1.bivariate.adm2.csv"))



# create map
bi.mcv1.adm2 <- ggplot() +
  geom_sf(data = data.adm2, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3, na.value="gray48") +
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  ggtitle("Bivariate distribution (number of children vs delayed MCV1) at 2nd admin-level")

# Zoom in on a specific region of the map
bi.mcv1.adm2 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_bi.mcv1.adm2 (750 x 450)


#adm3###########################################################
# Load your data for the % delayed and the distribution of delay
delayed_data.adm3 <- read.csv(paste0(filePathData, "adm3_estimates_mcv1_delayed.csv"), header=TRUE)
distribution_data.adm3 <- read.csv(paste0(filePathData, "est_num_delayed_mcv1_adm3.csv"), header=TRUE)

# Merge the two datasets by district ID, select the varivales needed (ID, mean.x, and num.delayed.adm2) and mutate num.delayed (i.e. rename and /100)
merged_data.adm3 <- merge(delayed_data.adm3, distribution_data.adm3, by = "ID") %>% 
  select("ID", "mean.x", "num.delayed.adm3")%>% 
  mutate(num.delayed = num.delayed.adm3)

#create new data object with 115 rows
IDx <- 1:115

# Create data frame with ID and IDx values
df2 <- data.frame(ID = 1:115, IDx = IDx)

# Merge df1 and df2 based on id column
merged_data.adm3 <- merge(merged_data.adm3, df2, by = "ID", all = TRUE)

#Add proportion with delayed vaccination (mean) and number of children (num.delayed) to the shape file
spol.adm3$mean.x <- merged_data.adm3$mean.x
spol.adm3$num.delayed <- merged_data.adm3$num.delayed


#Coordinates
coord <- coordinates(spol.adm3)
spol.adm3$long <- coord[, 1]
spol.adm3$lat <- coord[, 2]

#Convert to sf object
spolsf <- st_as_sf(spol.adm3)

# create classes
data.adm3 <- bi_class(spolsf, x = mean.x, y = num.delayed, style = "quantile", dim = 3)
##Remove NAs from the adm3 data
data.adm3 <- na.omit(data.adm3)


##write adm2/adm3 files as csv for writing results
data.adm3_nogeom <- st_drop_geometry(data.adm3)
write.csv(data.adm3_nogeom, paste0("mcv1.bivariate.adm3.csv"))


# create map
bi.mcv1.adm3 <- ggplot() +
  geom_sf(data = data.adm3, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3, na.value="gray48") +
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  ggtitle("Bivariate distribution (number of children vs delayed MCV1) at 3rd admin-level")

# Zoom in on a specific region of the map
bi.mcv1.adm3 + coord_sf(xlim = c(-16.8, -16.55), ylim = c(13.35, 13.5))

###save zoomed_bi.mcv1.adm3 (750 x 450)

#create legend###############################################

legend.mcv1 <- bi_legend(pal = "DkViolet",
                       dim = 3,
                       xlab = "Delayed MCV1 (%)",
                       ylab = "Number of children",
                       size = 12)

#Stitch maps and legend together using patchwork###########
bi.mcv1.adm2 + legend.mcv1 + bi.mcv1.adm3 + 
  plot_layout(widths = c(7, 1))

bi.mcv1.adm2 + legend.mcv1 + plot_layout(widths = c(7, 1.5))
bi.mcv1.adm3 + legend.mcv1 + plot_layout(widths = c(7, 1.5))

#bivariate_mcv1 <- bi.mcv1.adm2 + bi.mcv1.adm3 + legend.mcv1 +  plot_layout(widths = c(5, 5, 1))

###save plot as png at 1100 x 600 using (bivariate_mcv1)
#######################################################################################################################################################

