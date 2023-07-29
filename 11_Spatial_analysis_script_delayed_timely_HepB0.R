
library(INLA)
#INLA:::inla.dynload.workaround()
library(raster); library(maptools)
library(gtools); library(sp); library(spdep)
library(rgdal)
library(ggplot2)
library(dplyr)
library(sf)
library(ggrepel)
#library(Hmisc)

#Set working directory
setwd("filepath")

filePathData <- "filepath0"
filePathData1 <- "filepath1"
filePathData2 <- "filepath2"

#loading the data
dat <- read.csv("outcome_data.csv", header=TRUE)
dat.cov <- read.csv(paste0(filePathData, "HepB0_covariates_selected.csv"), header=TRUE)

#Aggregate outcome data file to cluster level
dat1 <- group_by(dat, DHSCLUST) %>%
  summarize(total = n(), 
            total_hepB0 = length(which(!is.na(hepB0_timely))), 
            hepB0_timely_count = sum(hepB0_timely, na.rm = TRUE),
            hepB0_timely_prop = hepB0_timely_count/total_hepB0,
            hepB0_delayed_count = sum(hepB0_delayed, na.rm = TRUE),
            hepB0_delayed_prop = hepB0_delayed_count/total_hepB0)


data.merge <- merge(dat1, dat.cov, by="DHSCLUST")
coords <- data.frame(LONGNUM = data.merge$longitude, LATNUM = data.merge$latitude)

head(data.merge)

vaxcov  <- data.merge[,8:14]   #includes coordinates as Longitude & Latitude
vaxdata <- data.merge[,c(2:7,15:16)]

#################################################################################################
##Create dataset for clusterplot of HepB0_delayed
clustplot <- vaxdata %>% 
  mutate(hepB0_delayed_prop = hepB0_delayed_prop*100) %>% 
  select(hepB0_delayed_prop, longitude, latitude) %>% 
  rename(outcome = hepB0_delayed_prop)

##Create dataset for clusterplot of HepB0_timely
clustplot_timely <- vaxdata %>% 
  mutate(hepB0_timely_prop = hepB0_timely_prop*100) %>% 
  select(hepB0_timely_prop, longitude, latitude) %>% 
  rename(outcome = hepB0_timely_prop)


##############
# Read in shapefiles for admin levels 1 and 2
gmb_shp_1 <- st_read(paste0(filePathData2,"gmb_admbnda_adm1_2022.shp"))
gmb_shp <- st_read(paste0(filePathData2,"gmb_admbnda_adm2_2022.shp"))

# Convert both shapefiles to sf objects with the same geometry column name
gmb_shp_1_sf <- st_as_sf(gmb_shp_1, "newname")
gmb_shp_sf <- st_as_sf(gmb_shp, "newname")

# Combine shapefiles
gmb_combined <- bind_rows(gmb_shp_1_sf, gmb_shp_sf)

# Create a variable to distinguish between admin 1 and admin 2 boundaries
gmb_combined$type <- ifelse(is.na(gmb_combined$ADM2_EN), "ADM1_EN", "ADM2_EN")
##################


##plot spatial distribution of delayed hepB0 in The Gambia
  ggplot() +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM2_EN",], color = "white", size = 0.8) +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM1_EN",], color = "black", size = 0.9, alpha = 0.1) + 
  geom_point(data = clustplot, aes(x=longitude, y=latitude, colour=outcome), size=2)+
  scale_colour_viridis_c(option = "inferno", direction = -1, name="Delayed (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title = element_blank())+
  labs(title="Cluster-level observed delayed HepB0")



ggplot() +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM2_EN",], color = "white", size = 0.8) +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM1_EN",], aes(fill = ADM1_EN), color = "black", size = 1, alpha = 0.1, show.legend = FALSE) + 
  geom_point(data = clustplot, aes(x=longitude, y=latitude, colour=outcome), size=2)+
  scale_colour_viridis_c(option = "inferno", direction = -1, name="Delayed (%)")+
  scale_fill_viridis_d(name = "ADM1_EN", option = "inferno", direction = -1, alpha = 0.8) +
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title = element_blank())+
  labs(title="Cluster-level observed delayed HepB0", fill = "ADM1_EN") +
  guides(fill = guide_legend(position = "bottom", title.position = "top")) +
  annotate("segment", x = -14.0, y = 13.52, xend = -14.0, yend = 13.7, size = 0.5, color = "black") + # add a line annotation
  annotate("text", x = -14.1, y = 13.74, label = "Basse", hjust = 0, size = 4)+# add a text annotation
  annotate("segment", x = -15.0, y = 13.52, xend = -15.0, yend = 13.3, size = 0.5, color = "black") + 
  annotate("text", x = -15.1, y = 13.27, label = "Janjabureh", hjust = 0, size = 4)+ 
  annotate("segment", x = -14.7, y = 13.6, xend = -14.7, yend = 13.75, size = 0.5, color = "black") + 
  annotate("text", x = -14.8, y = 13.8, label = "Kuntaur", hjust = 0, size = 4)+
  annotate("segment", x = -15.6, y = 13.4, xend = -15.6, yend = 13.2, size = 0.5, color = "black") + 
  annotate("text", x = -15.75, y = 13.18, label = "Mansakonko", hjust = 0, size = 4)+
  annotate("segment", x = -16.0, y = 13.5, xend = -16.0, yend = 13.75, size = 0.5, color = "black") + 
  annotate("text", x = -16.1, y = 13.8, label = "Kerewan", hjust = 0, size = 4)+
  annotate("segment", x = -16.3, y = 13.25, xend = -16.3, yend = 13.12, size = 0.5, color = "black") + 
  annotate("text", x = -16.4, y = 13.1, label = "Brikama", hjust = 0, size = 4)+
  annotate("segment", x = -16.65, y = 13.45, xend = -16.65, yend = 13.55, size = 0.5, color = "black") + 
  annotate("text", x = -16.85, y = 13.6, label = "Kanifing", hjust = 0, size = 4)+
  annotate("segment", x = -16.6, y = 13.45, xend = -16.6, yend = 13.75, size = 0.5, color = "black") + 
  annotate("text", x = -16.7, y = 13.8, label = "Banjul", hjust = 0, size = 4)
  




##plot spatial distribution of timely hepB0 in The Gambia
#scale_colour_gradientn(colours = terrain.colors(10), name="% delayed")# another way of changing colours

ggplot(gmb_shp) + 
  geom_sf() + 
  geom_point(data = clustplot_timely, aes(x=longitude, y=latitude, colour=outcome), size=2)+
  scale_colour_viridis_c(direction = -1, name="% timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="hepB0")

#3. save the image as PNG using name (clustplot_delayed_hepB0 and clustplot_timely_hepB0) with aspect ratio with = 1000 and height = 400
######################################################################################################################################


#Delete clusters where TotChild is zero from vaxdata only
zero.clust <- which(is.na(vaxdata$total_hepB0)|vaxdata$total_hepB0<=1)
if (length(zero.clust)>0){
  vaxdata <- vaxdata[-zero.clust,]
  vaxcov  <- vaxcov[-zero.clust,]
}

#All_ages
Numvacc    <- vaxdata$hepB0_delayed_count
weights    <- vaxdata$total_hepB0

#Coordinates
coords    <- cbind(vaxdata$longitude,vaxdata$latitude)
min.dist  <- as.numeric(summary(dist(coords))[1]) #Min of distances between cluster locations #0.0003146808

set.seed(500)

#Covariates
xp1     <- vaxcov$GMB_n12_dst_edge_cultivated_areas_2015
xp2     <- vaxcov$GMB_n42_elevation_model
xp3	    <- vaxcov$l_GMB_n66_pigs_density
xp4	    <- vaxcov$GMB_n94_avg_number_WetDays_2016_2019
xp5	    <- vaxcov$GMB_n95_proximity_national_borders
xp6     <- vaxcov$GMB_n102_avg_potential_evapotranspiration_2016_2019_mean
xp7     <- vaxcov$urban_rural


#Read in prediction covariates
GMB_n12  	<- raster(paste0(filePathData1,"GMB_n12_dst_edge_cultivated_areas_2015.tif"))
GMB_n42  	<- raster(paste0(filePathData1,"GMB_n42_elevation_model.tif")) 
l_GMB_n66 	<- raster(paste0(filePathData1,"GMB_n66_pigs_density.tif"))
GMB_n94  	<- raster(paste0(filePathData1,"GMB_n94_avg_number_WetDays_2016_2019.tif"))
GMB_n95	<- raster(paste0(filePathData1,"GMB_n95_proximity_national_borders.tif"))
GMB_n102	<- raster(paste0(filePathData1,"GMB_n102_avg_potential_evapotranspiration_2016_2019_mean.tif"))
GMB_urban_rural	<- raster(paste0(filePathData1,"gmb_urban_rural_1km.tif"))


#Prediction covariates
x1gp  	<- getValues(GMB_n12)
x2gp 	<- getValues(GMB_n42)
x3gp 	<- log(getValues(l_GMB_n66) + 0.05)   			
x4gp	<- getValues(GMB_n94) 
x5gp	<- getValues(GMB_n95) 
x6gp	<- getValues(GMB_n102)
x7gp	<- getValues(GMB_urban_rural) 


#Prediction grid
n25.p      <- raster(paste0(filePathData1,"GMB_n12_dst_edge_cultivated_areas_2015.tif"))
Pred_grid2 <- coordinates(n25.p)

#Population data for population-weighted aggregation
popc     <- raster(paste0(filePathData1,"GMB_n106_population_under_1_years_old_pop-2019.tif"))
popc	 <- getValues(popc) 

#Combine grid and covariates
pred.dat <- cbind(Pred_grid2, x1gp, x2gp, x3gp, x4gp, x5gp, x6gp, x7gp, popc) # 

ind <- apply(pred.dat, 1, function(x) any(is.na(x)))
miss    <- which(ind==TRUE)
nonmiss <- which(ind==FALSE)

pred.dat.1 <- pred.dat[nonmiss, ]
pop <- pred.dat.1[,10]              #Population counts for weighted aggregation
coord.p <- pred.dat.1[,1:2]
ypred=npred=rep(NA, nrow(pred.dat.1))

shp_gmb  <- readShapePoly(paste0(filePathData2,"gmb_admbnda_adm0_2022.shp"))

#meshfit: fine triangulated mesh
shp_df <- fortify(shp_gmb)
shp.bnd <- cbind(shp_df$long, shp_df$lat)
c.bnd <- as.matrix(shp.bnd)	
meshfit <- inla.mesh.2d(loc=coords,loc.domain=c.bnd, max.edge=c(0.07, 0.3),cutoff=0.01)			
#meshfit <- inla.mesh.2d(loc=coords,loc.domain=c.bnd, max.edge=c(0.2, 0.6),cutoff=0.18)
plot(meshfit); plot(shp_gmb, add=TRUE); points(coords)


#For priors
nu <- 1 #Matern smoothness parameter, redundant here as it implies alpha=2
alpha <- 2
#ran.pr.fit <- med.dist  #Median of distances between data locations

#kap.pr.fit <- sqrt(8*nu)/ran.pr.fit
# Matern SPDE model object
#spde <- inla.spde2.matern(mesh=meshfit, alpha=alpha,
#                          B.tau=matrix(c(0, 1, 0),nrow=1,ncol=3),
#                          B.kappa=matrix(c(0, 0, 1),nrow=1,ncol=3), 
#                          theta.prior.mean=c(0, log(kap.pr.fit)),
#                          theta.prior.prec=c(1, 1))

r0 <- 0.15 #This is 5% of the extent of Gambia in the east-west direction (i.e. 0.05*(xmax-xmin))

#Matern SPDE model object using inla.pcmatern
spde <- inla.spde2.pcmatern(mesh=meshfit, alpha=alpha, prior.range=c(r0, 0.01), prior.sigma=c(3, 0.01)) 

#For adm3 estimates
coord.p <- pred.dat.1[,1:2]
spol1   <- readShapePoly(paste0(filePathData2,"gmb_admbnda_adm3_2022.shp"))
spol    = as(spol1, "SpatialPolygons")   #NOTE - no data in spol
sp.1    <- rep(NA, nrow(coord.p))
for(i in 1:length(spol)){
  sp.1[as.vector(which(!is.na(over(SpatialPoints(coord.p), spol[i]))))] <- i
}

#For adm2 estimates
spol2   <- readShapePoly(paste0(filePathData2,"gmb_admbnda_adm2_2022.shp"))
spol    = as(spol2, "SpatialPolygons")   #NOTE - no data in spol
sp.2    <- rep(NA, nrow(coord.p))
for(i in 1:length(spol)){
  sp.2[as.vector(which(!is.na(over(SpatialPoints(coord.p), spol[i]))))] <- i
}

#For adm1 estimates
spol3   <- readShapePoly(paste0(filePathData2,"gmb_admbnda_adm1_2022.shp"))
spol    = as(spol3, "SpatialPolygons")   #NOTE - no data in spol
sp.3    <- rep(NA, nrow(coord.p))
for(i in 1:length(spol)){
  sp.3[as.vector(which(!is.na(over(SpatialPoints(coord.p), spol[i]))))] <- i
}

# Observation points
X0 <- model.matrix(~ -1 + xp1 + xp2 + xp3 + xp4 + xp5 + xp6 + factor(xp7))
Xobs <-  as.data.frame(X0[,-which(colnames(X0)%in%c("factor(xp7)0"))])
colnames(Xobs) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7_1") # 
Ap.i <- inla.spde.make.A(mesh=meshfit, loc=coords)
lp.i = rep(1,length(xp1))
stk.point <- inla.stack(tag='point',
                        data=list(y=Numvacc,n=weights),
                        A=list(Ap.i,1,1,1),
                        effects=list(s=1:spde$n.spde, rr=1:length(weights), intercept=rep(1, length(xp1)), Xobs))  #NOTE

# Prediction points - can be moved outside the loop
xpred1 <- pred.dat.1[,3]; xpred2 <- pred.dat.1[,4]; xpred3 <- pred.dat.1[,5]; xpred4 <- pred.dat.1[,6]; xpred5 <- pred.dat.1[,7]; 
xpred6 <- pred.dat.1[,8]; xpred7 <- pred.dat.1[,9]
X0 <- model.matrix(~ -1 + xpred1 + xpred2 + xpred3 + xpred4 + xpred5 + xpred6 + factor(xpred7))
#
Xpred <-  as.data.frame(X0[,-which(colnames(X0)%in%c("factor(xpred7)0"))])
colnames(Xpred) <- c("x1", "x2", "x3", "x4", "x5_1", "x6", "x7_1") #
Apred <- inla.spde.make.A(mesh=meshfit, loc=coord.p)
lpred = rep(1,nrow(pred.dat.1))
stk.pred <- inla.stack(tag='pred',
                       data=list(y=ypred,n=npred),
                       A=list(Apred,1,1),
                       effects=list(s=1:spde$n.spde, rr=(length(weights)+1):(length(weights)+nrow(pred.dat.1)), Xpred)) #NOTE

#Points, grid
# Stack
stk.full <- inla.stack(stk.point)  #Note no stk.pred and stk.val

# Fit model
hyper.prec = list(theta = list(prior="pc.prec", param=c(3,0.01)))
control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000)  

#Note hyperparamters for iid RE, in INLA default is 1, 0.00001 - see documentation                                                         
# beta_i has a N(0, 10^6) default prior
formula  <- y ~ -1 + intercept + x1 + x2 + x3 + x4 + x5 + x6 + x7_1 + f(s, model=spde) + f(rr, model="iid", hyper = hyper.prec) #
res <- inla(formula, data=inla.stack.data(stk.full), family="binomial", 
            Ntrials = stk.full$data$data$n,
            control.predictor=list(compute=TRUE, A=inla.stack.A(stk.full), link=1),
            control.compute=list(dic=TRUE, config = TRUE, waic=TRUE),
            control.fixed=control.fixed)

#save model
#save(res, file=paste0(filePathData2,"inla_model_dtp1_12_23.rda"))
#load(paste0(filePathData2,"inla_model_dtp1_12_23.rda"))

spde.result <- inla.spde2.result(inla=res,name="s",spde=spde)

#Parameters
coeff.reg <- summary(res)$fixed[,1:5]

#range for spatial RE
range.mean = inla.emarginal(function(x) x, spde.result$marginals.range.nominal[[1]]); #range.mean
range.ex.sq = inla.emarginal(function(x) x^2, spde.result$marginals.range.nominal[[1]])
range.sd = sqrt(range.ex.sq-(range.mean^2)); #range.sd
range.quant = inla.qmarginal(c(0.025,0.5,0.975), spde.result$marginals.range.nominal[[1]]);# range.quant 
range <- c(range.mean, range.sd, range.quant)

#variance for spatial RE
variance.mean = inla.emarginal(function(x) x, spde.result$marginals.variance.nominal[[1]]); #variance.mean
variance.ex.sq = inla.emarginal(function(x) x^2, spde.result$marginals.variance.nominal[[1]])
variance.sd = sqrt(variance.ex.sq-(variance.mean^2)); #variance.sd
variance.quant = inla.qmarginal(c(0.025,0.5,0.975), spde.result$marginals.variance.nominal[[1]]); #variance.quant 
variance <- c(variance.mean, variance.sd, variance.quant)

#variance for IID RE
#variance of IID random effect

var.ind      <- inla.tmarginal(function(x) 1/x, res$marginals.hyperpar[[3]])

var.iid      <- inla.zmarginal(var.ind,silent=TRUE)

variance.iid <- c(var.iid$mean, var.iid$sd, var.iid$quant0.025, var.iid$quant0.5, var.iid$quant0.975)

#iid.var <- res$summary.hyperpar[3,1:5]
param.all <- rbind(coeff.reg,range,variance, variance.iid)

write.csv(param.all, paste0("parameter_output_HepB0_delayed.csv"))


#Observation points
index.pred.obs 	<- inla.stack.index(stk.full, tag = "point")$data
fitted.pred.all.obs = round(res$summary.fitted.values[index.pred.obs,1:5], 4)
fitted.pred.mean.obs1 = as.vector(data.matrix(as.vector(fitted.pred.all.obs[,"mean"])))
fitted.pred.sd.obs1 = as.vector(data.matrix(as.vector(fitted.pred.all.obs[,"sd"])))
fitted.pred.low.obs1 = as.vector(data.matrix(as.vector(fitted.pred.all.obs[,"0.025quant"])))
fitted.pred.up.obs1 = as.vector(data.matrix(as.vector(fitted.pred.all.obs[,"0.975quant"])))

#length(fitted.pred.mean.obs1)
prob.obs <- Numvacc/weights
plot(prob.obs, fitted.pred.mean.obs1)
#cor(prob.obs, fitted.pred.mean.obs1)
ds <- data.frame(pred.prob=fitted.pred.mean.obs1, pred.obs=prob.obs, Vax=Numvacc, Tot=weights, 
                 pred.sd = fitted.pred.sd.obs1, pred.low=fitted.pred.low.obs1, pred.up=fitted.pred.up.obs1)
write.csv(ds, paste0("obsvpred_HepB0_delayed.csv"))


##################POSTERIOR SAMPLING
nsamp <- 1000

#Posterior sampling
ps <- inla.posterior.sample(nsamp, res) 
contents <- res$misc$configs$contents

#ID for spatial random effect
idSpace <- contents$start[which(contents$tag=="s")]-1 +
  (1:contents$length[which(contents$tag=="s")])

#ID for iid effects
idR <- contents$start[which(contents$tag=="rr")]-1 +
  (1:contents$length[which(contents$tag=="rr")])

#ID for fixed effects
idX <- contents$start[which(contents$tag=="intercept")]-1 + (1:8) # fixed effects, 8 = no of regression coefficients

# extract samples 
xLatent <- matrix(0, nrow=length(ps[[1]]$latent), ncol=nsamp) 
xHyper <- matrix(0, nrow=length(ps[[1]]$hyperpar), ncol=nsamp) 
for(i in 1:nsamp){
  xLatent[,i] <- ps[[i]]$latent
  xHyper[,i] <- ps[[i]]$hyperpar
}
xSpace <- xLatent[idSpace,]
XR <- xLatent[idR,]
xX <- xLatent[idX,]

# construct predictions
# in-sample
#linpred <- as.matrix(Ap.i %*% xSpace + as.matrix(cbind(1, Xobs$x1, Xobs$x2_1, Xobs$x3, Xobs$x4, Xobs$x5, Xobs$x6)) %*% xX + XR) #Compute linear predictor
#inv.linpred <- inv.logit(linpred)      #CHECK FUNCTION ON IRIDIS
#pred.obs <- data.frame(t(apply(inv.linpred, 1, FUN=function(x){ c(mean(x), sd(x), quantile(x, probs=c(0.025,0.5,0.975)))}))) 
#colnames(pred.obs) <- c("mean", "sd", "0.025", "median", "0.975")
#pred.obs.mean  <- as.vector(data.matrix(as.vector(pred.obs["mean"])))
#prob.obs <- Numvacc/weights
#rsq.obs <- (cor(pred.obs.mean,prob.obs))^2
#plot(prob.obs, pred.obs.mean)

#Prediction
#Draw samples for IID term
sample.IIDpred <- matrix(0, nrow(pred.dat.1),  nsamp)
for (i in 1:nsamp){
  ID.precision <- xHyper[3,i]                         #the 3rd row contains precision for rr; same as ps[[i]]$hyperpar[3]
  ID.sigma <- ID.precision^-0.5
  sample.IIDpred[, i] <- rnorm(nrow(pred.dat.1), sd=ID.sigma)
}

linpred     <- as.matrix(Apred %*% xSpace + as.matrix(cbind(1, Xpred$x1, Xpred$x2, Xpred$x3, Xpred$x4,  Xpred$x5, Xpred$x6, Xpred$x7_1)) %*% xX + sample.IIDpred)  #
inv.linpred <- inv.logit(linpred) 
inv.linpred <- inv.linpred*100

#Save draws for calculating remaining two indicators
save(inv.linpred, file=paste0("simprob_HepB0_delayed.rda"))

pred.grid    <- data.frame(t(apply(inv.linpred, 1, FUN=function(x){ c(mean(x), sd(x), quantile(x, probs=c(0.025,0.5,0.975)))}))) 
colnames(pred.grid) <- c("mean", "sd", "0.025quant", "0.5quant", "0.975quant")
fitted.pred.mean   <- as.vector(data.matrix(as.vector(pred.grid[,"mean"])))
fitted.pred.sd     <- as.vector(data.matrix(as.vector(pred.grid[,"sd"])))
fitted.pred.median <- as.vector(data.matrix(as.vector(pred.grid[,"0.5quant"])))
fitted.pred.low    <- as.vector(data.matrix(as.vector(pred.grid[,"0.025quant"])))
fitted.pred.up     <- as.vector(data.matrix(as.vector(pred.grid[,"0.975quant"])))

n25.p = raster(paste0(filePathData1,"GMB_n42_elevation_model.tif"))

#Mean
ll=1:length(ind); ll[nonmiss] = fitted.pred.mean; ll[miss] = NA
rr.mean = raster(n25.p); values(rr.mean) = ll

#sd
ll=1:length(ind); ll[nonmiss] = fitted.pred.sd; ll[miss] = NA
rr.sd = raster(n25.p); values(rr.sd) = ll

#low
ll=1:length(ind); ll[nonmiss] = fitted.pred.low; ll[miss] = NA
rr.low = raster(n25.p); values(rr.low) = ll

#up
ll=1:length(ind); ll[nonmiss] = fitted.pred.up; ll[miss] = NA
rr.up = raster(n25.p); values(rr.up) = ll

#median
ll=1:length(ind); ll[nonmiss] = fitted.pred.median; ll[miss] = NA
rr.med = raster(n25.p); values(rr.med) = ll

writeRaster(rr.mean, paste0("inla_vax_HepB0_delayed_mean.tif"), overwrite=TRUE)
writeRaster(rr.sd,   paste0("inla_vax_HepB0_delayed_sd.tif"), overwrite=TRUE)
writeRaster(rr.low,  paste0("inla_vax_HepB0_delayed_low.tif"), overwrite=TRUE)
writeRaster(rr.up,   paste0("inla_vax_HepB0_delayed_up.tif"), overwrite=TRUE)
writeRaster(rr.med,  paste0("inla_vax_HepB0_delayed_median.tif"), overwrite=TRUE)



#Calculate weighted population adm3, adm2 and adm1 estimates

#adm3 estimates and uncertainty (sd) 
dd    <- 1:nrow(spol1)
dd.un <- unique(sp.1)
dmiss <- which(!dd%in%dd.un)

if (length(dmiss)>0) dd_num <- dd[-dmiss]
if (length(dmiss)==0) dd_num <- dd

dist_out <- matrix(0, length(dd_num), 5)
for (i in 1:length(dd_num)){
  if (length(which(sp.1==dd_num[i]))==1){ 
    pop.ext <- pop[which(sp.1==dd_num[i])] 
    ext <- as.vector(sapply(inv.linpred[which(sp.1==dd_num[i]),], FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE))) 
  }
  if (length(which(sp.1==dd_num[i]))>1){  
    pop.ext <- pop[which(sp.1==dd_num[i])]
    ext <- as.vector(apply(inv.linpred[which(sp.1==dd_num[i]),], 2, FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE)))
  }
  
  dist_out[i,] <- as.vector(c(mean(ext), sd(ext), quantile(ext, probs=c(0.025,0.5,0.975))))						
}

dist_out <- cbind(dd_num, dist_out)
colnames(dist_out) <- c("ID", "mean", "sd", "0.025quant", "0.5quant", "0.975quant")

#The district-level estimates will have the same ordering as in the shapefile if they have the same no of areas
write.csv(dist_out, paste0("adm3_estimates_HepB0_delayed.csv"))


#adm2 estimates and uncertainty
dd    <- 1:nrow(spol2)
dd.un <- unique(sp.2)
dmiss <- which(!dd%in%dd.un)

if (length(dmiss)>0) dd_num <- dd[-dmiss]
if (length(dmiss)==0) dd_num <- dd

dist_out <- matrix(0, length(dd_num), 5)
for (i in 1:length(dd_num)){
  if (length(which(sp.2==dd_num[i]))==1){ 
    pop.ext <- pop[which(sp.2==dd_num[i])] 
    ext <- as.vector(sapply(inv.linpred[which(sp.2==dd_num[i]),], FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE))) 
  }
  if (length(which(sp.2==dd_num[i]))>1){  
    pop.ext <- pop[which(sp.2==dd_num[i])]
    ext <- as.vector(apply(inv.linpred[which(sp.2==dd_num[i]),], 2, FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE)))
  }
  
  dist_out[i,] <- as.vector(c(mean(ext), sd(ext), quantile(ext, probs=c(0.025,0.5,0.975))))						
}

dist_out <- cbind(dd_num, dist_out)
colnames(dist_out) <- c("ID", "mean", "sd", "0.025quant", "0.5quant", "0.975quant")

#The district-level estimates will have the same ordering as in the shapefile if they have the same no of areas
write.csv(dist_out, paste0("adm2_estimates_HepB0_delayed.csv"))


#adm1 estimates and uncertainty
dd    <- 1:nrow(spol3)
dd.un <- unique(sp.3)
dmiss <- which(!dd%in%dd.un)

if (length(dmiss)>0) dd_num <- dd[-dmiss]
if (length(dmiss)==0) dd_num <- dd

dist_out <- matrix(0, length(dd_num), 5)
for (i in 1:length(dd_num)){
  if (length(which(sp.3==dd_num[i]))==1){ 
    pop.ext <- pop[which(sp.3==dd_num[i])] 
    ext <- as.vector(sapply(inv.linpred[which(sp.3==dd_num[i]),], FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE))) 
  }
  if (length(which(sp.3==dd_num[i]))>1){  
    pop.ext <- pop[which(sp.3==dd_num[i])]
    ext <- as.vector(apply(inv.linpred[which(sp.3==dd_num[i]),], 2, FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE)))
  }
  
  dist_out[i,] <- as.vector(c(mean(ext, na.rm=TRUE), sd(ext, na.rm=TRUE), 
                              quantile(ext, probs=c(0.025,0.5,0.975), na.rm=TRUE)))							
}

dist_out <- cbind(dd_num, dist_out)
colnames(dist_out) <- c("ID", "mean", "sd", "0.025quant", "0.5quant", "0.975quant")

#The district-level estimates will have the same ordering as in the shapefile if they have the same no of areas
write.csv(dist_out, paste0("adm1_estimates_HepB0_delayed.csv"))


#Threshold calculations - 20%
ff1=function(x) length(which(x>=0.80))/nsamp
y.80 <- apply(inv.linpred, 1, ff1)   #Check me
ll=1:length(ind); ll[nonmiss] = y.80; ll[miss] = NA
rr.80 = raster(n25.p); values(rr.80) = ll
writeRaster(rr.80, paste0("inla_HepB0_delayed_80perc_thresh.tif"), overwrite=TRUE)


#########################################################################################################################################

####Calculation the numbers of children per admin-level with delayed vaccination
popc     <- raster(paste0(filePathData1,"GMB_n106_population_under_1_years_old_pop-2019.tif")) #reads in the raster of under-1 population
num.delayed.adm3 <- extract(rr.mean*popc, spol1, fun=sum, na.rm=TRUE)
num.delayed.adm2 <- extract(rr.mean*popc, spol2, fun=sum, na.rm=TRUE)
num.delayed.adm1 <- extract(rr.mean*popc, spol3, fun=sum, na.rm=TRUE)

totchild.adm3 <- extract(popc, spol1, fun=sum, na.rm=TRUE)
totchild.adm2 <- extract(popc, spol2, fun=sum, na.rm=TRUE)
totchild.adm1 <- extract(popc, spol3, fun=sum, na.rm=TRUE)

write.csv(data.frame(ID = 1:nrow(spol1), num.delayed.adm3, totchild = totchild.adm3), "est_num_delayed_HepB0_adm3.csv")
write.csv(data.frame(ID = 1:nrow(spol2), num.delayed.adm2, totchild = totchild.adm2), "est_num_delayed_HepB0_adm2.csv")
write.csv(data.frame(ID = 1:nrow(spol3), num.delayed.adm1, totchild = totchild.adm1), "est_num_delayed_HepB0_adm1.csv")




##############################################################################################################################################
#Calculate timely_HepB0
inv.linpred.1 <- 100 - inv.linpred

pred.grid    <- data.frame(t(apply(inv.linpred.1, 1, FUN=function(x){ c(mean(x), sd(x), quantile(x, probs=c(0.025,0.5,0.975)))}))) 
colnames(pred.grid) <- c("mean", "sd", "0.025quant", "0.5quant", "0.975quant")
fitted.pred.mean   <- as.vector(data.matrix(as.vector(pred.grid[,"mean"])))
fitted.pred.sd     <- as.vector(data.matrix(as.vector(pred.grid[,"sd"])))
fitted.pred.median <- as.vector(data.matrix(as.vector(pred.grid[,"0.5quant"])))
fitted.pred.low    <- as.vector(data.matrix(as.vector(pred.grid[,"0.025quant"])))
fitted.pred.up     <- as.vector(data.matrix(as.vector(pred.grid[,"0.975quant"])))

n25.p = raster(paste0(filePathData1,"GMB_n42_elevation_model.tif"))

#Mean
ll=1:length(ind); ll[nonmiss] = fitted.pred.mean; ll[miss] = NA
rr.mean = raster(n25.p); values(rr.mean) = ll

#sd
ll=1:length(ind); ll[nonmiss] = fitted.pred.sd; ll[miss] = NA
rr.sd = raster(n25.p); values(rr.sd) = ll

#low
ll=1:length(ind); ll[nonmiss] = fitted.pred.low; ll[miss] = NA
rr.low = raster(n25.p); values(rr.low) = ll

#up
ll=1:length(ind); ll[nonmiss] = fitted.pred.up; ll[miss] = NA
rr.up = raster(n25.p); values(rr.up) = ll

#median
ll=1:length(ind); ll[nonmiss] = fitted.pred.median; ll[miss] = NA
rr.med = raster(n25.p); values(rr.med) = ll

writeRaster(rr.mean, paste0("inla_vax_HepB0_timely_mean.tif"), overwrite=TRUE)
writeRaster(rr.sd,   paste0("inla_vax_HepB0_timely_sd.tif"), overwrite=TRUE)
writeRaster(rr.low,  paste0("inla_vax_HepB0_timely_low.tif"), overwrite=TRUE)
writeRaster(rr.up,   paste0("inla_vax_HepB0_timely_up.tif"), overwrite=TRUE)
writeRaster(rr.med,  paste0("inla_vax_HepB0_timely_median.tif"), overwrite=TRUE)



#Calculate weighted population adm3, adm2 and adm1 estimates

#adm3 estimates and uncertainty (sd) 
dd    <- 1:nrow(spol1)
dd.un <- unique(sp.1)
dmiss <- which(!dd%in%dd.un)

if (length(dmiss)>0) dd_num <- dd[-dmiss]
if (length(dmiss)==0) dd_num <- dd

dist_out <- matrix(0, length(dd_num), 5)
for (i in 1:length(dd_num)){
  if (length(which(sp.1==dd_num[i]))==1){ 
    pop.ext <- pop[which(sp.1==dd_num[i])] 
    ext <- as.vector(sapply(inv.linpred.1[which(sp.1==dd_num[i]),], FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE))) 
  }
  if (length(which(sp.1==dd_num[i]))>1){  
    pop.ext <- pop[which(sp.1==dd_num[i])]
    ext <- as.vector(apply(inv.linpred.1[which(sp.1==dd_num[i]),], 2, FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE)))
  }
  
  dist_out[i,] <- as.vector(c(mean(ext), sd(ext), quantile(ext, probs=c(0.025,0.5,0.975))))						
}

dist_out <- cbind(dd_num, dist_out)
colnames(dist_out) <- c("ID", "mean", "sd", "0.025quant", "0.5quant", "0.975quant")

#The district-level estimates will have the same ordering as in the shapefile if they have the same no of areas
write.csv(dist_out, paste0("adm3_estimates_HepB0_timely.csv"))


#adm2 estimates and uncertainty
dd    <- 1:nrow(spol2)
dd.un <- unique(sp.2)
dmiss <- which(!dd%in%dd.un)

if (length(dmiss)>0) dd_num <- dd[-dmiss]
if (length(dmiss)==0) dd_num <- dd

dist_out <- matrix(0, length(dd_num), 5)
for (i in 1:length(dd_num)){
  if (length(which(sp.2==dd_num[i]))==1){ 
    pop.ext <- pop[which(sp.2==dd_num[i])] 
    ext <- as.vector(sapply(inv.linpred.1[which(sp.2==dd_num[i]),], FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE))) 
  }
  if (length(which(sp.2==dd_num[i]))>1){  
    pop.ext <- pop[which(sp.2==dd_num[i])]
    ext <- as.vector(apply(inv.linpred.1[which(sp.2==dd_num[i]),], 2, FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE)))
  }
  
  dist_out[i,] <- as.vector(c(mean(ext), sd(ext), quantile(ext, probs=c(0.025,0.5,0.975))))						
}

dist_out <- cbind(dd_num, dist_out)
colnames(dist_out) <- c("ID", "mean", "sd", "0.025quant", "0.5quant", "0.975quant")

#The district-level estimates will have the same ordering as in the shapefile if they have the same no of areas
write.csv(dist_out, paste0("adm2_estimates_HepB0_timely.csv"))


#adm1 estimates and uncertainty
dd    <- 1:nrow(spol3)
dd.un <- unique(sp.3)
dmiss <- which(!dd%in%dd.un)

if (length(dmiss)>0) dd_num <- dd[-dmiss]
if (length(dmiss)==0) dd_num <- dd

dist_out <- matrix(0, length(dd_num), 5)
for (i in 1:length(dd_num)){
  if (length(which(sp.3==dd_num[i]))==1){ 
    pop.ext <- pop[which(sp.3==dd_num[i])] 
    ext <- as.vector(sapply(inv.linpred.1[which(sp.3==dd_num[i]),], FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE))) 
  }
  if (length(which(sp.3==dd_num[i]))>1){  
    pop.ext <- pop[which(sp.3==dd_num[i])]
    ext <- as.vector(apply(inv.linpred.1[which(sp.3==dd_num[i]),], 2, FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE)))
  }
  
  dist_out[i,] <- as.vector(c(mean(ext, na.rm=TRUE), sd(ext, na.rm=TRUE), 
                              quantile(ext, probs=c(0.025,0.5,0.975), na.rm=TRUE)))							
}

dist_out <- cbind(dd_num, dist_out)
colnames(dist_out) <- c("ID", "mean", "sd", "0.025quant", "0.5quant", "0.975quant")

#The district-level estimates will have the same ordering as in the shapefile if they have the same no of areas
write.csv(dist_out, paste0("adm1_estimates_HepB0_timely.csv"))


#Threshold calculations - 20%
ff1=function(x) length(which(x>=0.80))/nsamp
y.80 <- apply(inv.linpred.1, 1, ff1)   #Check me
ll=1:length(ind); ll[nonmiss] = y.80; ll[miss] = NA
rr.80 = raster(n25.p); values(rr.80) = ll
writeRaster(rr.80, paste0("inla_HepB0_timely_80perc_thresh.tif"), overwrite=TRUE)


#########################################################################################################################################
#A. #plot hepB0_delayed_1x1_pixel (inla_vax_HepB0_delayed_mean)

library(rasterVis)
library(viridisLite)

#1. read in tiff image (inla_vax_HepB0_delayed_mean)
hepB0pixel <- raster(paste0("inla_vax_HepB0_delayed_mean.tif"))

#2. actual plot
revMagma <- rasterTheme(region = rev(magma(10))) #this is needed to reverse the order of the levelplot default colour
levelplot(hepB0pixel, par.settings = revMagma, margin=FALSE, xlab = "", ylab = "") #margin=FALSE removes margin plot


tg <- textGrob("Modelled delayed HepB0 at 1km 1 km pixel", x = unit(0.08, "npc"), y = unit(0.87, "npc"), hjust = 0, gp = gpar(fontsize = 14))
grid.draw(tg)

#3. save the image as PNG using name (pixel_delayed_hepB0) with aspect ratio with = 1000 and height = 400


#########################################################################################
#B. #plot hepB0_timely_1x1_pixel (inla_vax_HepB0_timely_mean)
#1. read in tiff image (inla_vax_HepB0_timely_mean)
hepB0pixel_timely <- raster(paste0("inla_vax_HepB0_timely_mean.tif"))

#2. actual plot
#par(mar = c(1.5, 1, 1.5, 0.5)) # Set the margin on all sides (bottom, left, top, right)
#plot(hepB0pixel, col = terrain.colors(255), legend.args = list(text = 'Proportion delayed'), legend.shrink=1, main = "hepB0", cex = 0.6)

revViridis <- rasterTheme(region = rev(viridis(10))) #this is needed to reverse the order of the levelplot default colour
levelplot(hepB0pixel_timely, par.settings = revViridis, main = "HepB0 timely", margin=FALSE) #margin=FALSE removes margin plot

#3. save the image as PNG using name (pixel_timely_hepB0) with aspect ratio with = 1000 and height = 400







