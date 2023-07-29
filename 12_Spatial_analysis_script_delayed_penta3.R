
library(INLA)
#INLA:::inla.dynload.workaround()
library(raster); library(maptools)
library(gtools); library(sp); library(spdep)
library(rgdal)
library(ggplot2)
library(dplyr)
library(ggsci)


#Set working directory
setwd("filepath")

filePathData <- "filepath0"
filePathData1 <- "filepath1"
filePathData2 <- "filepath2"

#loading the data
dat <- read.csv("outcome_data.csv", header=TRUE)
dat.cov <- read.csv(paste0(filePathData, "penta3_delayed_covariates_selected.csv"), header=TRUE)

#Aggregate outcome data file to cluster level
dat1 <- group_by(dat, DHSCLUST) %>%
  summarize(total = n(), 
            total_penta3 = length(which(!is.na(penta3_timely))), 
            penta3_timely_count = sum(penta3_timely, na.rm = TRUE),
            penta3_timely_prop = penta3_timely_count/total_penta3,
            penta3_delayed_count = sum(penta3_delayed, na.rm = TRUE),
            penta3_delayed_prop = penta3_delayed_count/total_penta3,
            penta3_early_count = sum(penta3_early, na.rm = TRUE),
            penta3_early_prop = penta3_early_count/total_penta3)


data.merge <- merge(dat1, dat.cov, by="DHSCLUST")
coords <- data.frame(LONGNUM = data.merge$longitude, LATNUM = data.merge$latitude)

head(data.merge)

vaxcov  <- data.merge[,10:16]   #includes coordinates as Longitude & Latitude
vaxdata <- data.merge[,c(2:9,17:18)]

#################################################################################################
##Create dataset to plot cluster level timeliness
clustplot <- vaxdata %>% 
  mutate(penta3_delayed_prop = penta3_delayed_prop*100) %>% 
  select(penta3_delayed_prop, longitude, latitude) %>% 
  rename(outcome = penta3_delayed_prop)

##Create dataset for clusterplot of penta3_early
clustplot_early <- vaxdata %>% 
  mutate(penta3_early_prop = penta3_early_prop*100) %>% 
  select(penta3_early_prop, longitude, latitude) %>% 
  rename(outcome = penta3_early_prop)


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

# Define the mid value
mid <- median(clustplot_early$outcome)

##plot spatial distribution of early penta3 in The Gambia
#scale_colour_gradientn(colours = terrain.colors(10), name="% delayed")# another way of changing colours

ggplot(gmb_shp) + 
  geom_sf() + 
  geom_point(data = clustplot_early, aes(x=longitude, y=latitude, colour=outcome), size=2)+
  scale_color_gradient2(midpoint=mid, low="green", mid="gold", high="brown", name="Early (%)")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title = element_blank())+
  labs(title="Cluster-level observed early PENTA3")


##plot spatial distribution of delayed penta3 in The Gambia
ggplot(gmb_shp) + 
  geom_sf() + 
  geom_point(data = clustplot, aes(x=longitude, y=latitude, colour=outcome), size=2)+
  scale_colour_viridis_c(option = "inferno", direction = -1, name="% delayed")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title = element_blank())+
  labs(title="Cluster-level observed deleyad PENTA3")


ggplot() +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM2_EN",], color = "white", size = 0.8) +
  geom_sf(data = gmb_combined[gmb_combined$type == "ADM1_EN",], aes(fill = ADM1_EN), color = "black", size = 1, alpha = 0.1, show.legend = FALSE) + 
  geom_point(data = clustplot, aes(x=longitude, y=latitude, colour=outcome), size=2)+
  scale_colour_viridis_c(option = "inferno", direction = -1, name="Delayed (%)")+
  scale_fill_viridis_d(name = "ADM1_EN", option = "inferno", direction = -1, alpha = 0.8) +
  theme_bw()+
  theme(legend.key.height = unit(1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title = element_blank())+
  labs(title="Cluster-level observed deleyad PENTA3", fill = "ADM1_EN") +
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

#3. save the image as PNG using name (clustplot_delayed_penta3 and clustplot_early_penta3) with aspect ratio with = 1000 and height = 400
######################################################################################################################################


#Delete clusters where TotChild is zero from vaxdata only
zero.clust <- which(is.na(vaxdata$total_penta3)|vaxdata$total_penta3<=1)
if (length(zero.clust)>0){
  vaxdata <- vaxdata[-zero.clust,]
  vaxcov  <- vaxcov[-zero.clust,]
}

#All_ages
Numvacc    <- vaxdata$penta3_delayed_count
weights    <- vaxdata$total_penta3

#Coordinates
coords    <- cbind(vaxdata$longitude,vaxdata$latitude)
min.dist  <- as.numeric(summary(dist(coords))[1]) #Min of distances between cluster locations #0.0003146808

set.seed(500)

#Covariates
xp1     <- vaxcov$GMB_n42_elevation_model
xp2     <- vaxcov$GMB_n94_avg_number_WetDays_2016_2019
xp3	    <- vaxcov$GMB_n95_proximity_national_borders
xp4	    <- vaxcov$GMB_n98_avg_modis_NDVI_2016.2019_mean
xp5	    <- vaxcov$GMB_n99_avg_modis_Night_time_landsurface_temp_2016.2019_mean
xp6     <- vaxcov$GMB_n102_avg_potential_evapotranspiration_2016_2019_mean
xp7     <- vaxcov$urban_rural


#Read in prediction covariates
GMB_n42  	<- raster(paste0(filePathData1,"GMB_n42_elevation_model.tif"))
GMB_n94  	<- raster(paste0(filePathData1,"GMB_n94_avg_number_WetDays_2016_2019.tif")) 
GMB_n95 	<- raster(paste0(filePathData1,"GMB_n95_proximity_national_borders.tif"))
GMB_n98  	<- raster(paste0(filePathData1,"GMB_n98_avg_modis_NDVI_2016-2019_mean.tif"))
GMB_n99  	<- raster("../000_covariates for modelling/GMB_n99_avg_modis_Night_time_landsurface_temp_2016-2019_mean.tif")
GMB_n102	<- raster(paste0(filePathData1,"GMB_n102_avg_potential_evapotranspiration_2016_2019_mean.tif"))
GMB_urban_rural	<- raster(paste0(filePathData1,"gmb_urban_rural_1km.tif"))


#Prediction covariates
x1gp  	<- getValues(GMB_n42)
x2gp 	<- getValues(GMB_n94)
x3gp 	<- getValues(GMB_n95)   			
x4gp	<- getValues(GMB_n98) 
x5gp	<- getValues(GMB_n99) 
x6gp	<- getValues(GMB_n102)
x7gp	<- getValues(GMB_urban_rural) 


#Prediction grid
n25.p      <- raster(paste0(filePathData1,"GMB_n42_elevation_model.tif"))
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
colnames(Xpred) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7_1") #
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

write.csv(param.all, paste0("parameter_output_penta3_delayed.csv"))


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
write.csv(ds, paste0("obsvpred_penta3_delayed.csv"))


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
save(inv.linpred, file=paste0("simprob_penta3_delayed.rda"))


####IMMEIATELY RUN THE "penta3_timely_early script to generate admin level estimates and rasters for delayed.


