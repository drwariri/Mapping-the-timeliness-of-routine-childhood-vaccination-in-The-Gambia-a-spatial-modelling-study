
library(INLA)
#INLA:::inla.dynload.workaround()
library(raster); library(maptools)
library(gtools); library(sp); library(spdep)
library(rgdal)
library(ggplot2)
library(dplyr)
#library(Hmisc)

#Set working directory
setwd("filepath")

filePathData <- "filepath0"
filePathData1 <- "filepath1"
filePathData2 <- "filepath2"

#loading the data
dat <- read.csv("outcome_data.csv", header=TRUE)
dat.cov <- read.csv(paste0(filePathData, "mcv1_timely_covariates_selected.csv"), header=TRUE)


#Aggregate outcome data file to cluster level
dat1 <- group_by(dat, DHSCLUST) %>%
  summarize(total = n(), 
            total_mcv1 = length(which(!is.na(mcv1_timely))), 
            mcv1_timely_count = sum(mcv1_timely, na.rm = TRUE),
            mcv1_timely_prop = mcv1_timely_count/total_mcv1,
            mcv1_delayed_count = sum(mcv1_delayed, na.rm = TRUE),
            mcv1_delayed_prop = mcv1_delayed_count/total_mcv1)


data.merge <- merge(dat1, dat.cov, by="DHSCLUST")
coords <- data.frame(LONGNUM = data.merge$longitude, LATNUM = data.merge$latitude)

head(data.merge)

vaxcov  <- data.merge[,8:12]   #includes coordinates as Longitude & Latitude
vaxdata <- data.merge[,c(2:7,13:14)]

#################################################################################################
##Create dataset to plot cluster level timeliness
clustplot <- vaxdata %>% 
  mutate(mcv1_timely_prop = mcv1_timely_prop*100) %>% 
  select(mcv1_timely_prop, longitude, latitude) %>% 
  rename(outcome = mcv1_timely_prop)

#read in gambia shapefile
gmb_shp <- st_read(paste0(filePathData2,"gmb_admbnda_adm2_2022.shp"))

##plot spatial distribution of timely penta1-penta2 in The Gambia

ggplot(gmb_shp) + 
  geom_sf() + 
  geom_point(data = clustplot, aes(x=longitude, y=latitude, colour=outcome), size=2)+
  scale_colour_viridis_c(direction = -1, name="% timely")+
  theme_bw()+
  theme(legend.key.height = unit(1.1, 'cm'), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  labs(title="mcv1")

#3. save the image as PNG using name (clustplot_timely_mcv1) with aspect ratio with = 1000 and height = 400
######################################################################################################################################


#Delete clusters where TotChild is zero from vaxdata only
zero.clust <- which(is.na(vaxdata$total_mcv1)|vaxdata$total_mcv1<=1)
if (length(zero.clust)>0){
  vaxdata <- vaxdata[-zero.clust,]
  vaxcov  <- vaxcov[-zero.clust,]
}

#All_ages
Numvacc    <- vaxdata$mcv1_timely_count
weights    <- vaxdata$total_mcv1

#Coordinates
coords    <- cbind(vaxdata$longitude,vaxdata$latitude)
min.dist  <- as.numeric(summary(dist(coords))[1]) #Min of distances between cluster locations #0.0003146808

set.seed(500)

#Covariates
xp1     <- vaxcov$GMB_n94_avg_number_WetDays_2016_2019
xp2     <- vaxcov$GMB_n95_proximity_national_borders
xp3     <- vaxcov$GMB_n99_avg_modis_Night_time_landsurface_temp_2016.2019_mean
xp4	    <- vaxcov$GMB_n102_avg_potential_evapotranspiration_2016_2019_mean
xp5     <- vaxcov$urban_rural


#Read in prediction covariates
GMB_n94  	<- raster(paste0(filePathData1,"GMB_n94_avg_number_WetDays_2016_2019.tif"))
GMB_n95  	<- raster(paste0(filePathData1,"GMB_n95_proximity_national_borders.tif"))
GMB_n99  	<- raster("../000_covariates for modelling/GMB_n99_avg_modis_Night_time_landsurface_temp_2016-2019_mean.tif") 
GMB_n102 	<- raster(paste0(filePathData1,"GMB_n102_avg_potential_evapotranspiration_2016_2019_mean.tif"))
GMB_urban_rural	<- raster(paste0(filePathData1,"gmb_urban_rural_1km.tif"))


#Prediction covariates
x1gp 	<- getValues(GMB_n94) 
x2gp 	<- getValues(GMB_n95) 
x3gp 	<- getValues(GMB_n99)  
x4gp 	<- getValues(GMB_n102)
x5gp	<- getValues(GMB_urban_rural) 


#Prediction grid
n25.p      <- raster(paste0(filePathData1,"GMB_n94_avg_number_WetDays_2016_2019.tif"))
Pred_grid2 <- coordinates(n25.p)

#Population data for population-weighted aggregation
popc     <- raster(paste0(filePathData1,"GMB_n106_population_under_1_years_old_pop-2019.tif"))
popc	 <- getValues(popc) 

#Combine grid and covariates
pred.dat <- cbind(Pred_grid2, x1gp, x2gp, x3gp, x4gp, x5gp, popc) # 

ind <- apply(pred.dat, 1, function(x) any(is.na(x)))
miss    <- which(ind==TRUE)
nonmiss <- which(ind==FALSE)

pred.dat.1 <- pred.dat[nonmiss, ]
pop <- pred.dat.1[,8]              #Population counts for weighted aggregation
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
X0 <- model.matrix(~ -1 + xp1 + xp2 + xp3 + xp4 + factor(xp5))
Xobs <-  as.data.frame(X0[,-which(colnames(X0)%in%c("factor(xp5)0"))])
colnames(Xobs) <- c("x1", "x2", "x3", "x4", "x5_1") # 
Ap.i <- inla.spde.make.A(mesh=meshfit, loc=coords)
lp.i = rep(1,length(xp1))
stk.point <- inla.stack(tag='point',
                        data=list(y=Numvacc,n=weights),
                        A=list(Ap.i,1,1,1),
                        effects=list(s=1:spde$n.spde, rr=1:length(weights), intercept=rep(1, length(xp1)), Xobs))  #NOTE

# Prediction points - can be moved outside the loop
xpred1 <- pred.dat.1[,3]; xpred2 <- pred.dat.1[,4]; xpred3 <- pred.dat.1[,5]; xpred4 <- pred.dat.1[,6]; xpred5 <- pred.dat.1[,7]
X0 <- model.matrix(~ -1 + xpred1 + xpred2 + xpred3 + xpred4 + factor(xpred5))
#
Xpred <-  as.data.frame(X0[,-which(colnames(X0)%in%c("factor(xpred5)0"))])
colnames(Xpred) <- c("x1", "x2", "x3", "x4", "x5_1") #
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
formula  <- y ~ -1 + intercept + x1 + x2 + x3 + x4 + x5_1 + f(s, model=spde) + f(rr, model="iid", hyper = hyper.prec) #
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

write.csv(param.all, paste0("parameter_output_mcv1_timely.csv"))


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
write.csv(ds, paste0("obsvpred_output_mcv1_timely.csv"))


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
idX <- contents$start[which(contents$tag=="intercept")]-1 + (1:6) # fixed effects, 6 = no of regression coefficients

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

linpred     <- as.matrix(Apred %*% xSpace + as.matrix(cbind(1, Xpred$x1, Xpred$x2, Xpred$x3, Xpred$x4, Xpred$x5_1)) %*% xX + sample.IIDpred)  #
inv.linpred <- inv.logit(linpred) 
inv.linpred <- inv.linpred*100

#Save draws for calculating remaining two indicators
save(inv.linpred, file=paste0("simprob_output_mcv1_timely.rda"))


###############################################################################################################################################
##########################To calculate early MCV1 and adjust timely MCV1 and delayed MCV1 and write adjusted outputs

inv.linpred_timely <- inv.linpred

load(file=paste0("simprob_mcv1_delayed.rda"))
inv.linpred_delayed <- inv.linpred

#inv.linpred_early <- 100 - (inv.linpred_timely + inv.linpred_delayed)
inv.linpred_early <- matrix(0, nrow(inv.linpred_timely), ncol(inv.linpred_timely))
inv.linpred_timely_new <- inv.linpred_timely
inv.linpred_delayed_new <- inv.linpred_delayed
for (i in 1: nrow(inv.linpred_timely)){
  for (j in 1:ncol(inv.linpred_timely)){
    uu <- inv.linpred_timely[i,j] + inv.linpred_delayed[i,j]
    if (uu > 100) {
      diff <- (uu - 100)/2 
      inv.linpred_timely_new[i,j] <- inv.linpred_timely[i,j] - diff
      inv.linpred_delayed_new[i,j] <- inv.linpred_delayed[i,j] - diff
      inv.linpred_early[i,j] <- 0
    }
    if (uu <= 100){ 
      inv.linpred_early[i,j] <- 100 - (inv.linpred_timely[i,j] + inv.linpred_delayed[i,j])
    }
  }
}


#########Run step by step

#RUN 1
ind.current <- "early"
inv.linpred <- inv.linpred_early

#RUN2
ind.current <- "timely"
inv.linpred <- inv.linpred_timely_new

#RUN 3
ind.current <- "delayed"
inv.linpred <- inv.linpred_delayed_new


pred.grid    <- data.frame(t(apply(inv.linpred, 1, FUN=function(x){ c(mean(x), sd(x), quantile(x, probs=c(0.025,0.5,0.975)))}))) 
colnames(pred.grid) <- c("mean", "sd", "0.025quant", "0.5quant", "0.975quant")
fitted.pred.mean   <- as.vector(data.matrix(as.vector(pred.grid[,"mean"])))
fitted.pred.sd     <- as.vector(data.matrix(as.vector(pred.grid[,"sd"])))
fitted.pred.median <- as.vector(data.matrix(as.vector(pred.grid[,"0.5quant"])))
fitted.pred.low    <- as.vector(data.matrix(as.vector(pred.grid[,"0.025quant"])))
fitted.pred.up     <- as.vector(data.matrix(as.vector(pred.grid[,"0.975quant"])))

n25.p = raster(paste0(filePathData1,"GMB_n94_avg_number_WetDays_2016_2019.tif"))

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

writeRaster(rr.mean, paste0("inla_vax_mcv1_", ind.current, "_mean_adj.tif"), overwrite=TRUE)
writeRaster(rr.sd,   paste0("inla_vax_mcv1_", ind.current, "_sd_adj.tif"), overwrite=TRUE)
writeRaster(rr.low,  paste0("inla_vax_mcv1_", ind.current, "_low_adj.tif"), overwrite=TRUE)
writeRaster(rr.up,   paste0("inla_vax_mcv1_", ind.current, "_up_adj.tif"), overwrite=TRUE)
writeRaster(rr.med,  paste0("inla_vax_mcv1_", ind.current, "_median_adj.tif"), overwrite=TRUE)



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
write.csv(dist_out, paste0("adm3_estimates_mcv1", ind.current, "_adj.csv"))


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
write.csv(dist_out, paste0("adm2_estimates_mcv1_", ind.current, "_adj.csv"))


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
write.csv(dist_out, paste0("adm1_estimates_mcv1_", ind.current, "_adj.csv"))


#Threshold calculations - 50%
ff1=function(x) length(which(x>=0.50))/nsamp
y.50 <- apply(inv.linpred_early, 1, ff1)   #Check me
ll=1:length(ind); ll[nonmiss] = y.50; ll[miss] = NA
rr.50 = raster(n25.p); values(rr.50) = ll
writeRaster(rr.50, paste0("inla_mcv1_", ind.current, "_50perc_thresh_adj.tif"), overwrite=TRUE)


#########################################################################################################################################

####Calculation the numbers of children per admin-level with delayed vaccination# THIS PART CAN ONLY BE RUN IMMEDIATELY AFTER "RUN 3"

popc     <- raster(paste0(filePathData1,"GMB_n106_population_under_1_years_old_pop-2019.tif")) #reads in the raster of under-1 population
num.delayed.adm3 <- extract(rr.mean*popc, spol1, fun=sum, na.rm=TRUE)
num.delayed.adm2 <- extract(rr.mean*popc, spol2, fun=sum, na.rm=TRUE)
num.delayed.adm1 <- extract(rr.mean*popc, spol3, fun=sum, na.rm=TRUE)

totchild.adm3 <- extract(popc, spol1, fun=sum, na.rm=TRUE)
totchild.adm2 <- extract(popc, spol2, fun=sum, na.rm=TRUE)
totchild.adm1 <- extract(popc, spol3, fun=sum, na.rm=TRUE)

write.csv(data.frame(ID = 1:nrow(spol1), num.delayed.adm3, totchild = totchild.adm3), "est_num_delayed_mcv1_adm3.csv")
write.csv(data.frame(ID = 1:nrow(spol2), num.delayed.adm2, totchild = totchild.adm2), "est_num_delayed_mcv1_adm2.csv")
write.csv(data.frame(ID = 1:nrow(spol3), num.delayed.adm1, totchild = totchild.adm1), "est_num_delayed_mcv1_adm1.csv")


#########################################################################################################################################

#########################################################################################################################################

#plot mcv1_timely_interval_1x1_pixel (inla_vax_mcv1_timely_mean)

library(rasterVis)
library(viridisLite)
library(grid)

#DELAYED
#1. read in tiff image (inla_vax_mcv1_delayed_mean)
mcv1pixel <- raster(paste0("inla_vax_mcv1_delayed_mean_adj.tif"))

#2. actual plot
revMagma <- rasterTheme(region = rev(magma(10))) #this is needed to reverse the order of the levelplot default colour
levelplot(mcv1pixel, par.settings = revMagma, margin=FALSE, xlab = "", ylab = "") #margin=FALSE removes margin plot

tg <- textGrob("Modelled delayed MCV1 at 1km 1 km pixel", x = unit(0.08, "npc"), y = unit(0.87, "npc"), hjust = 0, gp = gpar(fontsize = 14))
grid.draw(tg)

#3. save the image as PNG using name (pixel_delayed_mcv1) with aspect ratio with = 1000 and height = 400

#########################################################################################

#TIMELY
#1. read in tiff image (inla_vax_mcv1_timely_mean)
mcv1_timelypixel <- raster(paste0("inla_vax_mcv1_timely_mean_adj.tif"))

#2. actual plot
#par(mar = c(1.5, 1, 1.5, 0.5)) # Set the margin on all sides (bottom, left, top, right)
#plot(hepB0pixel, col = terrain.colors(255), legend.args = list(text = 'Proportion delayed'), legend.shrink=1, main = "hepB0", cex = 0.6)

revViridis <- rasterTheme(region = rev(viridis(10))) #this is needed to reverse the order of the levelplot default colour
levelplot(mcv1_timelypixel, par.settings = revViridis, main = "MCV1 timely", margin=FALSE) #margin=FALSE removes margin plot

#3. save the image as PNG using name (pixel_timely_mcv1) with aspect ratio with = 1000 and height = 400

#########################################################################################

#EARLY
#1. read in tiff image (inla_vax_mcv1_early_mean)
mcv1_earlypixel <- raster(paste0("inla_vax_mcv1_early_mean_adj.tif"))

#2. actual plot
#par(mar = c(1.5, 1, 1.5, 0.5)) # Set the margin on all sides (bottom, left, top, right)
#plot(hepB0pixel, col = terrain.colors(255), legend.args = list(text = 'Proportion delayed'), legend.shrink=1, main = "hepB0", cex = 0.6)

# make the topo colour scale
clrtopo <- colorRampPalette(c("olivedrab2", "darkgoldenrod2", 
                              "darkgoldenrod4", "chocolate4"))
levelplot(mcv1_earlypixel, col.regions = clrtopo(16), margin=FALSE, xlab = "", ylab = "") #margin=FALSE removes margin plot

tg <- textGrob("Modelled early MCV1 at 1km 1 km pixel", x = unit(0.08, "npc"), y = unit(0.87, "npc"), hjust = 0, gp = gpar(fontsize = 14))
grid.draw(tg)

#3. save the image as PNG using name (pixel_early_mcv1) with aspect ratio with = 1000 and height = 400









