
library(INLA)
library(raster); library(maptools)
library(gtools); library(sp); library(spdep)
library(rgdal)
library(ggplot2)
#library(Hmisc)

library(gstat)
require(dplyr) 


#Set working directory
setwd("filepath")

#loading the data
dat      <- read.csv("outcome_data.csv", header=TRUE)
dat.cov  <- read.csv("penta3_delayed_covariates_selected.csv", header=TRUE)

#Aggregate outcome data file to cluster level
dat1 <- group_by(dat, DHSCLUST) %>%
  summarize(total = n(), 
            total_penta3 = length(which(!is.na(penta3_timely))), 
            penta3_timely_count = sum(penta3_timely, na.rm = TRUE),
            penta3_timely_prop = penta3_timely_count/total_penta3,
            penta3_delayed_count = sum(penta3_delayed, na.rm = TRUE),
            penta3_delayed_prop = penta3_delayed_count/total_penta3)


data.merge <- merge(dat1, dat.cov, by="DHSCLUST")
coords <- data.frame(LONGNUM = data.merge$longitude, LATNUM = data.merge$latitude)


#Fit variogram using gstat package

#Fit a non-spatial binomial regression model to extract the residuals
#Covariate names
covnames <- names(data.merge)[8:14]

# Observation points
X0 <- model.matrix(~ -1 + GMB_n42_elevation_model + 
                     GMB_n94_avg_number_WetDays_2016_2019 + 
                     GMB_n95_proximity_national_borders + 
                     GMB_n98_avg_modis_NDVI_2016.2019_mean + 
                     GMB_n99_avg_modis_Night_time_landsurface_temp_2016.2019_mean + 
                     GMB_n102_avg_potential_evapotranspiration_2016_2019_mean + 
                     factor(urban_rural), data = data.merge)

Xobs <-  as.data.frame(X0[,-which(colnames(X0)%in%c("factor(urban_rural)0"))])

#Data
data <- data.frame(penta3_delayed_count = data.merge$penta3_delayed_count, total_penta3 = data.merge$total_penta3, Xobs,
                   DHSCLUST = data.merge$DHSCLUST)

hyper=list(prec=list(prior="loggamma",param=c(1, 0.001)))
#hyper=list(prec=list(prior="loggamma",param=c(0.1, 0.1)))

form.inla = penta3_delayed_count ~ 1 + GMB_n42_elevation_model + 
  GMB_n94_avg_number_WetDays_2016_2019 + 
  GMB_n95_proximity_national_borders + 
  GMB_n98_avg_modis_NDVI_2016.2019_mean + 
  GMB_n99_avg_modis_Night_time_landsurface_temp_2016.2019_mean + 
  GMB_n102_avg_potential_evapotranspiration_2016_2019_mean + factor.urban_rural.1 + f(DHSCLUST, model = "iid", hyper = hyper)

inla.mod <- inla(form.inla, data = data.frame(data), family = "binomial", Ntrials = total_penta3,
                 control.compute = list(dic = TRUE, waic = TRUE), control.predictor = list(compute = TRUE, link=1))
summary(inla.mod)


names(inla.mod$marginals.random)

rr <- inla.mod$summary.random$DHSCLUST$mean


#Extract the residuals
data.merge$residuals <- rr #Residuals


#Calculate empirical variogram using the residuals
vgm1 <- variogram(residuals~1, data = data.merge, ~longitude+latitude, cutoff = 1.5)  
plot(vgm1) #Empirical variogram

#Fit a variogram model - Mat, Exp, Sph, etc
#Initial values for the parameters of the model can be provided tas well. These parameters
#are estimated using the specified model if not provided
#foo <- fit.variogram(vgm1, vgm("Exp"))
foo <- fit.variogram(vgm1, vgm("Mat", fit.kappa = TRUE))
#foo <- fit.variogram(vgm1, vgm("Sph"))
#foo1 <- fit.variogram(vgm1, vgm(0.4768, "Exp", 0.5627, nugget=0.5))

#View estimated parameters
foo

#Plot fitted variogram model
plot(vgm1, model=foo) #variogram + fitted line

#Plot empirical variogram, variogram fit as well as estimated parameters
plot(vgm1$dist, vgm1$gamma, ylim=c(0,0.015), col = "cornflowerblue", xlab = "distance", ylab = "semivariance")
lines(variogramLine(vgm(psill = foo$psill[2], "Mat", range = foo$range[2], 
                        kappa = foo$kappa[2], nugget = foo$psill[1]), 10), type = 'l') #psill, model, range, nugget
abline(h = foo$psill[1], col = "blue")
abline(v = foo$range[2], col = "red")
abline(h = foo$psill[1] + foo$psill[2], col = "green")


#Allocate the residuals at random to the locations and plot the empirical variogram
data.merge$residuals.rand <- sample(data.merge$residuals, nrow(data.merge), replace = FALSE)

#Variogram - observe that the variogram is no longer an increasing function of distance
vgm1 <- variogram(residuals.rand~1, data = data.merge, ~longitude+latitude)  #Empirical variogram
plot(vgm1) #Empirical variogram plot
####################################################################################################################
















