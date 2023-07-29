
#Load these libraries
library(Metrics)
library(plyr)
library(xtable)
library(ggplot2)
library(reshape2)
library(MASS)
require(dplyr) 
library(car)
library(gtools)

set.seed(500) 

#Set working directory
setwd("filepath")


#Read in covariate and outcome data sets
cov_dat <- read.csv("Covariates_The Gambia_2016_2019.csv", header = TRUE) 
out_dat <- read.csv("outcome_data.csv", header=TRUE) 


head(out_dat)
head(cov_dat)



#Aggregate outcome data file to cluster level
out_dat1 <- group_by(out_dat, DHSCLUST) %>%
  summarize(total = n(), 
            total_penta3 = length(which(!is.na(penta3_timely))), 
            penta3_timely_count = sum(penta3_timely, na.rm = TRUE),
            penta3_timely_prop = penta3_timely_count/total_penta3,
            penta3_early_count = sum(penta3_early, na.rm = TRUE),
            penta3_early_prop = penta3_early_count/total_penta3,
            penta3_delayed_count = sum(penta3_delayed, na.rm = TRUE),
            penta3_delayed_prop = penta3_delayed_count/total_penta3)


#sd(out_dat1$penta3_timely_prop)
#sd(out_dat1$penta3_delayed_prop)
#sd(out_dat1$penta3_early_prop)


#Merge both the outcome and covariate data using the ID variable
#Keep all locations where covariate data are available
data.merge <- merge(cov_dat, out_dat1, by = "DHSCLUST", all.x = TRUE) 
head(data.merge)

#Names of covariates in the covariate data file
cov_names <- names(cov_dat)[-c(1:4)]

#Rows of data with at least one missing covariate value
del <- numeric(nrow(data.merge))
for (i in 1:nrow(data.merge)){
  if (any(is.na(data.merge[i,5:33]))) del[i] <- i
}
#Delete rows with missing covariate values
if (length(which(del!=0))>0) data.merge <- data.merge[-which(del!=0),]     

#Delete rows of data where no individual was sampled from the data frame
zero.clust <- which(is.na(data.merge$total_penta3)|data.merge$total_penta3<=1) #NOTE THAT CLUSTERS WITH SS<=1 HAVE BEEN DELETED
if (length(zero.clust)>0){
  data.merge <- data.merge[-zero.clust,]
}

#Outcome variable
total_sampled <- data.merge$total_penta3 # Total number of vaccinated individuals/children surveyed in each location
num_success   <- data.merge$penta3_delayed_count # Number who had delayed vaccination

#Extract lon-lat coordinates from data frame
dat.coord <- data.merge[,4:3]
#dat_coord <- dat.coord[,c(1,4,3)]
head(dat.coord)

#Data frame with covariates only 
covars <- data.merge[,c(5:33)]


###########################################################################

1. #Use histograms to determine which covariates to log-transform
#Ideally, the relationships between the covariates and the empirical 
#logit transform of the data shoudl be examined to ensure that the transformation
#improves the linearity

par(mfrow=c(2,5))
for (i in 1:10){
  hist(covars[,i], main = names(covars)[i])
}

windows()
par(mfrow=c(2,5))
for (i in 11:20){
  hist(covars[,i], main = names(covars)[i])
}

windows()
par(mfrow=c(2,5))
for (i in 21:29){
  hist(covars[,i], main = names(covars)[i])
}

#windows()
#par(mfrow=c(2,5))
#for (i in 31:40){
#  hist(covars[,i], main = names(covars)[i])
#}


#Take logs of heavily skewed covariates
#1,2,4,7,8,13:15,16,17,27
llog <- c(1,2,4,7,8,13:15,16,17,27)
for (i in 1:length(llog)){
  covars[,llog[i]] <- log(covars[,llog[i]] + 0.05) #0.05 added to avoid taking log of zero
  #if (i==4) covars[,llog[i]] <- log(covars[,llog[i]] + 4.0) 
  names(covars)[llog[i]] <- paste0("l_",names(covars)[llog[i]]) #Add "l" to the names of log-transformed covariates
}


#Linearity checks
out_emp_logit <- log(num_success + 0.5/(total_sampled - num_success + 0.5)) #Empirical logit transform

#Data frame for plotting
cdat <- covars
dd <- data.frame(out_emp_logit,  cdat)

meltdat=melt(dd, id.vars=c("out_emp_logit"))

#Linear regression lines
plotsingles1=ggplot(meltdat, aes(x=value, y=out_emp_logit, xlab="")) +
  geom_point()+
  geom_smooth(col="red", method = "loess", span=0.8)+  
  geom_smooth(method="lm", fill=NA)+ 
  #stat_smooth(method = "gam", formula = y ~ s(x),se=FALSE, col="green")+ #natural splines
  labs(x="", y="Empirical logit (penta3_delay)")+
  facet_wrap(~variable, scale="free_x", ncol = 5)+   #        #scale=free, free_x, free_y
  scale_y_continuous()+
  theme(panel.background=element_rect(fill="white", colour="black"))

plotsingles1

#Save plot 
pdf("cov_plot_emp_logit_penta3_delay.pdf", height = 7, width = 7, pointsize = 6)
plotsingles1
dev.off()

#Remove data sets not needed for further processing
rm(meltdat)
rm(cdat)
rm(dd)

#Remove some bad covariates
#covars <- covars[,-c(1,2)]


###Declare categorical covariates (e.g. urban-rural) as factors
covars <- data.frame(covars)
#covars <- within(covars, {
#  Urban_rural1 <- factor(Urban_rural1)
#})


#-------------------------Covariate selection starts from here--------------------#

###########################################################################

2. #Fit single covariate models and rank the covariates based on their predictive R-square values
#Create a new data frame for this exercise
Data <- cbind(total_sampled, num_success, covars)

covlist <- names(Data[,-1]) #List of covariates
covlist=covlist[-grep("num_success", covlist)]


##The function below fits single covariate models using a Monte Carlo cross-validation approach
#repeated n.iter times with 20% of the data used for validation each time.
#The validation statistics calculated are averaged over the n.iter repetitions in the end

AICs=dev=nulldev=n.par=r2=pr2=iter=numeric()
model=response=character()
n.iter=10
propsub=0.8
resp="cbind(num_success, total_sampled-num_success)"
resp1 = "num_success"
weights = "total_sampled"

for(i in 1:n.iter){
  subind=sample(1:nrow(Data), propsub*nrow(Data), replace=F)
  subdat=Data[subind,]
  preddat=Data[-subind,]
  ## the null model, for comparison
  nullmod=glm(formula(paste(resp, "~1")), data=Data, family = binomial(logit))
  for(j in covlist){
    form=formula(paste(resp, "~", j))
    pmod=glm(form, data=subdat, family = binomial(logit))
    fullmod=glm(form, data=Data, family = binomial(logit))
    ## pr2 checks the predictive power of the model against a 'new' subset of the data
    pr2=c(pr2,cor(pmod$fitted, subdat[[resp1]]/subdat[[weights]])^2) 
    ## AIC for the model on the full data
    AICs=c(AICs, AIC(fullmod))
    dev=c(dev, deviance(fullmod))
    n.par=c(n.par,1)
    iter=c(iter,i)
    r2=c(r2,cor(fullmod$fitted, Data[[resp1]]/Data[[weights]])^2) 
    model=c(model,j)
    response=c(response, resp1)
    #print(paste(i, j, sep="X"))
  }
}
op=data.frame(model, response, dev, AICs, r2, pr2, n.par )
singles=ddply(op, .(model), summarise,
              model=model[1],
              response=response[1],
              dev=dev[1],
              pr2=mean(pr2),
              AICs=AICs[1],
              r2=r2[1])

### Add deviance reduction
singles$devred=singles$dev/deviance(nullmod)

## Order by pr2 - predictive R-squared
singles=singles[order(singles$pr2, decreasing=TRUE),]

#Ranks
singles <- within(singles, ranks <- 1:nrow(singles))
singles

#Covariates and ranks
cov.rank <- data.frame(covariate=as.character(singles$model), ranks = singles$ranks)

#NB: You coudl decide to exclude some covariates that didn't perform well after this step
#But for this exercise, we will keep all of them

###
#Separate categorical covariates before this step
#Keep old covars data frame
covars.old <- covars  
#Remove urban-rural covariate
#covars     <- covars[,-15]  



###########################################################################
3. ##Detection of multicollinearity through correlations between covariates and VIF analysis
#Correlations
#Determine correlations between the covariates and extract highly correlated pairs
#for screening and elimination. 
#Flag covariate pairs with correlations >= 0.8   #change to a higher value if necessary
corrs <- cor(covars[,-1])
bigcors <- matrix(0,1, 2) 
for (i in 2: nrow(corrs)){
  for (j in 1:(i-1)){
    if (abs(corrs[i,j])>=0.8) bigcors <- rbind(bigcors, c(i,j))  #Note 0.8 cut-off
  }
}

bigcors <- bigcors[-1,]
bigcors.dat <- matrix(0, nrow(bigcors), 3)
for (i in 1:nrow(bigcors)){
  bigcors.dat[i,] <- c(rownames(corrs)[bigcors[i,1]],colnames(corrs)[bigcors[i,2]], 
                       round(corrs[bigcors[i,1],bigcors[i,2]],3))
}

#Pairs of covariates with high correlations
bigcors.dat

#Select between pairs of highly-correlated covariates using their ranks
all.covs <-rownames(corrs)
for (i in 1:nrow(bigcors.dat)){
  #print(i)
  name.cov <- bigcors.dat[i,1:2]
  if (name.cov[1]%in%all.covs && name.cov[2]%in%all.covs){
    r1 <- which(cov.rank$covariate==name.cov[1])
    r2 <- which(cov.rank$covariate==name.cov[2])
    if (r1>r2) all.covs <- all.covs[-which(all.covs==name.cov[2])]
    if (r2>r1) all.covs <- all.covs[-which(all.covs==name.cov[1])]
  }
}

#Check that all remaining covariates are not highly correlated
correl  <- cor(covars[,all.covs])
correl #Correlation matrix

#Create new data frame containing only "uncorrelated" covariates
covars.1a      <- covars[,all.covs]

#Reconstitute "Data" to include categorical covariates (urban-rural) 
urban_rural <- data.merge$rural_urba
urban_rural1 <- numeric(length(urban_rural))
urban_rural1[urban_rural=="Urban"] <- 1

#all.covs1  <- c(all.covs,names(covars.old)[15])
covars.1  <- data.frame(covars.1a, urban_rural = urban_rural1)
Data <- cbind(total_sampled, num_success, covars.1)

#Define categorical covariates as factors
Data <- data.frame(Data)
Data <- within(Data, {
  urban_rural1 <- factor(urban_rural)
})

#VIF analysis using all the remaining covariates
covnames <- as.character(names(covars.1))

form <- paste("cbind(num_success, total_sampled - num_success)","~", paste(covnames, collapse=" + "))
mod.vif <- glm(form, data=Data, family = binomial(logit))
summary(mod.vif)
vif(mod.vif)



###########################################################################

4. ########################## Method 1
#StepAIC regression - k=2 penatly
n <- nrow(Data)
form <- paste("cbind(num_success, total_sampled - num_success)","~", paste(covnames, collapse=" + "))
fit  <- glm(form, data = Data, family = binomial(logit))
mod.step1 <- stepAIC(fit, trace=FALSE, direction = "backward") #Note penalty  
summary(mod.step1) #AIC: 3791.1
vif(mod.step1)


4. ########################## Method 2
#StepAIC regression - k=log(n) BIC penalty
fit  <- glm(form, data = Data, family = binomial(logit))
mod.step2 <- stepAIC(fit, trace=FALSE, direction = "backward", k = log(n)) #Note penalty
summary(mod.step2) 
vif(mod.step2)



##Selected covariates using BIC penalty

GMB_n42_elevation_model                         GMB_n94_avg_number_WetDays_2016_2019 
1.695116                                                     3.336547 
GMB_n95_proximity_national_borders                        GMB_n98_avg_modis_NDVI_2016.2019_mean 
1.588760                                                     2.075200 
GMB_n99_avg_modis_Night_time_landsurface_temp_2016.2019_mean     GMB_n102_avg_potential_evapotranspiration_2016_2019_mean 
5.253362                                                    10.575995

#Write selected covariate information for the next step of the analysis
covout <- c("GMB_n42_elevation_model",
            "GMB_n94_avg_number_WetDays_2016_2019",
            "GMB_n95_proximity_national_borders",
            "GMB_n98_avg_modis_NDVI_2016.2019_mean",
            "GMB_n99_avg_modis_Night_time_landsurface_temp_2016.2019_mean",
            "GMB_n102_avg_potential_evapotranspiration_2016_2019_mean",
            "urban_rural")

Data.out <- Data[,covout]
Data.out$DHSCLUST <- data.merge$DHSCLUST
Data.out1 <- data.frame(Data.out, dat.coord)

write.csv(Data.out1, "penta3_delayed_covariates_selected.csv")