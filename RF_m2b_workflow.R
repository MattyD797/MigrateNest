# Date April 7 2021
# Author Luke Wilde
#Script to build random forest model on GPS tagged BTGO to discern nesting from non-nesting

#variables of interest are: step length, turning angle, distance to water, revisit, residence Time, 
#other variables could be: days since arrival, days before departure, sliding window of variance, standard deviation (to capture autocorrelation)

# -1 = no observation
# 1 = incubating
# 2 = foraging 
# 3 = dead bird
# 4 = chick tending
# 5 = migrating



#create function to load and install (missing) packages

#remove all objects, clean environment
rm(list=ls(all=TRUE))

#install.packages("m2b")
foo <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

foo(c("randomForest", "m2b", "moveHMM", "momentuHMM", "dplyr","tidyverse", "caret","mlbench", "nestr", "coda", "jagsUI", "R2jags", "runjags", "rjags"))

options(max.print = 99)

as.POSIXct(Sys.time(), origin = "1970-01-01")

setwd("C:/Users/14064/Dropbox/BTGO Movmement Study/MigrateNest/")
files <- list.files("RF tracks")

myfiles <- lapply(paste0("./RF tracks/",files[1:25]), read.csv)

####train RF model####
data <- rbind(myfiles[[6]], myfiles[[7]], myfiles[[8]], myfiles[[9]], myfiles[[10]], myfiles[[11]], myfiles[[12]], myfiles[[13]], myfiles[[14]], myfiles[[15]], myfiles[[16]], myfiles[[17]], myfiles[[18]], myfiles[[19]], myfiles[[20]], myfiles[[21]], myfiles[[22]], myfiles[[23]], myfiles[[24]], myfiles[[25]])

#,myfiles[[9]]

str(data)
data_1 <- data[,c(4,3,6,5,1)]; str(data_1); head(data_1)

names(data_1)[1] <- "x"
names(data_1)[2] <- "y"
names(data_1)[3] <- "t"
names(data_1)[4] <- "b"
names(data_1)[5] <- "id"

str(data_1)
head(data_1);tail(data_1)

data_1$x <- as.numeric(data_1$x)
data_1$y <- -1*as.numeric(data_1$y)
data_1$b <- as.character(data_1$b)
data_1$id <- as.character(data_1$id)
data_1$t <- as.POSIXct(data_1$t, format = "%m/%d/%Y %H:%M:%S", origin = "1970-01-01")
str(data_1); head(data_1); tail(data_1)
unique(data_1$id)

data_1 <- data_1[complete.cases(data_1),]

#call test data, using similar to above
test.data <- rbind(myfiles[[1]],myfiles[[2]],myfiles[[3]],myfiles[[4]],myfiles[[5]])

#process as before, convert to xytb object
str(test.data)
test.data_1 <- test.data[,c(4,3,6,5,1)]; str(test.data_1); head(test.data_1)

names(test.data_1)[1] <- "x"
names(test.data_1)[2] <- "y"
names(test.data_1)[3] <- "t"
names(test.data_1)[4] <- "b"
names(test.data_1)[5] <- "id"

str(test.data_1)
head(test.data_1);tail(test.data_1)

test.data_1$x <- as.numeric(test.data_1$x)
test.data_1$y <- -1*as.numeric(test.data_1$y)
test.data_1$b <- as.character(test.data_1$b)
test.data_1$id <- as.character(test.data_1$id)
test.data_1$t <- as.POSIXct(test.data_1$t, format = "%m/%d/%Y %H:%M", origin = "1970-01-01") #for some reason, the t column lost the seconds

str(test.data_1); head(test.data_1); tail(test.data_1)
unique(test.data_1$id)

test.data_1 <- test.data_1[complete.cases(test.data_1),]

test.data_1$b <- -1

data_1 <- rbind(data_1, test.data_1)

#make two stage
data_1$b[data_1$b==5] <- -1
data_1$b[data_1$b==3] <- -1 #& data_1$b<5

#track_CAGA_005 is the example data set included in the package
str(data_1)

data_1$t <- as.POSIXct(data_1$t, format = "%m/%d/%Y %H:%M:%S", origin = "1970-01-01")

#build the xytb object
xytb <- xytb(data_1, desc="BTGO Birds",winsize=seq(3,15,2), idquant=seq(0,1,.25),move=c(5,10,15))
#ex <- xytb(track_CAGA_005, desc="BTGO Birds",winsize=seq(3,15,2), idquant=seq(0,1,.25))
xytb@befdxyt
#xytb@xyt
#ex

#xytb<-xytb(track_CAGA_005,desc="example track",winsize=seq(3,15,2),idquant=seq(0,1,.25),move=c(5,10,15))
#xytb@befdxyt
#preview, color coded
#plot(xytb)

#model with random forest
#watch out that the first t is not missing - if so, you will get a funky error "Error in `+.POSIXt`(as.POSIXct(origin, tz = "GMT", ...), x) :    binary '+' is not defined for "POSIXt" objects"
chick_rf <- modelRF(xytb, type = "actual", nob = "-1", ntree = 3201, mtry = 40) #method="ranger", no necessary as the modelRF function defaults to randomForest
#mtry is better as increases, cannot exceed columns in the dataset

str(chick_rf)

#resRF(chick_rf) #to view the out of bag error limit
#resRF(chick_rf, "importance") # importance of individual variables
resRF(chick_rf,"confusion") # view the confusion matrix and the statistics by each class


#resB(chick_rf, nob="-1") #this now presents the behavioral states as predicted, vs the one observed - compared in time

#resB(chick_rf, nob="-1", "space") # and then viewed in space

#resB(chick_rf,"density", nob="-1") # finally density - isnt working

# need to export and predict from trained model (above) on new data for which we have annotations. Testing the model.

predictions <- chick_rf@predb
class(predictions)



# to extract the model and make predictions without the need to merge birds together





####export and test#####
## --- To DO LIST --- ##

# need to export and predict from trained model (above) on new data for which we have annotations. Testing the model.

#plot(chick_modRF)

#call test data, using similar to above
test.data <- rbind(myfiles[[1]],myfiles[[2]],myfiles[[3]],myfiles[[4]],myfiles[[5]])

#process as before, convert to xytb object
str(test.data)
test.data_1 <- test.data[,c(4,3,6,5,1)]; str(test.data_1); head(test.data_1)

names(test.data_1)[1] <- "x"
names(test.data_1)[2] <- "y"
names(test.data_1)[3] <- "t"
names(test.data_1)[4] <- "b"
names(test.data_1)[5] <- "id"

str(test.data_1)
head(test.data_1);tail(test.data_1)

test.data_1$x <- as.numeric(test.data_1$x)
test.data_1$y <- -1*as.numeric(test.data_1$y)
test.data_1$b <- as.character(test.data_1$b)
test.data_1$id <- as.character(test.data_1$id)
test.data_1$t <- as.POSIXct(test.data_1$t, format = "%m/%d/%Y %H:%M", origin = "1970-01-01") #for some reason, the t column lost the seconds

str(test.data_1); head(test.data_1); tail(test.data_1)
unique(test.data_1$id)

test.data_1 <- test.data_1[complete.cases(test.data_1),]


xytb.test <- xytb(test.data_1, desc="BTGO Birds",winsize=seq(3,15,2), idquant=seq(0,1,.25),move=c(5,10,15))

xytb.test@befdxyt

xytb.test.pred <- cbind(xytb.test@b,xytb.test@dxyt,stringsAsFactors=F)

xytb.test.pred <- na.omit(xytb.test.pred)

chick_modRF <- extractRF(chick_rf)

xytb.test.pred

predictions_exported <- predict(chick_modRF, newdata = xytb.test.pred)

#class(xytb.test@dxyt)

#### create encounter and GPS matrices from predicted behaviors ####

# summarize predictions df
str(predictions)

#extract julian day
Julian <- as.numeric(format(predictions$t, "%j"))
predictions <- cbind(predictions, Julian)

unique(predictions$id)
#define bounds of the 
#Nesting - between 70 and 213
#chick tending - between 80 and 244

#fate



#GPS fixes
gps.fixes <- predictions %>% group_by(id) %>% count(Julian)

gps.fixes.df <- as.data.frame(gps.fixes)
head(gps.fixes.df)


#what I am trying to do here is fill in the missing values for days not in the sample. We will need both matrices (GPS fixes, and Fate) to have the same dimensions
range <- seq(1,365,by=1)
range_full <- expand.grid(range, gps.fixes.df$id)

colnames(range_full) <- c("Julian","id")

result <- merge(gps.fixes.df, range_full, by=("id"), all.y=F)



new <- result %>% spread(key = Julian, value = n)

gps.fixes.mat <- as.matrix(new)



gps.fixes.mat <- gps.fixes.df %>% gather(id,n,-Julian) %>% spread(Julian) %>% column_to_rownames(id)


#### predict survival from states ####







# function to calculate the last day alive:
get.last <- function(m) max(which(m %in% c(0,1)))
get.first <- function(m) min(which(m %in% c(0,1)))


# logit and inverse logit functions:
fn_logit <- function(X) log(X / (1 - X))
fn_invlogit <- function(X)	1/(1+exp(-(X)))

# 
# 

#set working directory
#this will need to be updated on each machine
#always reload wd after running the script

#
init <- Sys.time()	# record the start time for the example script

#
setwd("C:/Users/14064/Dropbox/Chapter 2/Data/Data for Analyses/Individual Mismatch/Data")

#
outfolder <- "HUGO_DSR_output/"
dir.create(outfolder)

## --- To DO LIST --- ##

# Create encounter histories for nesting and chick-tending phase
# to be able to hijack the nestR framework, we need two matrices of the same dimensions, where both have a row for each nest attempt and the n.col corresponding to the days of the attempt - so for nesting, 21 col, for chick tending, 28.
#matrices are: 
#1. 1 or 0 whether behavior was seen each day, based on persistence check (> 2 hrs) (either we model as a dnorm using hours spent in behavior, or we give a bernoulli 1 or 0 and include a persistence check). This feeds into the state (survival) process
#2. A matrix, same dimensions, of the number of gps fixes per day. This feeds in to the space (observation) process

#to reiterate, for the model to work, we need each birds history to start on the day of first nesting being detected (need to check against 28-31 days b4 hatch date) for either matrix

## ------------------ ##

#objects to define
days
nests
gps_fixes
CH


# Create vector with occasion of marking, where CH is the encounter history
#get.first <- function(x) min(which(x!=0))
f <- apply(CH, 1, get.first)

#
f <- as.numeric(apply(NH, 1, get.first))	
l <- as.numeric(apply(NH, 1, get.last))	




####choose among constant or time-varying models, JAGS ####
#constant survival, constant detection
sink("BTGO_UvA_nest_null.txt")
cat("model {

# Priors and constraints
phi.b0 ~ dnorm(0, 1e-5)
phi.b1 <- 0
p.b0 ~ dnorm(0, 1e-5)
p.b1 <- 0
  for (t in 1:(days-1)){
    logit(phi[t]) <- phi.b0 + phi.b1 * t
}

for (t in 1:days){
  logit(p[t]) <- p.b0 + p.b1 * t
  }


# Likelihood
for (i in 1:nests){
   # Define latent state at first capture
   z[i, 1] <- 1
   for (t in 2:days){
      # State process
      mu1[i, t] <- phi[t-1] * z[i, t-1]
      z[i, t] ~ dbern(mu1[i, t])

      # Observation process
      mu2[i, t] <- p[t] * z[i, t]
      y[i, t] ~ dbin(mu2[i, t], gps_fixes[i, t])
      } #t
   } #i
}
", fill=TRUE)
sink()

#constant survival, time-varying detection
sink("BTGO_UvA_nest_p_time.txt")
cat("model {
  
  # Priors and constraints
  phi.b0 ~ dnorm(0, 1e-5)
  phi.b1 <- 0
  p.b0 ~ dnorm(0, 1e-5)
  p.b1 ~ dnorm(0, 1e-5)
  
  for (t in 1:(days-1)){
    logit(phi[t]) <- phi.b0 + phi.b1 * t
  }
  
  for (t in 1:days){
    logit(p[t]) <- p.b0 + p.b1 * t
  }
  
  
  # Likelihood
  for (i in 1:nests){
    # Define latent state at first capture
    z[i, 1] <- 1
    for (t in 2:days){
      # State process
      mu1[i, t] <- phi[t-1] * z[i, t-1]
      z[i, t] ~ dbern(mu1[i, t])
      
      # Observation process
      mu2[i, t] <- p[t] * z[i, t]
      y[i, t] ~ dbin(mu2[i, t], gps_fixes[i, t])
    } #t
  } #i
}", fill=TRUE)
sink()

#time-varying survival, constant detection
sink("BTGO_UvA_nest_phi_time.txt")
cat("model {
  
  # Priors and constraints
  phi.b0 ~ dnorm(0, 1e-5)
  phi.b1 <- dnorm(0, 1e-5)
  p.b0 ~ dnorm(0, 1e-5)
  p.b1 ~ 0
  
  for (t in 1:(days-1)){
    logit(phi[t]) <- phi.b0 + phi.b1 * t
  }
  
  for (t in 1:days){
    logit(p[t]) <- p.b0 + p.b1 * t
  }
  
  
  # Likelihood
  for (i in 1:nests){
    # Define latent state at first capture
    z[i, 1] <- 1
    for (t in 2:days){
      # State process
      mu1[i, t] <- phi[t-1] * z[i, t-1]
      z[i, t] ~ dbern(mu1[i, t])
      
      # Observation process
      mu2[i, t] <- p[t] * z[i, t]
      y[i, t] ~ dbin(mu2[i, t], gps_fixes[i, t])
    } #t
  } #i
}", fill=TRUE)
sink()

#time-varying survival, time-varying detection
sink("BTGO_UvA_nest_p_time_phi_time.txt")
cat("model {
  
  # Priors and constraints
  phi.b0 ~ dnorm(0, 1e-5)
  phi.b1 <- dnorm(0, 1e-5)
  p.b0 ~ dnorm(0, 1e-5)
  p.b1 ~ dnorm(0, 1e-5)
  
  for (t in 1:(days-1)){
    logit(phi[t]) <- phi.b0 + phi.b1 * t
  }
  
  for (t in 1:days){
    logit(p[t]) <- p.b0 + p.b1 * t
  }
  
  
  # Likelihood
  for (i in 1:nests){
    # Define latent state at first capture
    z[i, 1] <- 1
    for (t in 2:days){
      # State process
      mu1[i, t] <- phi[t-1] * z[i, t-1]
      z[i, t] ~ dbern(mu1[i, t])
      
      # Observation process
      mu2[i, t] <- p[t] * z[i, t]
      y[i, t] ~ dbin(mu2[i, t], gps_fixes[i, t])
    } #t
  } #i
}", fill=TRUE)
sink()


#### run model ####

initialize_z <- function(ch) {
  # Initialize state using the "capture history" (in CMR parlance)
  state <- ch
  
  # Loop through each nest
  for (i in 1:nrow(ch)) {
    # The earliest "sighting" will always be the first day of the attempt
    n1 <- 1
    
    # The last sighting is the last time the animal was observed at the nest
    n2 <- max(which(ch[i,] > 0))
    
    # Set all states between first and last to 1
    state[i, n1:n2] <- 1
    
    # Reset first to NA (because always see them on first day by definition)
    state[i, n1] <- NA
  }
  
  # Now set any states remaining as 0 to NA so that JAGS will estimate them
  state[state == 0] <- NA
  
  # Return
  return(state)
}


#bundle data
jags.model

#path to JAGS file
jags_file <- file.path(system.file(package = "nestR"), "jags", model_txt)

#starting values for survival status
s1 <- initialize_z(ch=visits)

#set parametrs
mcmc_params = list(burn_in = 500,
                   n_chain = 4,
                   thin = 5,
                   n_adapt = 500,
                   n_iter = 10000)


#define JAGS model
jags <- rjags::jags.model(file = jags.file,
                          data = list("nests"=nrow(visits),
                                      "days"=ncol(visits),
                                      "gps_fixes"=fixes,
                                      "y"=visits),
                          inits = list("z"=s1),
                          n.chain = mcmc_params$n_chain,
                          n.adapt = mcmc_params$n_adapt)

#run the burn-in
rjags:::update.jags(object = jags, n.iter = mcmc_params$burn_in)

#generate posterior samples
post <- rjags::jags.samples(model = jags, variable.names = c("phi.b0", "phi.b1", "phi", "p.b0", "p.b1", "p", "z"),n.iter = mcmc_params$n_iter,thin = mcmc_params$thin)

#add the names to the list 'post'
post$names <- row.names(fixes)

#add the model type
post$model <- model

return(post)





#functions from S Pacardi and B Smith (nestR)
plot_survival <- function(mcmc_obj, ci = 0.95){
  
  # Get the detection element from the MCMC list
  phi <- mcmc_obj$phi
  
  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2
  
  # Data frame
  # Mean across all MCMC iterations and chains
  # Credible intervals across all MCMC iterations and chains
  phi_df <- data.frame(phi_mean = apply(phi, 1, mean),
                       phi_lwr <- apply(phi, 1, quantile, lwr),
                       phi_upr <- apply(phi, 1, quantile, upr))
  
  # Plot
  ggplot2::ggplot(phi_df,
                  ggplot2::aes(1:length(phi_mean), phi_mean)) +
    ggplot2::ylim(0, 1) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = phi_lwr,
                                      ymax = phi_upr),
                         fill = "#43BF7199") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))) +
    ggplot2::ylab("Daily Survival (phi)") +
    ggplot2::xlab("Days") +
    ggplot2::ggtitle("Survival")
  
}
plot_detection <- function(mcmc_obj, ci = 0.95){
  
  # Get the detection element from the MCMC list
  p <- mcmc_obj$p
  
  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2
  
  # Data frame
  # Mean across all MCMC iterations and chains
  # Credible intervals across all MCMC iterations and chains
  p_df <- data.frame(p_mean = apply(p, 1, mean),
                     p_lwr <- apply(p, 1, quantile, lwr),
                     p_upr <- apply(p, 1, quantile, upr))
  
  # Plot
  ggplot2::ggplot(p_df,
                  ggplot2::aes(1:length(p_mean), p_mean)) +
    ggplot2::ylim(0, 1) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p_lwr,
                                      ymax = p_upr),
                         fill = "#41448799") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))) +
    ggplot2::ylab("Daily Detection Probability (p)") +
    ggplot2::xlab("Days") +
    ggplot2::ggtitle("Detection Probability")
  
}
plot_nest_surv <- function(mcmc_obj, who = 1, ci = 0.95){
  
  # Get the latent survival element from the MCMC list
  z <- mcmc_obj$z
  
  # Get the individual of interest
  z_ind <- z[who, , ,]
  
  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2
  
  # Data frame
  # Mean across all MCMC iterations and chains
  # Credible intervals across all MCMC iterations and chains
  zind_df <- data.frame(z_mean = apply(z_ind, 1, mean),
                        z_lwr <- apply(z_ind, 1, quantile, lwr),
                        z_upr <- apply(z_ind, 1, quantile, upr))
  
  # Plot
  ggplot2::ggplot(zind_df,
                  ggplot2::aes(1:length(z_mean), z_mean)) +
    ggplot2::ylim(0, 1) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = z_lwr,
                                      ymax = z_upr),
                         fill = viridis::viridis(n = 1,
                                                 alpha = 0.5,
                                                 begin = 0.8,
                                                 end = 0.8)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))) +
    ggplot2::ylab("Daily Survival (Z)") +
    ggplot2::xlab("Days") +
    ggplot2::ggtitle(paste0("Survival of Nest ", mcmc_obj$names[who]))
  
}

summarize_outcomes <- function(mcmc_obj, ci = 0.95){
  
  # Initialize list for output
  out <- list()
  
  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2
  
  ### Population-level survival
  
  # If the model had time-varying phi, report the slope and intercept
  if (grepl("phi_time", mcmc_obj$model)){
    
    # Note that these parameters are on logit scale
    out$phi <- data.frame(b0_lwr = apply(mcmc_obj$phi.b0, 1, quantile, lwr),
                          b0_mean = apply(mcmc_obj$phi.b0, 1, mean),
                          b0_upr = apply(mcmc_obj$phi.b0, 1, quantile, upr),
                          b1_lwr = apply(mcmc_obj$phi.b1, 1, quantile, lwr),
                          b1_mean = apply(mcmc_obj$phi.b1, 1, mean),
                          b1_upr = apply(mcmc_obj$phi.b1, 1, quantile, upr))
  } else {
    
    # Note that these estimates are not on logit scale
    out$phi <- data.frame(lwr = quantile(mcmc_obj$phi, lwr),
                          mean = mean(mcmc_obj$phi),
                          upr = quantile(mcmc_obj$phi, upr))
    row.names(out$phi) <- NULL
    
  }
  
  ### Population-level detection
  
  if (grepl("p_time", mcmc_obj$model)){
    
    # Note that these parameters are on logit scale
    out$p <- data.frame(b0_lwr = apply(mcmc_obj$p.b0, 1, quantile, lwr),
                        b0_mean = apply(mcmc_obj$p.b0, 1, mean),
                        b0_upr = apply(mcmc_obj$p.b0, 1, quantile, upr),
                        b1_lwr = apply(mcmc_obj$p.b1, 1, quantile, lwr),
                        b1_mean = apply(mcmc_obj$p.b1, 1, mean),
                        b1_upr = apply(mcmc_obj$p.b1, 1, quantile, upr))
  } else {
    
    # Note that these estimates are not on logit scale
    out$p <- data.frame(lwr = quantile(mcmc_obj$p, lwr),
                        mean = mean(mcmc_obj$p),
                        upr = quantile(mcmc_obj$p, upr))
    row.names(out$p) <- NULL
    
  }
  
  ### Individual burst outcomes
  
  # Get the latent survival variable
  z <- mcmc_obj$z
  
  # Get the burst names
  bursts <- mcmc_obj$names
  
  # Create data.frame of results
  indiv <- data.frame(burst = bursts,
                      pr_succ_lwr = NA,
                      pr_succ_mean = NA,
                      pr_succ_upr = NA,
                      last_day_lwr = NA,
                      last_day_mean = NA,
                      last_day_upr = NA)
  
  # Probability burst was a successful nest (survived to last day)
  # Get the last day for each burst + iteration + chain
  last_day <- apply(z, c(1, 3, 4), getElement, ncol(z))
  # Get values for each burst
  indiv$pr_succ_lwr <- apply(last_day, 1, quantile, lwr)
  indiv$pr_succ_mean <- apply(last_day, 1, mean)
  indiv$pr_succ_upr <- apply(last_day, 1, quantile, upr)
  
  # Latest day that a nest survived to
  latest_day <- apply(z, c(1, 3, 4), sum)
  # Get values for each burst
  indiv$last_day_lwr <- apply(latest_day, 1, quantile, lwr)
  indiv$last_day_mean <- apply(latest_day, 1, mean)
  indiv$last_day_upr <- apply(latest_day, 1, quantile, upr)
  
  # Add to output list
  out$outcomes <- indiv
  
  return(out)
}