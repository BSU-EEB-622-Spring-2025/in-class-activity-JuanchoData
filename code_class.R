# Questions 

# 1. Does sound from boats influence beluga whale song duration and frequency (total calls/hr)? 
# 2. Do boats have other direct impacts on whale calling behavior, outside of their noise impacts?

# Question 1
# Predictor Variable: 
# Boats Activity: discrete, bounded by zero
# Boat Noise: continuoius, bounded by zero

# Response Variable: Beluga song 
# Number of songs: discrete, bounded by zero
# Length of song: continuous, bounded by zero

# Total Observations:
# 193 1-hr recordings across the 40 sensors

# Other variables:
# Distance from shore
# Water temp
# Water depth

recordings <- read.csv("recordings.csv")
sensorinfo <- read.csv("sensorinfo.csv")

library(MASS)
library(brms)
library(performance)
library(tidyverse)
library(ggplot2)
library(patchwork) # combine plots with the + and / signs
library(marginaleffects)

# Boat Noise Model
# whale call duration/count ~ boat noise * temp * boat activity
str(recordings)

#change water temp to integer
recordings$watertemp <- as.integer(recordings$watertemp)


# remove nas 
recordings<- na.omit(recordings)

scale2sd <- function(x){
  (x - mean(x))/(sd(x)*2)
}

recordings$boatnoise <- scale2sd(recordings$boatnoise)
recordings$watertemp <- scale2sd(recordings$watertemp)
recordings$boatactivity <- scale2sd(recordings$boatactivity)

boatnoise.nbin <- brm(totsongs~boatnoise + watertemp + boatactivity, data=recordings, family="negbinomial")

summary(boatnoise.nbin)

#posterior

mcmc_plot(boatnoise.nbin) + theme_bw()

#model fit

bayes_R2(boatnoise.nbin) #bayes_R2 can help you interpret model fit with bounds around it (suite of yhat along x-axis)
performance::mae(boatnoise.nbin)

#marginal effects
plot_predictions(boatnoise.nbin, condition ="boatnoise")

# Boat Activity Model
# whale call duration/ count ~ boat activity * water depth * dist from shore

data <- merge(recordings, sensorinfo, by = "sensorid")

data$waterdepth <- scale2sd(data$waterdepth)
data$distshore <- scale2sd(data$distshore)

data$songlength <- as.numeric(data$songlength)

# model
boatimpact.gam <- brm(songlength~boatactivity + waterdepth + distshore + (1 | sensorid), data=data, family="Gamma"(link=log))

summary(boatimpact.gam)

#posterior

mcmc_plot(boatimpact.gam) + theme_bw()

#model fit

bayes_R2(boatimpact.gam) #bayes_R2 can help you interpret model fit with bounds around it (suite of yhat along x-axis)
performance::mae(boatimpact.gam)

#marginal effects

plot_predictions(boatimpact.gam, condition="boatactivity")



# Questions: why did we use call count for the first model and call length for the second model

