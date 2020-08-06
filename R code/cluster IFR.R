####cluster weights


library(tidyverse)
library(mclust)

IFR_China <- 0.00657  ##overall IFR from Verity 2020 based on Chinese data 



###weights for all countries in WHO
load("./Data/weights_ISO_China.Rdata")

###world populations by age group per 1000
world_pop <- read.csv("./Data/World population by age 2020.csv", stringsAsFactors = F) 
world_pop$pop <- rowSums(world_pop[,5:22])
pop_total <- sum(world_pop[,"pop"])
world_pop <- world_pop %>%
  mutate(pop = pop/pop_total) %>%
  select(Country.code, pop)

UN_iso_codes <- read.csv("./Data/ISO3 to UN code.csv",stringsAsFactors = F) %>%
  mutate(ISO3=substr(ISO3,1,3))

weights_ISO_clust <- weights_ISO %>%
  left_join(UN_iso_codes, by="ISO3") %>%
  left_join(world_pop, by="Country.code") %>%
  mutate(IFR = IFR_China /weight_death,
         IFR_prop = IFR*pop)

sum(weights_ISO_clust$pop)

##assume single distribution and calculate the mean and sd
100*mean(weights_ISO_clust$IFR); 100*sd(weights_ISO_clust$IFR)


###model based clustering
###cluster the weights
wt_death <- weights_ISO_clust$weight_death
fit <- Mclust(wt_death)
plot(fit) # plot results
summary(fit) # display the best model 

aggregate(wt_death,by=list(fit$classification),FUN=mean)
aggregate(wt_death,by=list(fit$classification),FUN=min)
aggregate(wt_death,by=list(fit$classification),FUN=max)

weights_ISO_clust$wt_class <- 4-fit$classification

##cluster the IFRs

IFR_pop <- weights_ISO_clust$IFR
fit2 <- Mclust(IFR_pop)
#plot(fit2) # plot results
plot(fit2, what = "density")
summary(fit2) # display the best model 

weights_ISO_clust$IFR_class <- fit2$classification  #highest weight corresponds to lowest IFR

boot1 <- MclustBootstrap(fit2, nboot = 99999, type = "bs")
summary(boot1, what="ave")
summary(boot1, what="se")
summary(boot1, what="ci")
par(mfrow=c(1,3))
plot(boot1, what = "pro")
plot(boot1, what = "mean")

## Density estimation via Gaussian finite mixture modeling
mod <- densityMclust(IFR_pop)
summary(mod)
plot(mod, what = "density", data = IFR_pop, breaks = 50, xlab="IFR")

##model fit diagnostics
plot(mod, what="diagnostic", type="qq")
plot(mod, what = "diagnostic", type = "cdf")


