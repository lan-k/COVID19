rm(list = ls())
#.libPaths("C:/Users/lan.kelly/Documents/R/win-library/4.0")
####R world map for COVID weights and IFRs
library(rworldmap)
library(dplyr)

#setwd('D:/work/SAHMRI/COVID-19/Data')
setwd("C:/Users/lan.kelly/Documents/SAHMRI/COVID-19/Data")


# ---- plot trajectory ----
load(file="world_trajectories_13OCT2020.Rdata")

table(cum_death_clusters$cluster30) #1=61, 2=53
table(cum_death_clusters$cluster60) #1=78, 2=36
table(cum_death_clusters$cluster120) #1=98, 2=16
table(cum_death_clusters$cluster180) #1=86, 2=28


nochange <- cum_death_clusters %>%
  filter(cluster30 == cluster60 & cluster60 == cluster120  & cluster60==cluster180)
#37 Brazil and Kuwait in fast cluster at all time points


diff <- cum_death_clusters %>% anti_join(nochange, by="ID")  #77
worse <- cum_death_clusters %>% filter(cluster30 ==1 &  cluster180 == 2) #17
better <- cum_death_clusters %>% filter(cluster30 ==2 &  cluster180 == 1)  #42


cum_death_clusters <- cum_death_clusters %>%
  mutate(traj_change = 
           as.factor(case_when(cluster30 == 1 & cluster60 == 1  & cluster120 == 1 &  cluster180 == 1 ~ 1,
                               cluster30 == 2 & cluster60 == 2 & cluster120 == 2 &  cluster180 == 2 ~ 2,
                               cluster30 == 1 & (cluster60 !=1  | cluster120 != 1) & cluster180 == 1 ~ 3,
                               cluster30 == 2 & (cluster60 !=2  | cluster120 != 2) & cluster180 == 2 ~ 4,
                               cluster30 == 2 & cluster180 == 1 ~ 5,
                               cluster30 == 1 & cluster180 == 2 ~ 6,
                                )))
table(cum_death_clusters$traj_change, useNA = "always")


table(cum_death_clusters$cluster60,cum_death_clusters$traj_change)
table(cum_death_clusters$cluster120,cum_death_clusters$traj_change)

cum_death_clusters$cluster30 <- as.factor(cum_death_clusters$cluster30)
cum_death_clusters$cluster60 <- as.factor(cum_death_clusters$cluster60)
cum_death_clusters$cluster120 <- as.factor(cum_death_clusters$cluster120)
cum_death_clusters$cluster180 <- as.factor(cum_death_clusters$cluster180)



traj <- joinCountryData2Map( cum_death_clusters
                             , joinCode = "ISO3"
                             , nameJoinColumn = "ISO3") 
levels(traj@data[["cluster30"]]) <- c("Slow","Fast" ) 
levels(traj@data[["cluster60"]]) <- c("Slow","Fast" ) 
levels(traj@data[["cluster120"]]) <- c("Slow","Fast" ) 
levels(traj@data[["cluster180"]]) <- c("Slow","Fast" ) 

levels(traj@data[["traj_change"]]) <- c("Slow","Fast",
                                        "Stabilising","Accelerating",
                                        "Improved","Worse" ) 


colour <- c("blue","red")


tiff(file="trajectory clusters world map 30 to 180 days.tif", width = 1600, height = 1000, units = "px", res=300)

#par(mai=c(0.1,0.1,0.1,0.1),mfrow=c(2,2),xaxs="i",yaxs="i") 
par(oma=c(0.5,0.1,0.1,0.1), xpd=F)
par(mar=c(0,0.1,0.8,0.1), mfrow=c(2,2),xaxs="i",yaxs="i")

#30 days
mapCountryData( traj, nameColumnToPlot="cluster30" , numCats=2,catMethod="categorical",
                mapTitle = "A", addLegend = F,
                oceanCol = 'light blue', missingCountryCol='white',
                colourPalette = colour ) #

#60 days
mapCountryData( traj, nameColumnToPlot="cluster60" , numCats=2,catMethod="categorical",
                             mapTitle = "B", addLegend = F,
                             oceanCol = 'light blue', missingCountryCol='white',
                             colourPalette = colour ) #

##120 days

mapParams <- mapCountryData( traj, nameColumnToPlot="cluster120" , numCats=2,catMethod="categorical",
                             mapTitle = "C", addLegend = F,
                             oceanCol = 'light blue', missingCountryCol='white',
                             colourPalette = colour) #"heat"


##180 days

mapParams <- mapCountryData( traj, nameColumnToPlot="cluster180" , numCats=2,catMethod="categorical",
                             mapTitle = "D", addLegend = F,
                             oceanCol = 'light blue', missingCountryCol='white',
                             colourPalette = colour) #"heat"


###overlay legend
#par(fig = c(0, 1, 0, 1 ), oma = c(0, 0, 0, 0), mar = c(2, 0, 0, 0), new = TRUE)
par(fig = c(0, 1, 0, 1 ), oma = c(0, 0, 0, 0), mar = c(0.2, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

do.call( addMapLegendBoxes
         , c(mapParams
             ,cex = 0.6
             , pt.cex = 1.2
             ,bty = "n"
             ,x='bottom'
             ,title=""
             ,horiz=T))

dev.off()


###change in group membership over time
tiff(file="trajectory change world map 30 to 180 days.tif", width = 1600, height = 1050, units = "px", res=300)

par(mai=c(0,0,0.2,0),mfrow=c(1,1),xaxs="i",yaxs="i")  #

mapParams <- mapCountryData( traj, nameColumnToPlot="traj_change" , numCats=4,catMethod="categorical",
                             mapTitle = "", addLegend = F,
                             oceanCol = 'light blue', missingCountryCol='white',
                             colourPalette = c("blue", "red","purple" ,
                                               "yellow" ,"green" ,"orange")) #"heat"  

do.call( addMapLegendBoxes
         , c(mapParams
             ,cex = 0.6
             , pt.cex = 1.5
             ,bty = "n"
             ,x='bottom'
             ,title=""
             ,horiz=T))

dev.off()
