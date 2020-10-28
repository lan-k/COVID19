rm(list = ls())
#.libPaths("C:/Users/lan.kelly/Documents/R/win-library/4.0")
####R world map for COVID weights and IFRs
library(rworldmap)
library(dplyr)


# ---- plot trajectory ----
load(file="../Data/world_150daytrajectories_13OCT2020.Rdata")

table(cum_death_clusters$cluster50) #1=75, 2=47
table(cum_death_clusters$cluster150) #1=61, 2=38, 3=23


nochange <- cum_death_clusters %>%

  filter(cluster50 == cluster150)  #64


diff <- cum_death_clusters %>% anti_join(nochange, by="ID")  #58
better <- cum_death_clusters %>% 
  filter((cluster50 == 2 &  cluster150 == 1)  ) #14
worse <- cum_death_clusters %>% 
  filter((cluster50 ==1 &  cluster150 %in% c(2,3))  |
           (cluster50 ==2 &  cluster150 == 3)) #44



table(cum_death_clusters$traj_change, useNA = "always")
# 1   2    3    4 
# 47  17   14   44   

table(cum_death_clusters$cluster50,cum_death_clusters$traj_change)
table(cum_death_clusters$cluster150,cum_death_clusters$traj_change)

cum_death_clusters$cluster50 <- as.factor(cum_death_clusters$cluster50)
cum_death_clusters$cluster150 <- as.factor(cum_death_clusters$cluster150)




traj <- joinCountryData2Map( cum_death_clusters
                             , joinCode = "ISO3"
                             , nameJoinColumn = "ISO3") 
levels(traj@data[["cluster50"]]) <- c("Steady","Moderate" ) 
levels(traj@data[["cluster150"]]) <- c("Steady","Moderate","Fast" ) 


levels(traj@data[["traj_change"]]) <- c("Steady","Moderate",
                                        "Improved","Worse" ) 

col2 <- c("blue","orange")
col3 <- c("blue","orange","red")


tiff(file="trajectory clusters world map 50 to 150 days.tif", width = 1600, height = 1600, units = "px", res=300)

par(mai=c(0,0,0.2,0),mfrow=c(2,1),xaxs="i",yaxs="i")

#50 days
mapCountryData( traj, nameColumnToPlot="cluster50" , numCats=2,catMethod="categorical",
                mapTitle = "A", addLegend = F,
                oceanCol = 'light blue', missingCountryCol='white',
                colourPalette = col2 ) #

#150 days
mapParams <- mapCountryData( traj, nameColumnToPlot="cluster150" , numCats=2,catMethod="categorical",
                             mapTitle = "B", addLegend = F,
                             oceanCol = 'light blue', missingCountryCol='white',
                             colourPalette = col3 ) #


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
tiff(file="trajectory change world map 50 to 150 days.tif", width = 1600, height = 1050, units = "px", res=300)

par(mai=c(0,0,0.2,0),mfrow=c(1,1),xaxs="i",yaxs="i")  #

mapParams <- mapCountryData( traj, nameColumnToPlot="traj_change" , numCats=4,catMethod="categorical",
                             mapTitle = "", addLegend = F,
                             oceanCol = 'light blue', missingCountryCol='white',
                             colourPalette = c("blue", "orange","green" ,"red")) #"heat"  

do.call( addMapLegendBoxes
         , c(mapParams
             ,cex = 0.6
             , pt.cex = 1.5
             ,bty = "n"
             ,x='bottom'
             ,title=""
             ,horiz=T))

dev.off()
