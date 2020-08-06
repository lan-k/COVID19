rm(list = ls())

####R world map for COVID weights and IFRs
library(rworldmap)
library(dplyr)

load(file="./Data/IFR_wt_cluster.Rdata")

IFR_med_hi <- weights_ISO_clust %>%
  filter(IFR_class != 1) %>%
  summarise(mean=100*mean(IFR), n=n(), sd=100*sd(IFR),
            ci_lo = mean - 1.96*sd/sqrt(n), ci_hi=mean + 1.96*sd/sqrt(n))

weights_ISO_clust$IFR_class <- as.factor(weights_ISO_clust$IFR_class)



sPDF <- joinCountryData2Map( weights_ISO_clust
                             , joinCode = "ISO3"
                             , nameJoinColumn = "ISO3")  
###plot IFR

mapCountryData( sPDF, nameColumnToPlot="IFR", numCats = 8,
                mapTitle = "",  addLegend = T,
                oceanCol = 'light blue', missingCountryCol='white',
                colourPalette = "heat" )


##label the categories

levels(sPDF@data[["IFR_class"]]) <- c("0.20 (0.18-0.23)","0.38 (0.30-0.49)","0.93 (0.78-1.03)" )

# ---- plot IFR clusters ----
par(mfrow=c(1,1), mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
breaks = c(0.00139, 0.0025,0.0061,0.0151)
mapParams <- mapCountryData( sPDF, nameColumnToPlot="IFR", numCats = 3,
                             catMethod = breaks,
                mapTitle = "COVID-19 Infection Fatality Ratio",  addLegend = F,
                oceanCol = 'light blue', missingCountryCol='white',
                colourPalette = c("green","orange","red") ) #"rainbow"

do.call( addMapLegend
         , c( mapParams
              #,cutVector = breaks
              , legendLabels="all"
              , legendWidth=0.5
              , legendIntervals="data"
              , sigFigs = 2
              , legendMar = 2 ) )


##plot categories with 95% CIs

tiff(file="IFR world map CI.tif", width = 1600, height = 1050, units = "px", res=300)
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

mapParams <- mapCountryData( sPDF, nameColumnToPlot="IFR_class" , numCats=3,catMethod="categorical",
                mapTitle = "", addLegend = F,
                oceanCol = 'light blue', missingCountryCol='white',
                colourPalette = "heat") #COVID-19 Infection Fatality Ratio

do.call( addMapLegendBoxes
         , c(mapParams
             ,cex = 0.6
             , pt.cex = 1.5
             ,bty = "n"
             ,x='bottom'
             ,title="IFR (%) (95% CI)"
             ,horiz=T))

dev.off()


# ---- plot trajectory ----
load(file=".Data/world trajectories.Rdata")

table(cum_death_clusters$cluster50)
table(cum_death_clusters$cluster100)

cum_death_clusters <- cum_death_clusters %>%
  mutate(traj_change = as.factor(case_when(cluster50 == 1 & cluster100 == 1 ~ 1,
                                 cluster50 == 2 & cluster100 == 1 ~ 2,
                                 cluster50 == 1 & cluster100 == 2 ~ 3,
                                 cluster50 == 2 & cluster100 == 2 ~ 4)))

table(cum_death_clusters$cluster50,cum_death_clusters$traj_change)
table(cum_death_clusters$cluster100,cum_death_clusters$traj_change)

diff <- cum_death_clusters %>% filter(cluster50 != cluster100)

worse <- cum_death_clusters %>% filter(cluster50 ==1 &  cluster100 == 2)
better <- cum_death_clusters %>% filter(cluster50 ==2 &  cluster100 == 1)

cum_death_clusters$cluster50 <- as.factor(cum_death_clusters$cluster50)
cum_death_clusters$cluster100 <- as.factor(cum_death_clusters$cluster100)

tiff(file="trajectory clusters world map.tif", width = 1600, height = 1600, units = "px", res=300)

par(mai=c(0,0,0.2,0),mfrow=c(2,1),xaxs="i",yaxs="i")  #

traj <- joinCountryData2Map( cum_death_clusters
                             , joinCode = "ISO3"
                             , nameJoinColumn = "ISO3") 
levels(traj@data[["cluster50"]]) <- c("Stable","Accelerating" ) 
levels(traj@data[["cluster100"]]) <- c("Stable","Accelerating" )  

levels(traj@data[["traj_change"]]) <- c("Stable","Improved","Worse","Accelerating" ) 

#50 days
colour <- c("blue","red")

mapCountryData( traj, nameColumnToPlot="cluster50" , numCats=2,catMethod="categorical",
                             mapTitle = "A", addLegend = F,
                             oceanCol = 'light blue', missingCountryCol='white',
                             colourPalette = colour ) #

##100 days

mapParams <- mapCountryData( traj, nameColumnToPlot="cluster100" , numCats=2,catMethod="categorical",
                             mapTitle = "B", addLegend = F,
                             oceanCol = 'light blue', missingCountryCol='white',
                             colourPalette = colour) #"heat"

do.call( addMapLegendBoxes
         , c(mapParams
             ,cex = 0.6
             , pt.cex = 1.5
             ,bty = "n"
             ,x='bottom'
             ,title=""
             ,horiz=T))

dev.off()


###change in group membership over time
tiff(file="trajectory change world map.tif", width = 1600, height = 1050, units = "px", res=300)

par(mai=c(0,0,0.2,0),mfrow=c(1,1),xaxs="i",yaxs="i")  #

mapParams <- mapCountryData( traj, nameColumnToPlot="traj_change" , numCats=4,catMethod="categorical",
                             mapTitle = "", addLegend = F,
                             oceanCol = 'light blue', missingCountryCol='white',
                             colourPalette = c("blue", "purple" ,"orange" ,"red")) #"heat"  

do.call( addMapLegendBoxes
         , c(mapParams
             ,cex = 0.6
             , pt.cex = 1.5
             ,bty = "n"
             ,x='bottom'
             ,title=""
             ,horiz=T))

dev.off()


