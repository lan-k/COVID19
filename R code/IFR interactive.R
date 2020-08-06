library(tmap)

data("World")

load(file="./Data/IFR_wt_cluster.Rdata")
clust <- World %>%
  left_join(weights_ISO_clust, by=c("iso_a3"="ISO3")) %>%
  mutate(IFR=round(100*IFR, digits=2))
  
tmap_mode("view")  #interactive

tm <- tm_shape(clust) +
  tm_polygons("IFR", legend.title = "IFR (%)", colorNA = "white", drop.levels = T,
              palette="YlOrRd", inner.margins = c(0, .02, .02, .02), alpha=.7)  #-RdYlGn

tm

## save as stand-alone HTML file ("view" mode)
tmap_save(tm, filename = "IFR world_map.html")
