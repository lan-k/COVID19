library(shiny)
library(tmap)
library(tidyverse)

load("IFR_wt_cluster.Rdata")
data("World")

clust <- World %>%
  mutate(ISO3 = iso_a3) %>%
  left_join(weights_ISO_clust, by= "ISO3") %>%
  mutate(IFR=round(100*IFR, digits=2))


ui <- fluidPage(
  
  h2("COVID-19 Infection Fatality Ratio"),
  
  tmapOutput("mymap"), 
  
  downloadLink("downloadData", "Get the data")
  
)

server <- function(input, output) {
  
  output$mymap <- renderTmap({
    tm_shape(clust) +
      tm_polygons("IFR",
                  legend.title = "IFR (%)", colorNA = "white", drop.levels = T,
                  palette="YlOrRd", inner.margins = c(0, .02, .02, .02), 
                  alpha=.7)
  })
  
  output$downloadData <- downloadHandler(
    filename = "regions_data.csv",
    content = function(file) {
      write.csv(clust, file, row.names = FALSE)
    }, 
    contentType = "text/csv"
  )
  
}

shinyApp(ui = ui, server = server)