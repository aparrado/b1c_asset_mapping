library(leaflet)
library(htmltools)
library(shiny)

testdata <- read.csv("test_data.csv")

#Making data into HTML
for (i in 1:nrow(testdata)) {
  testdata$html[i] <- paste(sep = "",
                            "<b><a href='",testdata$Website[i],"'>",testdata$Name[i],"</a></b><br/>",
                            testdata$Address.Line.1[i], "<br/>",
                            testdata$Address.Line.2[i],"<br/>","Phone: ",testdata$Phone[i])
}



##### Shapes for CT


stamford <- geojsonio::geojson_read("stamford.geojson", what = "sp")
stamford$customers<-as.numeric(stamford$customers)

bins <- c(0,2,4,6,8,Inf)
pal <- colorBin("YlOrRd", domain = stamford$customers, bins = bins)



iconSet <- iconList(
  Housing = makeIcon("house-icon.png",iconWidth = 24, iconHeight = 24),
  Hospital = makeIcon("hospital-icon.png",iconWidth = 24, iconHeight = 24)
  )




m<- leaflet() %>% addTiles() %>%
  addMarkers(data=testdata, ~Longitude, ~Latitude,icon = ~iconSet[Type], popup = ~html,group=~Type) %>%
  addLayersControl(
    overlayGroups = c("Housing","No B1C Customers"),position = c("topleft"),
    options = layersControlOptions(collapsed = FALSE,fillOpacity = 0.3)
  ) %>% addPolygons(data = stamford, weight=3, opacity = 1, fillOpacity = 0.1, fillColor = ~pal(customers),color=~pal(customers),group ="No B1C Customers")
  


#####

if (testdata$Type[i] == "Housing" & testdata$Accept_Undocumented[i] == "No"){
  testdata$group[i] = "Housing, no papers"
}




