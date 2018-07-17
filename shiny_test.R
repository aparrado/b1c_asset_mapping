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

## Getting the icons

for (i in 1:nrow(testdata)) {
  if (testdata$Type[i] == "School"){
    testdata$icontype[i] = "school"
  }
  if(testdata$Type[i] == "Hospital"){
    testdata$icontype[i] = "hospital"
  }
  if(testdata$Type[i] == "Housing"){
    testdata$icontype[i] = "housing"
  }
  
}

######Creating variables for papers and no papers
for (i in 1:nrow(testdata)){
  if (testdata$Type[i] == "Housing" & testdata$Accept_Undocumented[i] == "Yes"){
    testdata$group[i] = "Housing, needs papers"
  }
  
  if (testdata$Type[i] == "Housing" & testdata$Accept_Undocumented[i] == "No"){
    testdata$group[i] = "Housing, no papers"
  }
  
  if (testdata$Type[i] == "Hospital" & testdata$Accept_Undocumented[i] == "Yes"){
    testdata$group[i] = "Hospital, needs papers"
  }
  
  if (testdata$Type[i] == "Hospital" & testdata$Accept_Undocumented[i] == "No"){
    testdata$group[i] = "Hospital, no papers"
  }
  
}

##### Shapes for Stamford

CT <- geojsonio::geojson_read("stamford.geojson", what = "sp")
CT$customers <- as.numeric(CT$customers)



###### Icons

iconSet <- iconList(
  housing = makeIcon("house-icon.png",iconWidth = 24, iconHeight = 24),
  hospital = makeIcon("hospital-icon.png",iconWidth = 24, iconHeight = 24),
  school = makeIcon("http://icons.iconarchive.com/icons/paomedia/small-n-flat/1024/house-icon.png",iconWidth = 32, iconHeight = 32)
)

##### Bins for map


bins <- c(0,2,4,6,8,Inf)
pal <- colorBin("YlOrRd", domain = CT$customers, bins = bins)



############# Shiny Part #################


ui <- fluidPage( 
  
  leafletOutput("mymap", height="500px")
  
)

server <- function(input, output) {


  
  output$mymap <- renderLeaflet({
    
    leaflet() %>% addTiles() %>%
      addMarkers(data=testdata, ~Longitude, ~Latitude,icon = ~iconSet[icontype], popup = ~html,group=~group) 
      
      
    
    })
  
  
  observe({
    
    
    leafletProxy("mymap") %>% addLayersControl(
      overlayGroups = c("Housing, needs papers","Housing, no papers",
                        "Hospital, needs papers", "Hospital, no papers","Show/Hide B1C Costumers"), position = c("topleft"),
      options = layersControlOptions(collapsed = FALSE,opacity = 1)) %>% 
      addPolygons(data = CT, weight=3, opacity = 1, fillOpacity = 0.2, dashArray = "", group = "Show/Hide B1C Costumers",
                  color= ~pal(customers),fillColor = ~pal(customers),
                  highlight = highlightOptions(
                    weight = 5,
                    dashArray = "",
                    fillOpacity = 0.5,
                    bringToFront = TRUE)) %>%
      addLegend(data=CT,pal=pal, values=~customers,opacity=0.7,title="Number of B1C Customers",position="bottomright",group="Show/Hide B1C Costumers")
  })
  
}

shinyApp(ui, server)







