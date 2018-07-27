library(leaflet)
library(htmltools)
library(shiny)



testdata <- read.csv("test_data.csv")

#Making data into HTML
for (i in 1:nrow(testdata)) {
  testdata$html[i] <- paste(sep = "",
                            "<b><a href='",testdata$Website[i],"' target='_blank'>",testdata$Name[i],"</a></b><br/>",
                            testdata$Address.Line.1[i], "<br/>",
                            testdata$Address.Line.2[i],"<br/>","Phone: ",testdata$Phone[i])
}



######Creating variables for papers and no papers
#Creating variables for icons
for (i in 1:nrow(testdata)){
  if (testdata$Type[i] == "Employment"){
    testdata$group[i] = "Employment/ Training/ Business Development"
  }
  
  if (testdata$Type[i] == "Financial"){
    testdata$group[i] = "Financial Assistance and Material Aid"
  }
  
  if (testdata$Type[i] == "Abuse"){
    testdata$group[i] = "Abuse/ Domestic Violence/ Sexual Assault"
  }
  
  if (testdata$Type[i] == "Food" ){
    testdata$group[i] = "Food Programs"
  }
  
  if (testdata$Type[i] == "Children" ){
    testdata$group[i] = "Children and Family Services"
  }
  
  if (testdata$Type[i] == "Education" ){
    testdata$group[i] = "Education and Literary Services"
  }
  
  if (testdata$Type[i] == "Housing" ){
    testdata$group[i] = "Housing"
  }
  
}
##### Shapes for Stamford

CT <- geojsonio::geojson_read("stamford.geojson", what = "sp")
CT$customers <- as.numeric(CT$customers)



###### Icons


iconSet <- iconList(
  Housing = makeIcon("housing.png",iconWidth = 26, iconHeight = 26),
  Employment = makeIcon("employment.png",iconWidth = 26, iconHeight = 26),
  Financial = makeIcon("financial.png",iconWidth = 26, iconHeight = 26),
  Abuse = makeIcon("abuse.png",iconWidth = 26, iconHeight = 26),
  Food = makeIcon("food.png",iconWidth = 26, iconHeight = 26),
  Children = makeIcon("children.png",iconWidth = 26, iconHeight = 26),
  Education = makeIcon("education.png",iconWidth = 26, iconHeight = 26)
)
##### Bins for map


bins <- c(0,2,4,6,8,Inf)
pal <- colorBin("Blues", domain = CT$customers, bins = bins)


############# Shiny Part #################


ui <- fluidPage( 
  
  leafletOutput("mymap", height="500px")
  
)

server <- function(input, output) {


  
  output$mymap <- renderLeaflet({
    
    leaflet() %>% addTiles() %>%
      addMarkers(data=testdata, ~Longitude, ~Latitude,icon = ~iconSet[Type], popup = ~html,group=~group) 
      
      
    
    })
  
  
  observe({
    
    
    leafletProxy("mymap") %>% addLayersControl(
      overlayGroups = c("Employment/ Training/ Business Development","Financial Assistance and Material Aid",
                        "Abuse/ Domestic Violence/ Sexual Assault", "Food Programs","Children and Family Services",
                        "Education and Literary Services","Housing","Show/Hide B1C Clients"),
      position = c("topleft"),
      options = layersControlOptions(collapsed = FALSE,opacity = 1)) %>% 
      addPolygons(data = CT, weight=3, opacity = 1, fillOpacity = 0.35, dashArray = "", group = "Show/Hide B1C Clients",
                  color= ~pal(customers),fillColor = ~pal(customers),
                  highlight = highlightOptions(
                    weight = 5,
                    dashArray = "",
                    fillOpacity = 0.5,
                    bringToFront = TRUE)) %>%
      addLegend(data=CT,pal=pal, values=~customers,opacity=0.7,title="Number of B1C Clients",position="bottomright",group="Show/Hide B1C Clients")
  })
  
}

shinyApp(ui, server)







