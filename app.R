library(leaflet)
library(htmltools)
library(shiny)
library(tidyverse)

testdata <- read.csv("clean_data_current.csv")

#Making data into HTML
for (i in 1:nrow(testdata)) {
  testdata$html[i] <- paste(sep = "",
                            "<b><a href='",testdata$Website[i],"' target='_blank'>",testdata$Name[i],"</a></b><br/>",
                            testdata$Address.Line.1[i], "<br/>",testdata$City[i]," ",testdata$State[i],", 0",
                            testdata$Zip[i],"<br/>","Phone: ",testdata$Phone[i])
}



######Creating variables for papers and no papers
#Creating variables for icons
for (i in 1:nrow(testdata)){
  
  if (testdata$Type[i] == "Addiction" ){
    testdata$group[i] = "Addiction"
  }
  
  if (testdata$Type[i] == "Children" ){
    testdata$group[i] = "Children"
  }
  
  if (testdata$Type[i] == "Abuse"){
    testdata$group[i] = "Domestic/ Sexual Abuse"
  }
  
  if (testdata$Type[i] == "Employment"){
    testdata$group[i] = "Employment"
  }
  
  if (testdata$Type[i] == "Financial"){
    testdata$group[i] = "Financial Aid"
  }
  

  
  if (testdata$Type[i] == "Food" ){
    testdata$group[i] = "Food Assistance"
  }
  

  
  if (testdata$Type[i] == "Education" ){
    testdata$group[i] = "Education"
  }
  
  if (testdata$Type[i] == "Housing" ){
    testdata$group[i] = "Housing"
  }
  
  
  if (testdata$Type[i] == "Human" ){
    testdata$group[i] = "Human Services"
  }
  
  if (testdata$Type[i] == "Health" ){
    testdata$group[i] = "Healthcare"
  }
  
  if (testdata$Type[i] == "Mental" ){
    testdata$group[i] = "Mental Health Care"
  }
  
}
##### Shapes for Stamford

data <- read.csv("tracts.csv")
new_data <- data %>% group_by(Tract) %>%
  count(Tract)

CT <- geojsonio::geojson_read("stamford.geojson", what = "sp")

for (i in CT$name){
  CT$customers[CT$name ==i]<-new_data$n[new_data$Tract==i]
  print(CT$customers[CT$name ==i])
}

CT$customers <- as.numeric(CT$customers)


###### Icons


iconSet <- iconList(
  Housing = makeIcon("housing.png",iconWidth = 26, iconHeight = 26),
  Employment = makeIcon("employment.png",iconWidth = 26, iconHeight = 26),
  Financial = makeIcon("financial.png",iconWidth = 26, iconHeight = 26),
  Abuse = makeIcon("abuse.png",iconWidth = 26, iconHeight = 26),
  Mental = makeIcon("abuse.png",iconWidth = 26, iconHeight = 26),
  Addiction = makeIcon("addiction.png",iconWidth = 26, iconHeight = 26),
  Health = makeIcon("abuse.png",iconWidth = 26, iconHeight = 26),
  Food = makeIcon("food.png",iconWidth = 26, iconHeight = 26),
  Children = makeIcon("children.png",iconWidth = 26, iconHeight = 26),
  Education = makeIcon("education.png",iconWidth = 26, iconHeight = 26)
)
##### Bins for map


bins <- c(0,10,25,50,75,100,150,200,500,800,1300)
pal <- colorBin("YlGnBu", domain = CT$customers, bins = bins)


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
      overlayGroups = c("Addiction","Children","Domestic/ Sexual Abuse","Education","Employment","Financial Aid",
                         "Food Assistance","Healthcare","Housing","Mental Health Care","Show/Hide B1C Clients"),
      position = c("topleft"),
      options = layersControlOptions(collapsed = FALSE,opacity = 1)) %>% 
      addPolygons(data = CT, weight=4, opacity = 1, fillOpacity = 0.35, dashArray = "", group = "Show/Hide B1C Clients",
                  color= ~pal(customers),fillColor = ~pal(customers),
                  highlight = highlightOptions(
                    weight = 8,
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = TRUE)) %>%
      addLegend(data=CT,pal=pal, values=~customers,opacity=0.7,title="Number of B1C Clients",position="bottomright",group="Show/Hide B1C Clients") 
     
  })
  
}

shinyApp(ui, server)







