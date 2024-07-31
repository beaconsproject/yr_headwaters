library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(leafem)
library(markdown)
library(leaflet.esri)
library(shinydashboard)
options(shiny.maxRequestSize=100*1024^2) 

spot = "https://mapservices.gov.yk.ca/imagery/rest/services/Satellites/Satellites_MedRes_Update/ImageServer"
google = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G"

#bnd <- st_read('www/yr_headwaters_4326.gpkg', 'watersheds', quiet=TRUE)
x <- st_read('www/yr_headwaters_4326.gpkg', 'watersheds', quiet=TRUE)
#streams <- st_read('www/yr_headwaters_4326.gpkg', 'streams', quiet=TRUE)
#streams <- st_read('www/yr_headwaters_4326.gpkg', 'streams_50k', quiet=TRUE)
#intact <- st_read('www/yr_headwaters_4326.gpkg', 'intactness', quiet=TRUE)
#indicators <- c('intact_pct','forest_pct','precip_mean','elev_mean','elev_max','slope_mean','slope_max')
indicators <- names(x)[6:51]

# Define UI for application
ui = dashboardPage(skin = "green",
  dashboardHeader(title = "Watershed Explorer"),
  dashboardSidebar(
    sidebarMenu(id="tabs",
      menuItem("View indicators", tabName = "get", icon = icon("th")),
      selectInput("field", "Indicator:", choices=indicators),
      selectInput("type", "Legend type:", choices=c('Numeric','Bin','Quantile','Factor'), selected='Numeric'),
      selectInput("fill", "Color:", choices=c('YlOrRd','Oranges','Reds','Blues','Greens','Dark2'), selected='YlOrRd'),
      sliderInput("alpha", label = "Opacity:", min = 0, max = 1, value = 0.5),
      hr(),
      div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download geopackage", style='color: #000'))
      )
      ),
  dashboardBody(
    tabItems(
      tabItem(tabName="get",
        fluidRow(
          tabBox(id = "one", width="8", 
            tabPanel("Layer", leafletOutput("map", height=750)),
            tabPanel("Attributes", DTOutput("table", height=750)),
            tabPanel("Help", includeMarkdown("www/help.md"))),
          tabBox(id = "two", width="4", 
            tabPanel("Indicators", DTOutput("table2", height=750)))
        )        
      )
   )
 )
)

# Define server logic
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    i = pull(x, input$field)
    if (input$type=='Numeric') {
      pal <- colorNumeric(palette=input$fill, domain=i)
    } else if (input$type=='Bin') {  
      pal <- colorBin(palette=input$fill, domain=i)
    } else if (input$type=='Quantile') {  
      pal <- colorQuantile(palette=input$fill, domain=i)
    } else if (input$type=='Factor') {  
      pal <- colorFactor(palette=input$fill, domain=i)
    }    

    #labels <- sprintf("<strong>%s</strong><br/>Indicator1 = %g<br/>Indicator2 = %g<br/>Indicator3 = %g",
    #  x$SUB_SUB_NAME, x$Indicator1, x$Indicator2, x$Indicator3) %>% 
    #lapply(htmltools::HTML)
    
    m <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      addTiles(google, group="Google.Imagery") %>%
      addEsriImageMapLayer(url=spot, group="SPOT.Imagery") %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
      addScaleBar(position='bottomleft') %>%
      #addPolygons(data=bnd, stroke=T, weight=2, fillOpacity=0, group='Boundary') %>%
      addPolygons(data=x, stroke=T, weight=1, color='black', fillColor=~pal(i),
        fillOpacity=input$alpha, layerId=x$HYBAS_ID, group='Watersheds', label=x$Station) %>%
      #addPolygons(data=intact, stroke=F, fillColor='darkgreen', fillOpacity=input$alpha, group='Intactness') %>%
      #addPolylines(data=streams, weight=1, color='blue', group='Streams') %>%
      addStaticLabels(data=x, label = x$Station, group='Watershed labels') %>% 
      addLayersControl(position = "topright",
        baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Google.Imagery","SPOT.Imagery"),
        overlayGroups = c("Watersheds", "Watershed labels"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c('Watershed labels')) %>%
      addLegend(pal=pal, values=i , opacity=0.7, title=input$field, position="bottomright")
   })

  output$table <- renderDT({
    x <- x %>% st_drop_geometry()
    datatable(x, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 15), class="compact")
  })

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click

    # Subset your table with the id of the clicked polygon 
    selected <- filter(x, HYBAS_ID==click$id) %>%
      st_drop_geometry()

    # If click id isn't null render the table
    if(!is.null(click$id)) {
      output$table2 = renderDT({
        xt <- t(selected)
        datatable(xt, rownames=T, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 20), class="compact")
      }) 
    } 
  }) 
  
  # Save features to a geopackage
  output$downloadData <- downloadHandler(
    filename = function() { paste("yr_headwaters-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
        showModal(modalDialog("Downloading...", footer=NULL))
        on.exit(removeModal())
        y <- st_transform(x, 3578)
        st_write(y, dsn=file, layer='watersheds')
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
