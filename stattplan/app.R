### EAGLE Master - SOS Scientific Graphics
### Shiny App for a sustainable Shopping Map for W?rzburg and more cities...
### Helena Wehner

### Tipps and Tricks
# https://rstudio.github.io/shinydashboard/appearance.html
# https://rstudio.github.io/shinydashboard/structure.html


name.spc <- c("raster", "sf", "rgdal", "ggplot2", "mapview", "leaflet", "leaflet.extras",
              "shiny", "shinythemes", "shinydashboard")
lapply(name.spc, function(x){
    sc <- try(library(x, character.only = TRUE))
    if(class(sc)[1] == "try-error"){
        install.packages(x)
        library(x, character.only = TRUE)
    }
})

server.dir <- getwd()

#################################

### Data Preparation

# Würzburg
lebensmittel <- read_sf('GIS_data/Würzburg/Lebensmittel.shp')
kultur <- read_sf('GIS_data/Würzburg/Kultur.shp')
cowo <- read_sf('GIS_data/Würzburg/Co-Working.shp')
gastro <- read_sf('GIS_data/Würzburg/Gastronomie.shp')
kk <- read_sf('GIS_data/Würzburg/Kleidung_Kosmetik.shp')
rep <- read_sf('GIS_data/Würzburg/Reperatur_Gebraucht.shp')


# Eichstätt
lebensmittel_ei <- read_sf('GIS_data/Eichstaett/Lebensmittel_ei.shp')
gastro_ei <- read_sf('GIS_data/Eichstaett/Gastronomie.shp')
cowo_ei <- read_sf('GIS_data/Eichstaett/cowo_ei.shp')
ko_ei <- read_sf('GIS_data/Eichstaett/Kosmetik.shp')
rep_ei <- read_sf('GIS_data/Eichstaett/Gebraucht_Rep.shp')

### Define UI
ui <- dashboardPage(skin = 'green',
                    dashboardHeader(title = 'Nachhaltig, Bio & Fair Einkaufen - Bayerische Städte', titleWidth = 500),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem('Information', tabName = 'Information', icon = icon('info')),
                            menuItem('Würzburg', tabName = 'Würzburg', icon = icon('city')),
                            menuItem('Eichstätt', tabName = 'Eichstätt', icon = icon('city')))),
                    # start dashboard Sidebar
                    dashboardBody(
                        tabItems(
                            # Information UI
                            tabItem(tabName = 'Information', title = 'Willkommen zu den Stattplänen!',fluidRow(
                                column(width = 12,
                                       box(
                                           title = strong('Information'),
                                           helpText('Wähle eine Stadt aus und lasse dir ziegen, wo du dort biologische,
                                     regionale und fair gehandelte Produkte einkaufen kannst!
                                     Klicke auf den Laden und lasse dir Informationen zu der Kategorie (z.B.:
                                     Lebensmittel, Reperatur/Gebraucht etc.), sowie einen Link zu der Website
                                     des Gesch?ftes anzeigen.',
                                                    div(img(src='https://www.uni-wuerzburg.de/fileadmin/_processed_/9/3/csm_Logo_gruen_transparent_web_cae8cf813f.png',
                                                            width =100), style = 'text-align: left;'))
                                       ),
                                       box(title = strong('Kontakt:'),
                                           helpText("Für Fehlermeldungen oder Ideen (neue Städte etc.)
                                       kontaktiere bitte folgende E-Mailadresse:
                                       sinnsuedost@netzwerk-n.org;",
                                                    tags$a('https://plattform-n.org/group/sinnsudost/',
                                                           href ='https://plattform-n.org/group/sinnsudost/'))
                                       ),
                                       # add a third box with Logos
                                ))),
                            # Würzburg UI
                            tabItem(tabName = 'Würzburg',
                                    fluidPage(leafletOutput('map', width = '100%', height = '700')
                                    )),
                            # Eichstätt UI
                            tabItem(tabName = 'Eichstätt',
                                    fluidPage(leafletOutput('map2', width = '100%', height = '700')
                                    ))
                        )))

### Define server function 
server <- function(input, output, session){
    # write Info text for Information Output
    
    # create a static Map of W?rzburg
    # interactivity is integrated using leaflet functions
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap, group = 'Open Street Map',
                             providerTileOptions(noWrap = T)) %>%
            addProviderTiles(providers$Stamen.Watercolor, group = 'Stamen Watercolor',providerTileOptions(noWrap = T)) %>%
            addProviderTiles(providers$Esri.WorldImagery, group = 'Esri World Imagery', providerTileOptions(noWrap = T)) %>%
            addMarkers(data= lebensmittel,
                       popup = paste0("<b>Name: </b>",lebensmittel$type,"<br>",
                                      "<b>Kategorie: </b>", lebensmittel$Kategorie,"<br>",
                                      "<b>Website: </b>", lebensmittel$Website,
                                      "<a href =\"",lebensmittel$Website, "\", target=\"blank\">Link</a"),
                       group = 'Lebensmittel') %>%
            addMarkers(data= kultur, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",kultur$type,"<br>",
                                      "<b>Kategorie: </b>", kultur$Kategorie,"<br>",
                                      "<b>Website: </b>", kultur$Website,
                                      "<a href =\"",kultur$Website, "\", target=\"blank\">Link</a"),
                       group = 'Kultur') %>%
            addMarkers(data= gastro, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",gastro$type,"<br>",
                                      "<b>Kategorie: </b>", gastro$Kategorie,"<br>",
                                      "<b>Website: </b>", gastro$Website,
                                      "<a href =\"",gastro$Website, "\", target=\"blank\">Link</a"),
                       group = 'Gastronomie') %>%
            addMarkers(data= rep, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",rep$type,"<br>",
                                      "<b>Kategorie: </b>", rep$Kategorie,"<br>",
                                      "<b>Website: </b>", rep$Website,
                                      "<a href =\"",rep$Website, "\", target=\"blank\">Link</a"),
                       group = 'Reperatur/Gebraucht') %>%
            addMarkers(data= cowo, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",cowo$type,"<br>",
                                      "<b>Kategorie: </b>", cowo$Kategorie,"<br>",
                                      "<b>Website: </b>", cowo$Website,
                                      "<a href =\"",cowo$Website, "\", target=\"blank\">Link</a"),
                       group = 'Co-Working') %>%
            addMarkers(data= kk, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",kk$type,"<br>",
                                      "<b>Kategorie: </b>", kk$Kategorie,"<br>",
                                      "<b>Website: </b>", kk$Website,
                                      "<a href =\"",kk$Website, "\", target=\"blank\">Link</a"),
                       group = 'Kleidung/Kosmetik') %>%
            addLayersControl(baseGroups = c('Open Street Map', 'Esri World Imagery', 'Stamen Watercolor'),
                             overlayGroups = c('Lebensmittel', 'Kultur', 'Gastronomie', 'Reperatur/Gebraucht','Co-Working',
                                               'Kleidung/Kosmetik'),
                             position = c('bottomleft'),
                             options = layersControlOptions(collapsed = F)) %>%
            addFullscreenControl() %>%
            addResetMapButton()
    })
    
    # create a static map of Eichst?tt
    output$map2 <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap, group = 'Open Street Map',
                             providerTileOptions(noWrap = T)) %>%
            addProviderTiles(providers$Stamen.Watercolor, group = 'Stamen Watercolor',providerTileOptions(noWrap = T)) %>%
            addProviderTiles(providers$Esri.WorldImagery, group = 'Esri World Imagery', providerTileOptions(noWrap = T)) %>%
            addMarkers(data= lebensmittel_ei,
                       popup = paste0("<b>Name: </b>",lebensmittel_ei$Name,"<br>",
                                      "<b>Kategorie: </b>", lebensmittel_ei$Kategorie,"<br>",
                                      "<b>Website: </b>", lebensmittel_ei$Website,
                                      "<a href =\"",lebensmittel_ei$Website, "\", target=\"blank\">Link</a"),
                       group = 'Lebensmittel') %>%
            addMarkers(data= gastro_ei,
                       popup = paste0("<b>Name: </b>",gastro_ei$Name,"<br>",
                                      "<b>Kategorie: </b>", gastro_ei$Kategorie,"<br>",
                                      "<b>Website: </b>", gastro_ei$Website,
                                      "<a href =\"",gastro_ei$Website, "\", target=\"blank\">Link</a"),
                       group = 'Gastronomie') %>%
            addMarkers(data= rep_ei,
                       popup = paste0("<b>Name: </b>",rep_ei$Name,"<br>",
                                      "<b>Kategorie: </b>", rep_ei$Kategorie,"<br>",
                                      "<b>Website: </b>", rep_ei$Website,
                                      "<a href =\"",rep_ei$Website, "\", target=\"blank\">Link</a"),
                       group = 'Reperatur/Gebraucht') %>%
            addMarkers(data= cowo_ei,
                       popup = paste0("<b>Name: </b>",cowo_ei$Name,"<br>",
                                      "<b>Kategorie: </b>", cowo_ei$Kategorie,"<br>",
                                      "<b>Website: </b>", cowo_ei$Website,
                                      "<a href =\"",cowo_ei$Website, "\", target=\"blank\">Link</a"),
                       group = 'Co-Working') %>%
            addMarkers(data= ko_ei,
                       popup = paste0("<b>Name: </b>",ko_ei$Name,"<br>",
                                      "<b>Kategorie: </b>", ko_ei$Kategorie,"<br>",
                                      "<b>Website: </b>", ko_ei$Website,
                                      "<a href =\"",ko_ei$Website, "\", target=\"blank\">Link</a"),
                       group = 'Kosmetik') %>%
            addLayersControl(baseGroups = c('Open Street Map', 'Esri World Imagery', 'Stamen Watercolor'),
                             overlayGroups = c('Lebensmittel', 'Gastronomie', 'Reperatur/Gebraucht','Co-Working', 'Kosmetik'),
                             position = c('bottomleft'),
                             options = layersControlOptions(collapsed = F)) %>%
            addFullscreenControl() %>%
            addResetMapButton()
    })
    # interactivity is integrated by leaflet functions
}


# Create Shiny Object
mapApp <- shinyApp(ui=ui, server = server)
mapApp
