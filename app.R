### EAGLE Master - SOS Scientific Graphics
### Shiny App for a sustainable Shopping Map for W?rzburg and more cities...
### Helena Wehner

### Tipps and Tricks
# https://rstudio.github.io/shinydashboard/appearance.html
# https://rstudio.github.io/shinydashboard/structure.html


#################################

### Data Preparation
library(raster)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(shinydashboard)
library(shinythemes)


### Define UI
ui <- dashboardPage(skin = 'green',
                    dashboardHeader(title = 'Nachhaltig, Bio & Fair Einkaufen - Bayerische Staedte', titleWidth = 500),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem('Information', tabName = 'Information', icon = icon('info')),
                            menuItem('Wuerzburg', tabName = 'Wuerzburg', icon = icon('city')),
                            menuItem('Eichstaett', tabName = 'Eichstaett', icon = icon('city')))),
                    # start dashboard Sidebar
                    dashboardBody(
                        tabItems(
                            # Information UI
                            tabItem(tabName = 'Information', title = 'Willkommen zu den Stattplaenen!',fluidRow(
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
                            tabItem(tabName = 'Wuerzburg',
                                    fluidPage(leafletOutput('map', width = '100%', height = '550')
                                    )),
                            # Eichstätt UI
                            tabItem(tabName = 'Eichstaett',
                                    fluidPage(leafletOutput('map2', width = '100%', height = '550')
                                    ))
                        )))

### Define server function 
server <- function(input, output, session){
    # write Info text for Information Output
  
    
    # Würzburg
    lebensmittel <<- data.frame(id = c(1,2,10,11,12,13,14,16,18,19,23),
                                type = c('Ebl Naturkostladen','Koehlers Vollkornbaeckerei',
                                         'Evangelische Studentengemeide','Koehlers Vollkornbaeckerei',
                                         'Koehlers Vollkornbaeckerei','Katholische Hochschulgemeinde',
                                         'Unverpackt Wuerzburg','Naehcafe Edeltraud','denns Biomarkt',
                                         'Weltladen', 'Lollo Rosso'),
                                longitude = c(9.965869, 9.965981, 9.938241, 9.933667, 9.927442,
                                              9.940461, 9.930533, 9.929387, 9.931659, 9.931170,
                                              9.950674),
                                latitude = c(49.79090, 49.79090, 49.78741, 49.77899, 49.79334, 49.79513,
                                             49.78936, 49.79636, 49.79717, 49.79254, 49.79814),
                                Kategorie = c('Lebensmittel', 'Lebensmittel','Lebensmittel','Lebensmittel',
                                              'Lebensmittel', 'Lebensmittel','Lebensmittel','Lebensmittel',
                                              'Lebensmittel','Lebensmittel','Lebensmittel'),
                                Website = c('https://www.ebl-naturkost.de/', 'https://www.koehlers-vollkornbaeckerei.de/',
                                            'https://esg-wuerzburg.de/','https://www.koehlers-vollkornbaeckerei.de/',
                                            'https://www.koehlers-vollkornbaeckerei.de/','https://www.khg-wuerzburg.de/',
                                            'https://www.wuerzburg-unverpackt.de/','https://www.cafe-edeltraud.de/',
                                            'https://www.denns-biomarkt.de/wuerzburg-juliuspromenade-64/marktseite/',
                                            'https://www.weltladen-wuerzburg.de/','https://www.lollo-rosso.de/'))
    kultur <<- data.frame(id = c(7), type = c('Kellerperle'), longitude = c(9.934207), latitude = c(49.78613),
                         Kategorie = c('Kultur'), Website = c('https://www.kellerperle.de/'))
    
    cowo <<- data.frame(id = c(22,25), type = c('FabLab','Umweltstation'), longitude = c(9.923168, 9.918895),
                        latitude = c(49.80190, 49.79506), Kategorie = c('Co-Working','Co-Working'),
                        Website = c('https://fablab-wuerzburg.de/', 'https://www.wuerzburg.de/themen/umwelt-verkehr/umweltstation/index.html'))
    gastro <<- data.frame(id = c(9,15,17,24,27), type = c('Kult','Veggie Bros','Vrohstoff','Viertelkultur','Cafe Perspektive'),
                          longitude = c(9.931054, 9.930342, 9.933856, 9.912034, 9.920989), latitude = c(49.78771, 49.78950, 49.79736,
                                                                                                        49.79484, 49.76552),
                          Kategorie = c('Gastronomie','Gastronomie','Gastronomie','Gastronomie','Gastronomie'),
                          Website = c('https://www.facebook.com/kult.wuerzburg/', 'https://www.veggiebros.de/',
                                      'https://vrohstoff.de/','https://www.viertelkultur.de/', 'https://www.kvwuerzburg.brk.de/angebote/cafe-perspektive/cafe-perspektive.html'))
    
    kk <<- data.frame(id = c(5,6,20), type = c('Exklave Vintage Shop','Liten Lycka','Naturkaufhaus'),
                      longitude = c(9.930309, 9.928757, 9.928019), latitude = c(49.79592, 49.79285, 49.79441),
                      Kategorie = c('Kleidung/Kosmetik','Kleidung/Kosmetik','Kleidung/Kosmetik'),
                      Website = c('http://www.exklave-würzburg.com/',' https://litenlycka.de/','https://naturkaufhaus.de/'))
    
    rep_wue <<- data.frame(id = c(3,4,8,21,26), type = c('Oxfam Shop','Schuhschmiede','TonArt Kreativraum','Schrauben Sepp','Angestoepselt'),
                       longitude = c(9.928437, 9.932379, 9.929533, 9.930138, 9.922726), latitude = c(49.79188, 49.78845, 49.79167, 49.79637,49.79400),
                       Kategorie = c('Reperatur&Gebraucht','Reperatur&Gebraucht','Reperatur&Gebraucht','Reperatur&Gebraucht','Reperatur&Gebraucht'),
                       Website = c('https://shops.oxfam.de/shops/wuerzburg','https://www.schuhschmiede.de/','https://tonart-wue.de/',
                                   'https://www.schrauben-sepp.de/','https://www.angestoepselt.de/'))

    # Eichstätt
    lebensmittel_ei <<- data.frame(id = c(1,2,3,4,5,6), Kategorie = c('Lebensmittel','Lebensmittel','Lebensmittel','Lebensmittel',
                                                                      'Lebensmittel','Lebensmittel'),
                                   longitude = c(11.1829, 11.17846, 11.18412, 11.17708, 11.18058, 11.18389),
                                   latitude = c(48.89448, 48.8936, 48.8938, 48.895, 48.89351, 48.89384),
                                   Name = c('Einfach so','Bella Vita','Schnellers Backstuben','Baeckerei Schneller','Baeckerei Schneller',
                                            'Weltbruecke Eichstaett'),
                                   Website = c('https://www.unverpackt-eichstaett.de/','https://www.biomarkt.de/7749_Biomarkt_Bella_Vita.html',
                                               'https://www.schnellers-backstubn.de/filialen/','https://www.schnellers-backstubn.de/filialen/',
                                               'https://www.schnellers-backstubn.de/filialen/','https://www.facebook.com/weltladen.eichstaett/'))
    gastro_ei <<- data.frame(id = c(1,2), Kategorie = c('Gastronomie','Gastronomie'), longitude = c(11.18909, 11.18388),
                             latitude = c(48.88996, 48.89385), Name = c('Kantinchen','Chocolatique'),
                             Website = c('https://www.facebook.com/Kantinchen.Eichstaett','https://www.genussmanufaktur-chocolatique.de/'))
    
    cowo_ei <<- data.frame(id = c(1,2,3), Kategorie = c('Co-Working','Co-Working','Co-Working'),
                           longitude = c(11.17959,11.19098, 11.18784), latitude = c(48.89439, 48.88947, 48.89027),
                           Name = c('Mensch in Bewegung','Kapuzinergarten','Green Office Eichstaett'),
                           Website = c('https://mensch-in-bewegung.info/', 'https://www.ku.de/konvent/ueber-uns/arbeitskreise/ak-kapuzinergarten-eden',
                                       'https://www.ku.de/unileben/nachhaltige-ku/handlungsfelder/'))
    
    ko_ei <<- data.frame(id = c(1), Kategorie = c('Kosmetik'), longitude = c(11.18048), latitude = c(48.89249),
                         Name = c('Seifenmanufaktur Lau'), Website = c('http://www.eichstätter-seifenstüberl.de/'))
    
    rep_ei <<- data.frame(id = c(1), Kategorie = c('Reperatur/Gebraucht'), longitude = c(11.19134), latitude = c(48.8962),
                          Name = c('Eichstaetter Dienste'), Website = c('http://www.soziale-dienste-gmbh.de/eichstaett/')) 

    # create a static Map of W?rzburg
    # interactivity is integrated using leaflet functions
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap, group = 'Open Street Map',
                             providerTileOptions(noWrap = T)) %>%
            addProviderTiles(providers$Stamen.Watercolor, group = 'Stamen Watercolor',providerTileOptions(noWrap = T)) %>%
            addProviderTiles(providers$Esri.WorldImagery, group = 'Esri World Imagery', providerTileOptions(noWrap = T)) %>%
            addMarkers(lebensmittel$longitude, lebensmittel$latitude, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",lebensmittel$type,"<br>",
                                      "<b>Kategorie: </b>", lebensmittel$Kategorie,"<br>",
                                      "<b>Website: </b>", lebensmittel$Website,
                                      "<a href =\"",lebensmittel$Website, "\", target=\"blank\">Link</a"),
                       group = 'Lebensmittel') %>%
            addMarkers(kultur$longitude, kultur$latitude, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",kultur$type,"<br>",
                                      "<b>Kategorie: </b>", kultur$Kategorie,"<br>",
                                      "<b>Website: </b>", kultur$Website,
                                      "<a href =\"",kultur$Website, "\", target=\"blank\">Link</a"),
                       group = 'Kultur') %>%
            addMarkers(gastro$longitude, gastro$latitude, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",gastro$type,"<br>",
                                      "<b>Kategorie: </b>", gastro$Kategorie,"<br>",
                                      "<b>Website: </b>", gastro$Website,
                                      "<a href =\"",gastro$Website, "\", target=\"blank\">Link</a"),
                       group = 'Gastronomie') %>%
            addMarkers(rep_wue$longitude, rep_wue$latitude, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",rep_wue$type,"<br>",
                                      "<b>Kategorie: </b>", rep_wue$Kategorie,"<br>",
                                      "<b>Website: </b>", rep_wue$Website,
                                      "<a href =\"",rep_wue$Website, "\", target=\"blank\">Link</a"),
                       group = 'Reperatur/Gebraucht') %>%
            addMarkers(cowo$longitude, cowo$latitude, clusterOptions = markerClusterOptions(),
                       popup = paste0("<b>Name: </b>",cowo$type,"<br>",
                                      "<b>Kategorie: </b>", cowo$Kategorie,"<br>",
                                      "<b>Website: </b>", cowo$Website,
                                      "<a href =\"",cowo$Website, "\", target=\"blank\">Link</a"),
                       group = 'Co-Working') %>%
            addMarkers(kk$longitude, kk$latitude, clusterOptions = markerClusterOptions(),
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
            addMarkers(lebensmittel_ei$longitude, lebensmittel_ei$latitude,
                       popup = paste0("<b>Name: </b>",lebensmittel_ei$Name,"<br>",
                                      "<b>Kategorie: </b>", lebensmittel_ei$Kategorie,"<br>",
                                      "<b>Website: </b>", lebensmittel_ei$Website,
                                      "<a href =\"",lebensmittel_ei$Website, "\", target=\"blank\">Link</a"),
                       group = 'Lebensmittel') %>%
            addMarkers(gastro_ei$longitude, gastro_ei$latitude,
                       popup = paste0("<b>Name: </b>",gastro_ei$Name,"<br>",
                                      "<b>Kategorie: </b>", gastro_ei$Kategorie,"<br>",
                                      "<b>Website: </b>", gastro_ei$Website,
                                      "<a href =\"",gastro_ei$Website, "\", target=\"blank\">Link</a"),
                       group = 'Gastronomie') %>%
            addMarkers(rep_ei$longitude, rep_ei$latitude,
                       popup = paste0("<b>Name: </b>",rep_ei$Name,"<br>",
                                      "<b>Kategorie: </b>", rep_ei$Kategorie,"<br>",
                                      "<b>Website: </b>", rep_ei$Website,
                                      "<a href =\"",rep_ei$Website, "\", target=\"blank\">Link</a"),
                       group = 'Reperatur/Gebraucht') %>%
            addMarkers(cowo_ei$longitude, cowo_ei$latitude,
                       popup = paste0("<b>Name: </b>",cowo_ei$Name,"<br>",
                                      "<b>Kategorie: </b>", cowo_ei$Kategorie,"<br>",
                                      "<b>Website: </b>", cowo_ei$Website,
                                      "<a href =\"",cowo_ei$Website, "\", target=\"blank\">Link</a"),
                       group = 'Co-Working') %>%
            addMarkers(ko_ei$longitude, ko_ei$latitude,
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
