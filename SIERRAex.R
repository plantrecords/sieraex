library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(sf)
library(plotly)
library(shinyBS)         # For collapsible panels
library(raster)          # For loading GeoTIFF raster
library(leafem)
library(shinycssloaders) # For spinner while map loads
library(smoothr)
library(shinyjs)

wild_occurrences <- reactiveVal(NULL)
ex_situ_df <- reactiveVal(NULL)
wild_df <- reactiveVal(NULL)
raw_ex_situ_df <- reactiveVal(NULL)


# Read and transform global spatial layers
moku_data <- st_read("basemaps/moku_folder/moku.shp", quiet = TRUE) %>% 
  st_transform(crs = 4326)
ahupuaa_data <- st_read("basemaps/ahupuaa_folder/ahupuaa.shp", quiet = TRUE) %>% 
  st_transform(crs = 4326)
Ecoregion_data <- st_read("basemaps/Ecoregions_folder/Hawaii_ecological_regions_final.shp", quiet = TRUE) %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_buffer(0)

# Load watershed layer globally
watershed_layer <- st_read("basemaps/watershed_dar_folder/watersheds_dar.shp", quiet = TRUE) %>% 
  st_transform(crs = 4326)

# Load Geographic Reference Area layer (gra_layer) globally
gra_layer <- st_read("https://services.arcgis.com/HQ0xoN0EzDPBOEci/arcgis/rest/services/AllPopRefs1/FeatureServer/0/query?where=1=1&outFields=*&f=geojson", quiet = TRUE)
if (is.na(st_crs(gra_layer))) {
  gra_layer <- st_set_crs(gra_layer, 4326)
}
gra_layer <- st_transform(gra_layer, crs = 4326)

# Remove any potential "lat" or "lng" columns that might confuse leaflet
Ecoregion_data <- Ecoregion_data[, !names(Ecoregion_data) %in% c("lat", "lng")]

ui <- fluidPage(
  useShinyjs(),
  tabsetPanel(id = "tabs",
              tabPanel("Gap Analysis", value = "main",
                       tags$head(
                         tags$script(HTML("
  $(document).on('shiny:sessioninitialized', function() {
    var map = $('#map1').data('leaflet-map');
    if (map) {
      map.on('baselayerchange', function(e) {
        Shiny.setInputValue('map_base_layer', e.name, {priority: 'event'});
      });
    }
  });
"))
                         
                         ,
                         tags$style(HTML("
    @media print {
      #footer { 
        display: none; 
      }
    }
  ")),
                         tags$head(
                           tags$script(HTML("
    Shiny.addCustomMessageHandler('resetFileInput', function(id) {
      var el = document.getElementById(id);
      if(el) {
        el.value = '';
      }
    });
  "))
                         )
                         ,
                         tags$script(HTML("
      Shiny.addCustomMessageHandler('filterTable', function(message) {
        // Select the actual <table> inside the container with ID = message.table
        var dtSelector = '#' + message.table + ' table';
        var table = $(dtSelector).DataTable();
        // Apply a global search for the given text
        table.search(message.search).draw();
      });
    ")),
                         tags$style(HTML("
	/* Style the tab buttons */
    .nav-tabs > li > a {
      color: #fff;
      background-color: #337ab7;
      border: 1px solid #ddd;
      border-radius: 4px 4px 0 0;
      font-weight: bold;
      padding: 10px 20px;
      margin-right: 2px;
    }
    /* Style the active tab */
    .nav-tabs > li.active > a, 
    .nav-tabs > li.active > a:hover, 
    .nav-tabs > li.active > a:focus {
      color: #fff;
      background-color: #286090;
      border-color: #ddd;
    }
          body { margin: 0; padding: 0; }
          #titlePanel {
            background-color: #000; 
            color: #fff;
            font-size: 36px;
            padding: 20px;
            text-align: center;
            display: flex;
            align-items: center;
            justify-content: center;
          }
          #titlePanel img {
            margin-right: 15px;
            height: auto;
            width: 150px;
          }
          #explainerText {
            background-color: #000; 
            color: #fff;
            font-size: 18px;
            text-align: center; 
            font-weight: bold;
            margin-bottom: 20px;
            padding: 10px;
          }
          #footer {
            background-color: #000; 
            color: #fff;
            text-align: center;
            padding: 10px;
            position: fixed; 
            bottom: 0;
            left: 0;
            width: 100%;
            border-top: 1px solid #ddd;
          }
        "))
                       ),
                       div(
                         id = "titlePanel",
                         style = "display: flex; align-items: center; justify-content: space-between;",
                         
                         # Left image
                         img(src = "https://www2.hawaii.edu/~houckkev/images/pic02.jpg", style = "height:auto; width:150px;"),
                         
                         # Center container for title and explainer text (stacked vertically)
                         div(
                           style = "text-align: center; display: flex; flex-direction: column;",
                           
                           # Remove extra margin from the main title if you want:
                           h1("SIERA-ex for Gap Analysis", style = "margin-bottom: 0;"),
                           
                           # Explainer text with no bottom margin on the paragraph
                           div(
                             id = "explainerText",
                             tags$p("Single-island Endemic Representativeness Analysis for ex situ Collections",
                                    style = "margin-bottom: 0px;")  # Very small bottom margin
                           ),
                           
                           # Links container with minimal top margin
                           div(
                             style = "font-size: 0.4em; margin-top: 0px;",
                             a(
                               href = "README.md",
                               "ReadMe",
                               target = "_blank"
                             )
                           )
                           
                         ),
                         
                         # Right image
                         img(src = "https://www2.hawaii.edu/~houckkev/images/banner.jpg", style = "height:auto; width:150px;")
                       )
                       
                       ,
                       
                       sidebarLayout(
                         sidebarPanel(
                           div(
                             style = "display: flex; align-items: center; gap: 10px;",
                             
                             # checkboxInput("showLabels", "Show Polygon Labels", value = FALSE)
                           ),
                           fileInput("uploadWild", "Upload Wild Dataset (.csv)", accept = c(".csv")),
                           h3("Wild Species Data"),
                           textInput("taxonSearch", "Search Taxon Names:", value = ""),
                           actionButton("clearTaxon", "Clear Selected"),
                           actionButton("selectAllTaxon", "Select All"),
                           div(style = "max-height: 200px; overflow-y: auto;",
                               checkboxGroupInput("taxonCheckboxes", "Select Taxon Names:", choices = NULL)
                           ),
                           
                           
                           actionButton("submit", "Submit"), br(), br(),
                           
                           fileInput("uploadExSitu", "Upload ex situ Dataset", accept = c(".csv")),
                           h3("Ex situ Data"),
                           div(style = "max-height: 200px; overflow-y: auto;",
                               checkboxGroupInput("exSituTaxonCheckboxes", "Select Taxon Names (choose only taxa that correspond to selected wild names above):", choices = NULL)),
                           actionButton("clearExSituTaxon", "Clear Selected"),
                           actionButton("submitExSitu", "Submit"), br(), br(),
                           selectInput("bufferSize", "Select Buffer Size (km):", 
                                       choices = c(0.1, 1, 2, 5, 10, 20, 50), selected = 2), br(),
                           actionButton("runAnalysis", "Run Gap Analysis", style = "color: white; background-color: #337ab7; border-color: #ddd;"), br(),br(),
                           actionButton("generateScore", "Generate Representativeness Scores"),
                           br(), br(),
                           actionButton("generateAnalysis", "Generate Representativeness Report", style = "color: white; background-color: #337ab7; border-color: #ddd;")
                           , 
                           br(),  br(),
                           plotlyOutput("scorePlot"), br(),br()),
                         mainPanel(
                           withSpinner(leafletOutput("map1", width = "100%", height = "600px")),
                           br(),
                           div(
                             style = "display: flex; align-items: center; gap: 10px;",  # Flexbox for alignment
                             selectInput("insideFilter", "Filter Wild Occurrences:",
                                         choices = c("All", "Inside ex situ buffer", "Outside ex situ buffer"),
                                         selected = "All"),
                             actionButton("clearData", "Clear Data", 
                                          style = "color: white; background-color: #337ab7; border-color: #ddd;")
                           )
                           ,
                           
                           
                           bsCollapse(
                             id = "collapseWild",
                             bsCollapsePanel("Wild Founders",
                                             tagList(
                                               actionButton("removeWild", "Remove Selected Wild Row(s)"),
                                               DTOutput("mapTableGBIF")
                                             ),
                                             style = "primary"
                             )
                           ),
                           br(),
                           bsCollapse(
                             id = "collapseExSitu",
                             bsCollapsePanel("Ex Situ Provenance",
                                             tagList(
                                               actionButton("excludeExSitu", "Exclude Selected Ex Situ Data"),
                                               DTOutput("exSituTable")
                                             ),
                                             style = "primary"
                             )
                           )
                           , br(),br()
                         )
                       ),
                       div(id = "footer",
                           tags$footer(
                             tagList(
                               "SIERA-ex was built by Kevin Houck of the National Tropical Botanical Garden in partial fulfillment of a Masters of Library and Information Science at the University of Hawaiʻi at Mānoa, using modified open-source code with permission from the Global Assessment of Metacollections for Mapping and Analysis tool while it was under development by Atlanta Botanical Garden in 2024 and 2025. Base-layers: GRA, Moku, Ahupuaa, Ecoregion, Watershed: Hawai‘i state GIS program; Wao adapted from Winter & Lucas (2018). Sample datasets available at ",
                               tags$a(href = "http://go.ntbg.org/sieraex", "http://go.ntbg.org/sieraex", target = "_blank")
                             ),
                             style = "z-index: 1000; font-size: 12px;"
                           )
                       )
                       
              )))
#,
#     tabPanel("Representativeness Report", value = "report",
#             div(id = "titlePanel",
#                img(src = "https://www2.hawaii.edu/~houckkev/images/pic02.jpg", style = "height:auto; width:150px;"),
#               titlePanel(HTML("SIERA-ex for Gap Analysis&nbsp;&nbsp;")),
#              img(src = "https://www2.hawaii.edu/~houckkev/images/banner.jpg", style = "height:auto; width:150px;")
#         ),
#        uiOutput("reportTabUI")
# )
#)


server <- function(input, output, session) {
  
  wild_occurrences <- reactiveVal(NULL)
  ex_situ_df <- reactiveVal(NULL)
  wild_df <- reactiveVal(NULL)
  
  
  observe({
    if (is.null(ex_situ_df())) {
      disable("generateAnalysis")
    } else {
      enable("generateAnalysis")
    }
  })
  
  observeEvent(input$clearExSituTaxon, {
    req(raw_ex_situ_df())
    ex_data <- raw_ex_situ_df()
    ex_taxa <- sort(unique(ex_data$Taxon.Name))
    updateCheckboxGroupInput(session, "exSituTaxonCheckboxes", 
                             choices = ex_taxa, 
                             selected = character(0))
  })
  
  
  
  observeEvent(input$clearData, {
    # Clear reactive values
    wild_df(NULL)
    ex_situ_df(NULL)
    
    # Clear the GBIF table and ex situ table
    output$mapTableGBIF <- renderDT({
      req(wild_df())
      datatable(
        wild_df(), # %>% 
        # mutate(
        #  Latitude = round(Latitude, 1),
        #  Longitude = round(Longitude, 1)
        #),
        selection = "multiple",
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',         # Table controls including Buttons
          buttons = list('colvis'),  # Column visibility button
          paging = FALSE,
          scrollY = "400px",
          scrollX = TRUE,
          scroller = TRUE,
          deferRender = TRUE,
          # Adjust these targets to match the column order of your data.
          # Here we assume that columns 2 and 3 (0-based indexing) correspond to Latitude and Longitude.
          columnDefs = list(list(visible = FALSE, targets = c(1,2)))
        )
      )
    })
    
    output$exSituTable <- renderDT({
      req(ex_situ_df())
      ex_data <- ex_situ_df()
      if (!is.null(input$exSituTaxonCheckboxes) && length(input$exSituTaxonCheckboxes) > 0) {
        ex_data <- ex_data %>% filter(Taxon.Name %in% input$exSituTaxonCheckboxes)
      }
      datatable(
        ex_data, # %>% 
        # mutate(
        # Latitude = round(Latitude, 1),
        # Longitude = round(Longitude, 1)
        # ),
        selection = "multiple",
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list('colvis'),
          paging = FALSE,
          scrollY = "400px",
          scrollX = TRUE,
          scroller = TRUE,
          deferRender = TRUE,
          columnDefs = list(list(visible = FALSE, targets = c(1,2)))
        )
      )
    })
    
    
    
    
    # Clear markers on the map
    leafletProxy("map1", session = session) %>%
      clearGroup("Wild") %>%
      clearGroup("Ex Situ") %>%
      clearGroup("Wild Buffers") %>%
      clearGroup("Ex Situ Buffers") %>%
      removeControl("wildLegend") %>%  # Removes wild points legend
      removeControl("bufferLegend")  %>%
      showGroup("Wild Buffers") %>%  # Show wild buffer overlay
      showGroup("Ex Situ")           # Show ex situ overlay
    
    
    # Reset any search filters in the tables
    session$sendCustomMessage("filterTable", list(table = "mapTableGBIF", search = ""))
    session$sendCustomMessage("filterTable", list(table = "exSituTable", search = ""))
    
    # Reset the score graph
    output$scorePlot <- renderPlotly(NULL)
    
    # Reset file inputs using the custom JS handler
    session$sendCustomMessage("resetFileInput", "uploadWild")
    session$sendCustomMessage("resetFileInput", "uploadExSitu")
    
    showNotification("All data and score graph cleared.", type = "message")
  })
  
  
  
  
  
  # Reactive filtering now uses the selected taxon names directly
  selected_data <- reactive({
    req(wild_occurrences(), input$taxonCheckboxes)
    wild_occurrences() %>% filter(Taxon.Name %in% input$taxonCheckboxes)
  })
  
  output$map1 <- renderLeaflet({
    raster_data <- brick("basemaps/wao_folder/wao.png")
    if (is.na(crs(raster_data))) {
      extent(raster_data) <- c(-159.822879045, -159.258222601, 21.848044545, 22.254177951)
      crs(raster_data) <- CRS("+proj=longlat +datum=WGS84")
    }
    watershed_layer <- st_read("basemaps/watershed_dar_folder/watersheds_dar.shp", quiet = TRUE) %>% 
      st_transform(crs = 4326)
    moku_layer <- st_read("basemaps/moku_folder/moku.shp", quiet = TRUE) %>% 
      st_transform(crs = 4326)
    
    # Use the globally loaded (and fixed) Ecoregion layer
    Ecoregion_layer <- Ecoregion_data
    
    # Smooth the Ecoregion layer for a more rounded appearance
    library(smoothr)
    smoothed_ecoregion <- smooth(Ecoregion_layer, method = "ksmooth", smoothness = 6)
    
    ahupuaa_layer <- st_read("basemaps/ahupuaa_folder/ahupuaa.shp", quiet = TRUE) %>% 
      st_transform(crs = 4326)
    gra_layer <- st_read("https://services.arcgis.com/HQ0xoN0EzDPBOEci/arcgis/rest/services/AllPopRefs1/FeatureServer/0/query?where=1=1&outFields=*&f=geojson", quiet = TRUE)
    if (is.na(st_crs(gra_layer))) {
      gra_layer <- st_set_crs(gra_layer, 4326)
    }
    gra_layer <- st_transform(gra_layer, crs = 4326)
    
    #reducing zoom increments, couldn't get it to work
    #leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0.5))%>%
    
    leaflet() %>%
      addTiles(group = "OpenStreetMap", options = providerTileOptions(name = "OpenStreetMap")) %>%
      addRasterRGB(raster_data, r = 1, g = 2, b = 3, group = "Wao", options = pathOptions(name = "Wao")) %>%
      addPolygons(data = watershed_layer, group = "Watershed", fillColor = "transparent",
                  color = "black", weight = 1, opacity = 1, label = ~NAME,
                  highlightOptions = highlightOptions(color = "yellow", weight = 2, bringToFront = TRUE),
                  options = pathOptions(name = "Watershed")) %>%
      addPolygons(
        data = smoothed_ecoregion, 
        group = "Ecoregion", 
        fillColor = "transparent",
        color = "orange", 
        weight = 1, 
        opacity = 1, 
        label = ~broad_eco,
        highlightOptions = highlightOptions(
          color = "black", 
          fillColor = "orange",
          weight = 2, 
          bringToFront = TRUE
        ),
        options = pathOptions(name = "Ecoregion", lineJoin = "round", lineCap = "round")
      ) %>%
      addPolygons(data = moku_layer, group = "Moku", fillColor = "transparent",
                  color = "purple", weight = 1, opacity = 1, label = ~MOKU,
                  highlightOptions = highlightOptions(color = "purple", fillColor = "purple", weight = 1, bringToFront = TRUE),
                  options = pathOptions(name = "Moku")) %>%
      addPolygons(data = ahupuaa_layer, group = "Ahupuaa", fillColor = "transparent",
                  color = "purple", weight = 1, opacity = 1, label = ~ahupuaa,
                  highlightOptions = highlightOptions(color = "purple", weight = 2, bringToFront = TRUE),
                  options = pathOptions(name = "Ahupuaa")) %>%
      addPolygons(data = gra_layer, group = "GeoRef", fillColor = "transparent",
                  color = "black", weight = 1, opacity = 1, label = ~POPREFNAME,
                  highlightOptions = highlightOptions(color = "black", fillColor = "yellow", weight = 2, bringToFront = TRUE),
                  options = pathOptions(name = "GeoRef")) %>%
      addMapPane("myMarkers", zIndex = 650) %>%
      addLayersControl(
        baseGroups = c("GeoRef", "Ecoregion", "Watershed", "OpenStreetMap", "Moku", "Ahupuaa", "Wao"),
        overlayGroups = c("Wild", "Ex Situ", "Wild Buffers", "Ex Situ Buffers"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Wao", "Ecoregion", "Watershed", "Moku", "Ahupuaa", "GeoRef")) %>%
      showGroup("OpenStreetMap") %>%
      setView(lng = -159.56, lat = 22.06, zoom = 11) %>%
      addLegend(
        colors = c("black", "green"),
        labels = c("Wild", "ex situ"),
        title = "Uploaded Points",
        opacity = 1,
        position = "bottomright"
      )
    
    
  })
  observe({
    print(paste("Active Base Layer:", input$map_base_layer))
  })
  
  #this is broken, can't get the base layer 
  #observe({
  #  req(input$showLabels, input$map_base_layer)
  
  #  leafletProxy("map1") %>%
  #    clearGroup("Moku") %>%
  #   clearGroup("Ahupuaa") %>%
  #    clearGroup("GeoRef") %>%
  #    clearGroup("Ecoregion") %>%
  #    clearGroup("Watershed")
  
  #  if (input$showLabels) {
  #    if (input$map_base_layer == "Moku") {
  #      leafletProxy("map1") %>%
  #        addLabelOnlyMarkers(
  #          data = st_make_valid(moku_data),
  #         lng = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,1],
  #        lat = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,2],
  #       label = ~MOKU,
  #      labelOptions = labelOptions(noHide = TRUE, direction = "auto", textOnly = TRUE),
  #     group = "Moku"
  #  )
  #    } else if (input$map_base_layer == "Ahupuaa") {
  #     leafletProxy("map1") %>%
  #      addLabelOnlyMarkers(
  #       data = st_make_valid(ahupuaa_data),
  #      lng = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,1],
  #     lat = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,2],
  #    label = ~ahupuaa,
  #   labelOptions = labelOptions(noHide = TRUE, direction = "auto", textOnly = TRUE),
  #  group = "Ahupuaa"
  #)
  #    } else if (input$map_base_layer == "Ecoregion") {
  #     leafletProxy("map1") %>%
  #      addLabelOnlyMarkers(
  #       data = st_make_valid(Ecoregion_data),
  #      lng = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,1],
  #     lat = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,2],
  #    label = ~broad_eco,
  #   labelOptions = labelOptions(noHide = TRUE, direction = "auto", textOnly = TRUE),
  #  group = "Ecoregion"
  #)
  # } else if (input$map_base_layer == "Watershed") {
  #  leafletProxy("map1") %>%
  #   addLabelOnlyMarkers(
  #    data = st_make_valid(watershed_layer),
  #   lng = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,1],
  #  lat = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,2],
  # label = ~NAME,
  #labelOptions = labelOptions(noHide = TRUE, direction = "auto", textOnly = TRUE),
  # group = "Watershed"
  #)
  #    } else if (input$map_base_layer == "GeoRef") {
  #     leafletProxy("map1") %>%
  #      addLabelOnlyMarkers(
  #       data = st_make_valid(gra_layer),
  #      lng = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,1],
  #     lat = ~st_coordinates(st_centroid(st_make_valid(geometry)))[,2],
  #    label = ~POPREFNAME,
  #   labelOptions = labelOptions(noHide = TRUE, direction = "auto", textOnly = TRUE),
  #  group = "GeoRef"
  # )
  # }
  #}
  #})
  
  
  
  
  # When a wild CSV is uploaded, read it in and update wild_occurrences
  observeEvent(input$uploadWild, {
    req(input$uploadWild)
    wild_csv <- read.csv(input$uploadWild$datapath, stringsAsFactors = FALSE)
    if (!"Taxon.Name" %in% colnames(wild_csv)) {
      showNotification("Wild CSV must contain a 'Taxon.Name' column.", type = "error")
      wild_occurrences(NULL)
    } else {
      wild_csv$id <- as.character(seq_len(nrow(wild_csv)))
      wild_occurrences(wild_csv)
    }
  })
  
  # Create a reactive expression that computes the filtered taxon names based on the search box
  filteredTaxonNames <- reactive({
    req(wild_occurrences())
    data <- wild_occurrences()
    all_names <- sort(unique(data$Taxon.Name))
    query <- input$taxonSearch
    if (!is.null(query) && query != "") {
      filtered_names <- all_names[grepl(query, all_names, ignore.case = TRUE)]
    } else {
      filtered_names <- all_names
    }
    filtered_names
  })
  
  # Update the checkbox group with the filtered Taxon.Name values
  observe({
    req(wild_occurrences())
    data <- wild_occurrences()
    all_names <- sort(unique(data$Taxon.Name))
    query <- input$taxonSearch
    if (!is.null(query) && query != "") {
      available_taxa <- all_names[grepl(query, all_names, ignore.case = TRUE)]
    } else {
      available_taxa <- all_names
    }
    current_selected <- isolate(input$taxonCheckboxes)
    preserved_selection <- current_selected[current_selected %in% available_taxa]
    updateCheckboxGroupInput(session, "taxonCheckboxes", 
                             choices = available_taxa, 
                             selected = preserved_selection)
  })
  
  
  # Clear the checked selections when "Clear Selected" is clicked
  observeEvent(input$clearTaxon, {
    updateCheckboxGroupInput(session, "taxonCheckboxes", selected = character(0))
  })
  
  observeEvent(input$excludeExSitu, {
    req(ex_situ_df())
    selected <- input$exSituTable_rows_selected
    if (length(selected) == 0) {
      showNotification("No ex situ rows selected.", type = "warning")
      return()
    }
    
    # Identify which rows are being removed
    displayed <- ex_situ_df()
    remove_ids <- displayed$id[selected]
    new_data <- displayed[!(displayed$id %in% remove_ids), ]
    
    # Update the reactive
    ex_situ_df(new_data)
    
    # Clear the DT search filter so we don't stay stuck on a removed ID
    session$sendCustomMessage("filterTable", list(
      table = "exSituTable",
      search = ""
    ))
    
    # Re-render the table with rounded coordinates
    output$exSituTable <- renderDT({
      req(ex_situ_df())
      ex_data <- ex_situ_df()
      if (!is.null(input$exSituTaxonCheckboxes) && length(input$exSituTaxonCheckboxes) > 0) {
        ex_data <- ex_data %>% filter(Taxon.Name %in% input$exSituTaxonCheckboxes)
      }
      datatable(
        ex_data, # %>% 
        # mutate(
        # Latitude = round(Latitude, 1),
        # Longitude = round(Longitude, 1)
        # ),
        selection = "multiple",
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list('colvis'),
          paging = FALSE,
          scrollY = "400px",
          scrollX = TRUE,
          scroller = TRUE,
          deferRender = TRUE,
          columnDefs = list(list(visible = FALSE, targets = c(1,2)))
        )
      )
    })
    
    
    
    
    # Update the map
    leafletProxy("map1") %>%
      clearGroup("Ex Situ") %>%
      addCircleMarkers(
        lng = ex_situ_df()$Longitude,
        lat = ex_situ_df()$Latitude,
        popup = paste(ex_situ_df()$Taxon.Name, "<br>", ex_situ_df()$Locality, "<br>", ex_situ_df()$Collection.ID),
        radius = 6,
        color = "green",
        fillOpacity = 0.7,
        group = "Ex Situ",
        layerId = paste0("exsitu_", ex_situ_df()$id)
      )
    
    showNotification("Selected ex situ rows have been excluded.", type = "message")
  })
  
  
  # Select all currently filtered names when "Select All" is clicked
  observeEvent(input$selectAllTaxon, {
    updateCheckboxGroupInput(session, "taxonCheckboxes", selected = filteredTaxonNames())
  })
  
  # When the user clicks "Submit", update wild_df and map accordingly
  observeEvent(input$submit, {
    # Extract the selected taxon names from the checkbox input
    selected_taxa <- input$taxonCheckboxes
    if (!is.null(selected_taxa) && length(selected_taxa) > 0) {
      # Extract the genus (first word) from each taxon name
      genera <- sapply(selected_taxa, function(x) strsplit(x, " ")[[1]][1])
      unique_genera <- unique(genera)
      # If more than one unique genus is selected, show a warning modal
      if (length(unique_genera) > 1) {
        showModal(modalDialog(
          title = "Warning",
          paste("Warning: map will display markers for both", 
                paste(unique_genera, collapse = ", "),
                "which may not readily hybridize."),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }
    data <- selected_data()
    wild_df(data)
    leafletProxy("map1", session = session) %>%
      clearGroup("Wild") %>%
      addCircleMarkers(
        lng = data$Longitude,
        lat = data$Latitude,
        popup = paste(data$current.germplasm.type, " #", data$id, "<br>",
                      data$fullname, "<br>", data$Locality, "<br>", data$Collection.ID),
        radius = 6,
        color = "blue",
        fillOpacity = 0.7,
        group = "Wild",
        layerId = paste0("wild_", data$id),
        options = pathOptions(pane = "myMarkers")
      ) %>%
      fitBounds(
        lng1 = min(data$Longitude),
        lat1 = min(data$Latitude),
        lng2 = max(data$Longitude),
        lat2 = max(data$Latitude)
      )
    
    output$mapTableGBIF <- renderDT({
      req(wild_df())
      datatable(
        wild_df(), # %>% 
        # mutate(
        # Latitude = round(Latitude, 1),
        # Longitude = round(Longitude, 1)
        # ),
        selection = "multiple",
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',         # Table controls including Buttons
          buttons = list('colvis'),  # Column visibility button
          paging = FALSE,
          scrollY = "400px",
          scrollX = TRUE,
          scroller = TRUE,
          deferRender = TRUE,
          # Adjust these targets to match the column order of your data.
          # Here we assume that columns 2 and 3 (0-based indexing) correspond to Latitude and Longitude.
          columnDefs = list(list(visible = FALSE, targets = c(1,2)))
        )
      )
    })
    
  })
  
  observeEvent(raw_ex_situ_df(), {
    req(raw_ex_situ_df())
    ex_data <- raw_ex_situ_df()
    ex_taxa <- sort(unique(ex_data$Taxon.Name))
    updateCheckboxGroupInput(session, "exSituTaxonCheckboxes", choices = ex_taxa)
  })
  
  
  # When ex situ CSV is uploaded, read it and update ex_situ_df
  observeEvent(input$uploadExSitu, {
    req(input$uploadExSitu)
    ex_csv <- read.csv(input$uploadExSitu$datapath, stringsAsFactors = FALSE)
    if (!("Latitude" %in% colnames(ex_csv) && "Longitude" %in% colnames(ex_csv))) {
      showNotification("Ex situ data must contain 'Latitude' and 'Longitude' columns.", type = "error")
      raw_ex_situ_df(NULL)
    } else {
      ex_csv$id <- as.character(seq_len(nrow(ex_csv)))
      raw_ex_situ_df(ex_csv)
    }
  })
  
  
  exSituTableProxy <- dataTableProxy("exSituTable")
  
  observe({
    req(raw_ex_situ_df())
    ex_data <- raw_ex_situ_df()
    ex_taxa <- sort(unique(ex_data$Taxon.Name))
    current_selected <- isolate(input$exSituTaxonCheckboxes)
    preserved_selection <- current_selected[current_selected %in% ex_taxa]
    updateCheckboxGroupInput(session, "exSituTaxonCheckboxes", 
                             choices = ex_taxa, 
                             selected = preserved_selection)
  })
  
  
  
  # Render the ex situ data table
  output$exSituTable <- renderDT({
    req(ex_situ_df())
    ex_data <- ex_situ_df()
    if (!is.null(input$exSituTaxonCheckboxes) && length(input$exSituTaxonCheckboxes) > 0) {
      ex_data <- ex_data %>% filter(Taxon.Name %in% input$exSituTaxonCheckboxes)
    }
    datatable(
      ex_data, # %>% 
      # mutate(
      # Latitude = round(Latitude, 1),
      # Longitude = round(Longitude, 1)
      # ),
      selection = "multiple",
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        paging = FALSE,
        scrollY = "400px",
        scrollX = TRUE,
        scroller = TRUE,
        deferRender = TRUE,
        columnDefs = list(list(visible = FALSE, targets = c(1,2)))
      )
    )
  })
  
  
  
  
  # When a map marker is clicked, select the corresponding row in the data table
  observeEvent(input$map1_marker_click, {
    req(input$map1_marker_click$id)
    clicked_id <- input$map1_marker_click$id
    
    if (startsWith(clicked_id, "wild_")) {
      actual_id <- sub("^wild_", "", clicked_id)
      req(wild_df())
      df <- wild_df()
      
      row_to_select <- which(as.character(df$id) == as.character(actual_id))
      if (length(row_to_select) > 0) {
        # Expand the collapse panel
        updateCollapse(session, "collapseWild", open = "Wild Founders")
        
        # Highlight the row in the DT
        dataTableProxy("mapTableGBIF") %>% selectRows(row_to_select)
        
        # Filter the table so only that record shows
        session$sendCustomMessage("filterTable", list(
          table = "mapTableGBIF",  # The DTOutput ID
          search = actual_id      # The text to filter by
        ))
      }
    } else if (startsWith(clicked_id, "exsitu_")) {
      actual_id <- sub("^exsitu_", "", clicked_id)
      req(ex_situ_df())
      ex_df <- ex_situ_df()
      
      row_to_select <- which(as.character(ex_df$id) == as.character(actual_id))
      if (length(row_to_select) > 0) {
        updateCollapse(session, "collapseExSitu", open = "Ex Situ Provenance")
        dataTableProxy("exSituTable") %>% selectRows(row_to_select)
        session$sendCustomMessage("filterTable", list(
          table = "exSituTable",
          search = actual_id
        ))
      }
    }
  })
  
  observeEvent(input$mapTableGBIF_rows_selected, {
    req(input$mapTableGBIF_rows_selected)
    req(wild_df())  # Ensure data exists
    
    selected_index <- input$mapTableGBIF_rows_selected
    selected_row <- wild_df()[selected_index, ]
    
    if (!is.null(selected_row)) {
      leafletProxy("map1") %>%
        clearPopups() %>%
        addPopups(
          lng = selected_row$Longitude,
          lat = selected_row$Latitude,
          popup = paste(
            "<strong>", selected_row$Taxon.Name, "</strong><br>",
            "Locality: ", selected_row$Locality, "<br>",
            "Collection ID: ", selected_row$Collection.ID
          )
        )
    }
  })
  
  observeEvent(input$exSituTable_rows_selected, {
    req(input$exSituTable_rows_selected)
    req(ex_situ_df())  # Ensure data exists
    
    selected_index <- input$exSituTable_rows_selected
    selected_row <- ex_situ_df()[selected_index, ]
    
    if (!is.null(selected_row)) {
      leafletProxy("map1") %>%
        clearPopups() %>%
        addPopups(
          lng = selected_row$Longitude,
          lat = selected_row$Latitude,
          popup = paste(
            "<strong>", selected_row$Taxon.Name, "</strong><br>",
            "Locality: ", selected_row$Locality, "<br>",
            "Collection ID: ", selected_row$Collection.ID
          )
        )
    }
  })
  
  
  
  
  # When "Submit Ex Situ" is clicked, add ex situ points to the map
  observeEvent(input$submitExSitu, {
    req(raw_ex_situ_df())
    # Filter raw data by selected taxon names; if none are selected, use all data.
    ex_data_filtered <- if (is.null(input$exSituTaxonCheckboxes) || length(input$exSituTaxonCheckboxes) == 0) {
      raw_ex_situ_df()
    } else {
      raw_ex_situ_df() %>% filter(Taxon.Name %in% input$exSituTaxonCheckboxes)
    }
    
    # Update the reactive used for markers and the table
    ex_situ_df(ex_data_filtered)
    
    # Update the map markers
    leafletProxy("map1", session = session) %>%
      clearPopups() %>%  
      clearGroup("Ex Situ") %>%
      addCircleMarkers(
        lng = ex_data_filtered$Longitude,
        lat = ex_data_filtered$Latitude,
        popup = paste(ex_data_filtered$Taxon.Name, "<br>", ex_data_filtered$Locality, "<br>", ex_data_filtered$`Collection.ID`),
        radius = 6,
        color = "green",
        fillOpacity = 0.7,
        group = "Ex Situ",
        layerId = paste0("exsitu_", ex_data_filtered$id),
        options = pathOptions(pane = "myMarkers")
      )
    
    # Now update the data table (only with filtered data)
    output$exSituTable <- renderDT({
      req(ex_situ_df())
      datatable(
        ex_situ_df(), 
        selection = "multiple",
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list('colvis'),
          paging = FALSE,
          scrollY = "400px",
          scrollX = TRUE,
          scroller = TRUE,
          deferRender = TRUE,
          columnDefs = list(list(visible = FALSE, targets = c(1,2)))
        )
      )
    })
  })
  
  
  
  # When "Remove Selected Wild" is clicked, remove the corresponding rows and update the map and table
  observeEvent(input$removeWild, {
    req(wild_occurrences(), wild_df())
    selected <- input$mapTableGBIF_rows_selected
    if (length(selected) > 0) {
      # Retrieve the full dataset and the currently filtered (displayed) subset
      current <- wild_occurrences()
      displayed <- wild_df()
      
      # Identify the IDs to remove from the displayed subset
      remove_ids <- displayed$id[selected]
      
      # Remove those rows from the full dataset and update the reactive value
      new_data <- current[!(current$id %in% remove_ids), ]
      wild_occurrences(new_data)
      
      # Recalculate the filtered dataset using the updated data
      updated_filtered <- new_data %>% filter(Taxon.Name %in% input$taxonCheckboxes)
      wild_df(updated_filtered)
      
      # Re-render the GBIF data table with the updated filtered data
      output$mapTableGBIF <- renderDT({
        req(wild_df())
        datatable(
          wild_df(), # %>% 
          # mutate(
          #  Latitude = round(Latitude, 1),
          # Longitude = round(Longitude, 1)
          # ),
          selection = "multiple",
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',         # Table controls including Buttons
            buttons = list('colvis'),  # Column visibility button
            paging = FALSE,
            scrollY = "400px",
            scrollX = TRUE,
            scroller = TRUE,
            deferRender = TRUE,
            # Adjust these targets to match the column order of your data.
            # Here we assume that columns 2 and 3 (0-based indexing) correspond to Latitude and Longitude.
            columnDefs = list(list(visible = FALSE, targets = c(1,2)))
          )
        )
      })
      
      
      # Refresh the map markers using the updated filtered wild points
      leafletProxy("map1", session = session) %>%
        clearPopups() %>%  # Clears popups when removing data
        clearGroup("Wild") %>%
        addCircleMarkers(
          lng = updated_filtered$Longitude,
          lat = updated_filtered$Latitude,
          popup = paste(updated_filtered$current.germplasm.type, " #", updated_filtered$id, "<br>",
                        updated_filtered$fullname, "<br>", updated_filtered$Locality, "<br>", updated_filtered$Collection.ID),
          radius = 6,
          color = "blue",
          fillOpacity = 0.7,
          group = "Wild",
          layerId = paste0("wild_", updated_filtered$id)
        )
      
      showNotification("Selected wild rows have been removed.", type = "message")
    } else {
      showNotification("No wild rows selected.", type = "warning")
    }
  })
  
  
  
  updateGapAnalysis <- function() {
    withProgress(message = "Running gap analysis...", value = 0, {
      wild_data <- selected_data()
      ex_data <- ex_situ_df()
      req(wild_data, ex_data)
      
      incProgress(0.2, detail = "Converting data...")
      sf_wild <- st_as_sf(wild_data, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
      sf_ex_situ <- st_as_sf(ex_data, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
      
      buffer_m <- as.numeric(input$bufferSize) * 1000  # Buffer in meters
      
      # Reproject to a projected CRS (EPSG:3857, Web Mercator) for accurate buffering
      sf_wild_proj <- st_transform(sf_wild, 3857)
      sf_ex_situ_proj <- st_transform(sf_ex_situ, 3857)
      
      incProgress(0.1, detail = "Buffering wild occurrences in projected CRS...")
      wild_buffer_zones_proj <- st_buffer(sf_wild_proj, dist = buffer_m) %>% st_make_valid() %>% st_buffer(0)
      
      incProgress(0.1, detail = "Buffering ex situ data in projected CRS...")
      ex_situ_buffer_zones_proj <- st_buffer(sf_ex_situ_proj, dist = buffer_m) %>% st_make_valid() %>% st_buffer(0)
      
      incProgress(0.2, detail = "Creating union...")
      ex_situ_union_proj <- st_union(ex_situ_buffer_zones_proj)
      # Simplify and fix the union geometry to avoid degenerate edges.
      ex_situ_union_proj <- st_simplify(ex_situ_union_proj, dTolerance = 1) %>% st_make_valid() %>% st_buffer(0)
      # Transform the unioned geometry back to EPSG:4326 for intersection testing.
      ex_situ_union <- st_transform(ex_situ_union_proj, 4326)
      
      
      # Also transform the buffered zones back for mapping
      wild_buffer_zones <- st_transform(wild_buffer_zones_proj, 4326)
      ex_situ_buffer_zones <- st_transform(ex_situ_buffer_zones_proj, 4326)
      
      incProgress(0.1, detail = "Intersecting...")
      inside <- st_intersects(sf_wild, ex_situ_union, sparse = FALSE)[,1]
      wild_marker_colors <- ifelse(inside, "blue", "red")
      
      incProgress(0.1, detail = "Updating wild data...")
      wild_data_df <- as.data.frame(sf_wild)
      wild_data_df$inside <- inside
      wild_data_df$id <- as.character(wild_data_df$id)
      wild_df(wild_data_df)
      
      incProgress(0.1, detail = "Updating table...")
      output$mapTableGBIF <- renderDT({
        req(wild_df())
        df <- wild_df()
        if (input$insideFilter == "Inside ex situ buffer") {
          df <- df[df$inside == TRUE, ]
        } else if (input$insideFilter == "Outside ex situ buffer") {
          df <- df[df$inside == FALSE, ]
        }
        datatable(
          st_drop_geometry(df),
          # df %>% 
          # mutate(
          #  Latitude = round(Latitude, 2),
          #  Longitude = round(Longitude, 2)
          # ) %>% 
          # select(-geometry),
          selection = "multiple", 
          extensions = 'Buttons', # after gap analysis, adding column visibility controls
          options = list(
            dom = 'Bfrtip',
            buttons = list('colvis'),
            paging = FALSE,
            scrollY = "400px",
            scrollX = TRUE,
            scroller = TRUE,
            deferRender = TRUE,
            columnDefs = list(list(visible = FALSE, targets = c(1,2))))
        ) %>%
          formatStyle(
            'inside',
            target = 'row',
            backgroundColor = styleEqual(c(TRUE, FALSE), c('#d9edf7', '#f2dede'))
          )
      })
      
      incProgress(0.1, detail = "Updating map...")
      leafletProxy("map1", session = session) %>%
        clearGroup("Wild Buffers") %>%
        clearGroup("Ex Situ Buffers") %>%
        addPolygons(data = wild_buffer_zones, color = "blue", weight = 2, opacity = 0.5,
                    fillOpacity = 0.2, group = "Wild Buffers") %>%
        addPolygons(data = ex_situ_buffer_zones, color = "green", weight = 2, opacity = 0.5,
                    fillOpacity = 0.2, group = "Ex Situ Buffers") %>%
        clearGroup("Wild") %>%
        addCircleMarkers(
          lng = wild_data$Longitude,
          lat = wild_data$Latitude,
          popup = paste(
            wild_data$current.germplasm.type, " #", wild_data$id, "<br>",
            wild_data$fullname, "<br>", wild_data$Locality, "<br>", wild_data$Collection.ID
          ),
          radius = 4,
          color = wild_marker_colors,
          fillOpacity = 0.7,
          group = "Wild",
          layerId = paste0("wild_", wild_data$id)
        ) %>%
        hideGroup("Wild Buffers") %>%  # Hide wild buffer overlay
        hideGroup("Ex Situ")          # Hide ex situ overlay
      
      incProgress(0.1, detail = "Done.")
    })
  }
  
  
  observeEvent(input$runAnalysis, {
    #showModal(modalDialog(
    #    title = "Running Gap Analysis",
    #    "Large datasets may take several minutes to process",
    #    easyClose = TRUE,
    #    footer = modalButton("OK")
    #  ))
    
    updateGapAnalysis()
    
    # Add legend for wild points (red/blue)
    leafletProxy("map1", session = session) %>%
      addLegend(
        position = "bottomright",
        colors = c("blue", "red"),
        labels = c("Inside Ex Situ Buffers", "Outside Ex Situ Buffers"),
        title = "Wild Points",
        opacity = 1,
        layerId = "WildLegend"
      ) %>%
      addLegend(
        colors = c("cornflowerblue", "lightgreen"),
        labels = c("Wild", "ex situ"),
        title = "Buffers",
        opacity = 1,
        layerId = "bufferLegend"
      )
  })
  
  observe({
    req(wild_df())
    df <- wild_df()
    if (input$insideFilter == "Inside ex situ buffer") {
      df <- df[df$inside == TRUE, ]
    } else if (input$insideFilter == "Outside ex situ buffer") {
      df <- df[df$inside == FALSE, ]
    }
    marker_colors <- if ("inside" %in% names(df)) {
      ifelse(df$inside, "blue", "red")
    } else {
      rep("black", nrow(df))
    }
    leafletProxy("map1") %>%
      clearGroup("Wild") %>%
      addCircleMarkers(
        lng = df$Longitude,
        lat = df$Latitude,
        popup = paste(df$fullname, "<br>", df$Locality, "<br>", df$Collection.ID),
        radius = 6,
        color = marker_colors,
        fillOpacity = 0.7,
        group = "Wild",
        layerId = paste0("wild_", df$id)
      )
  })
  
  observeEvent(input$generateScore, {
    req(selected_data(), ex_situ_df())  # Ensure both datasets exist
    
    # Convert wild and ex situ data to sf objects (EPSG:4326)
    wild_data <- selected_data()
    ex_data <- ex_situ_df()
    sf_wild <- st_as_sf(wild_data, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
    sf_ex_situ <- st_as_sf(ex_data, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
    
    # Define buffer distance in meters
    buffer_m <- as.numeric(input$bufferSize) * 1000
    
    # Reproject both datasets to a projected CRS (EPSG:3857) for accurate buffering
    sf_wild_proj <- st_transform(sf_wild, 3857)
    sf_ex_situ_proj <- st_transform(sf_ex_situ, 3857)
    
    # Buffer ex situ points in projected CRS, union them, simplify to remove degenerate edges,
    # and then transform back to EPSG:4326
    ex_buffers_proj <- st_buffer(sf_ex_situ_proj, dist = buffer_m) %>%
      st_make_valid() %>%
      st_union()
    # Simplify and fix the union geometry (adjust dTolerance as needed)
    ex_buffers_proj <- st_simplify(ex_buffers_proj, dTolerance = 1) %>% st_make_valid() %>% st_buffer(0)
    ex_buffers <- st_transform(ex_buffers_proj, 4326)
    
    # Identify wild points inside the ex situ buffer
    inside <- st_intersects(sf_wild, ex_buffers, sparse = FALSE)[,1]
    sf_wild$inside <- inside  # Store inside/outside status
    
    # Calculate Wild Coverage Scores
    wild_covered <- sum(sf_wild$inside)
    total_wild <- nrow(sf_wild)
    wild_outside <- total_wild - wild_covered
    
    wild_coverage_score <- ifelse(total_wild > 0, (wild_covered / total_wild) * 100, 0)
    wild_outside_score <- 100 - wild_coverage_score
    
    score_data <- data.frame(
      Category = rep("Wild Coverage", 2),
      Status = c("Inside Ex Situ Buffers", "Outside Ex Situ Buffers"),
      Percentage = c(round(wild_coverage_score, 1), round(wild_outside_score, 1)),
      Color = c("#2ECC71", "#E74C3C")
    )
    
    output$scorePlot <- renderPlotly({
      plot_ly(score_data, 
              x = ~Category, 
              y = ~Percentage, 
              type = 'bar',
              text = ~paste(Percentage, "%"),
              textposition = 'inside',
              marker = list(color = score_data$Color),
              hoverinfo = "text",
              name = ~Status) %>%
        layout(
          title = "Wild Coverage Representativeness",
          xaxis = list(title = ""),
          yaxis = list(
            range = c(0, 100),
            tickvals = seq(0, 100, by = 20),
            ticktext = paste0(seq(0, 100, by = 20), "%"),
            title = "Percentage (%)"
          ),
          barmode = "stack",
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2,
            yanchor = "top"
          )
        )
    })
    
    showNotification("Wild Coverage Score Calculated", type = "message")
  })
  
  
  
  observeEvent(input$generateAnalysis, {
    req(wild_df())
    withProgress(message = "Generating Representativeness Analysis Report...", value = 0, {
      
      # Generate a unique tab ID based on timestamp
      timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      new_tab_id <- paste0("report_", timestamp)
      
      incProgress(0.05, detail = "Processing wild data...")
      sf_wild <- st_as_sf(wild_df(), coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
      
      incProgress(0.05, detail = "Reading Watershed data...")
      watershed_data <- st_read("basemaps/watershed_dar_folder/watersheds_dar.shp", quiet = TRUE)
      
      incProgress(0.05, detail = "Transforming data...")
      watershed_data <- st_transform(watershed_data, crs = 4326) %>% st_make_valid() %>% st_buffer(0)
      
      incProgress(0.05, detail = "Joining Watershed data...")
      watershed_join <- st_join(sf_wild, watershed_data, left = TRUE)
      
      incProgress(0.05, detail = "Summarizing Watershed data...")
      watershed_report <- watershed_join %>% as.data.frame() %>% group_by(NAME) %>% summarise(overlapped = any(inside), count = n())
      
      incProgress(0.05, detail = "Extracting Watershed values...")
      watershed_green <- watershed_report %>% filter(overlapped == TRUE, !is.na(NAME)) %>% pull(NAME)
      watershed_red   <- watershed_report %>% filter(overlapped == FALSE, !is.na(NAME)) %>% pull(NAME)
      
      incProgress(0.1, detail = "Analyzing Ecoregion...")
      Ecoregion_fixed <- st_make_valid(Ecoregion_data) %>% st_buffer(0)
      Ecoregion_join <- st_join(sf_wild, Ecoregion_fixed, left = TRUE)
      Ecoregion_report <- Ecoregion_join %>% as.data.frame() %>% group_by(broad_eco) %>% summarise(overlapped = any(inside), count = n())
      Ecoregion_green <- Ecoregion_report %>% filter(overlapped == TRUE) %>% pull(broad_eco)
      Ecoregion_red   <- Ecoregion_report %>% filter(overlapped == FALSE) %>% pull(broad_eco)
      
      incProgress(0.1, detail = "Analyzing Moku...")
      moku_fixed <- st_make_valid(moku_data) %>% st_buffer(0)
      moku_join <- st_join(sf_wild, moku_fixed, left = TRUE)
      moku_report <- moku_join %>% as.data.frame() %>% group_by(MOKU) %>% summarise(overlapped = any(inside), count = n())
      moku_green <- moku_report %>% filter(overlapped == TRUE, !is.na(MOKU), MOKU != "") %>% pull(MOKU)
      moku_red   <- moku_report %>% filter(overlapped == FALSE, !is.na(MOKU), MOKU != "") %>% pull(MOKU)
      
      incProgress(0.1, detail = "Analyzing Ahupuaa...")
      ahupuaa_fixed <- st_make_valid(ahupuaa_data) %>% st_buffer(0)
      ahupuaa_join <- st_join(sf_wild, ahupuaa_fixed, left = TRUE)
      ahupuaa_report <- ahupuaa_join %>% as.data.frame() %>% group_by(ahupuaa) %>% summarise(overlapped = any(inside), count = n())
      ahupuaa_green <- ahupuaa_report %>% filter(overlapped == TRUE, !is.na(ahupuaa)) %>% pull(ahupuaa)
      ahupuaa_red   <- ahupuaa_report %>% filter(overlapped == FALSE, !is.na(ahupuaa)) %>% pull(ahupuaa)
      
      incProgress(0.1, detail = "Analyzing Localities...")
      locality_report <- wild_df() %>% group_by(Locality) %>% summarise(overlapped = any(inside), count = n())
      locality_green <- locality_report %>% filter(overlapped == TRUE) %>% pull(Locality)
      locality_red   <- locality_report %>% filter(overlapped == FALSE) %>% pull(Locality)
      
      incProgress(0.1, detail = "Analyzing GRA...")
      gra_data <- st_read("https://services.arcgis.com/HQ0xoN0EzDPBOEci/arcgis/rest/services/AllPopRefs1/FeatureServer/0/query?where=1=1&outFields=*&f=geojson", quiet = TRUE)
      if (is.na(st_crs(gra_data))) {
        gra_data <- st_set_crs(gra_data, 4326)
      }
      gra_data <- gra_data %>% st_transform(crs = 4326) %>% st_simplify(dTolerance = 0.0001) %>% st_make_valid() %>% st_buffer(0)
      
      incProgress(0.05, detail = "Joining GRA data...")
      gra_join <- st_join(sf_wild, gra_data, left = TRUE)
      
      incProgress(0.05, detail = "Summarizing GRA data...")
      gra_report <- gra_join %>% as.data.frame() %>% group_by(POPREFNAME) %>% summarise(overlapped = any(inside), count = n())
      gra_green <- gra_report %>% filter(overlapped == TRUE, !is.na(POPREFNAME)) %>% pull(POPREFNAME)
      gra_red   <- gra_report %>% filter(overlapped == FALSE, !is.na(POPREFNAME)) %>% pull(POPREFNAME)
      
      incProgress(0.1, detail = "Building Report")
      
      # Define collapsible panels for each analysis
      gra_panel <- bsCollapsePanel(
        "Geographic Reference Area Representation",
        tagList(
          if(length(gra_green) > 0) {
            tags$div("Overlapped by ex situ buffers:",
                     tags$ul(lapply(gra_green, function(x) {
                       tags$li(tags$span(style = "color: green;", x))
                     }))
            )
          } else {
            ""
          },
          if(length(gra_red) > 0) {
            tags$div("Not overlapped by ex situ buffers:",
                     tags$ul(lapply(gra_red, function(x) {
                       tags$li(tags$span(style = "color: red;", x))
                     }))
            )
          } else {
            ""
          }
        ),
        style = "primary", collapsible = TRUE, open = FALSE
      )
      
      watershed_panel <- bsCollapsePanel(
        "Watershed Representation",
        tagList(
          if (length(watershed_green) > 0) {
            tags$div("Overlapped by ex situ buffers:",
                     tags$ul(lapply(watershed_green, function(x) {
                       tags$li(tags$span(style = "color: green;", x))
                     }))
            )
          } else {
            ""
          },
          if (length(watershed_red) > 0) {
            tags$div("Not overlapped by ex situ buffers:",
                     tags$ul(lapply(watershed_red, function(x) {
                       tags$li(tags$span(style = "color: red;", x))
                     }))
            )
          } else {
            ""
          }
        ),
        style = "primary", collapsible = TRUE, open = FALSE
      )
      
      Ecoregion_panel <- bsCollapsePanel(
        "Ecoregion Representation",
        tagList(
          if(length(Ecoregion_green) > 0) {
            tags$div("Overlapped by ex situ buffers:",
                     tags$ul(lapply(Ecoregion_green, function(x) {
                       tags$li(tags$span(style = "color: green;", x))
                     }))
            )
          } else {
            ""
          },
          if(length(Ecoregion_red) > 0) {
            tags$div("Not overlapped by ex situ buffers:",
                     tags$ul(lapply(Ecoregion_red, function(x) {
                       tags$li(tags$span(style = "color: red;", x))
                     }))
            )
          } else {
            ""
          }
        ),
        style = "primary", collapsible = TRUE, open = FALSE
      )
      
      moku_panel <- bsCollapsePanel(
        "Moku Representation",
        tagList(
          if(length(moku_green) > 0) {
            tags$div("Overlapped by ex situ buffers:",
                     tags$ul(lapply(moku_green, function(x) {
                       tags$li(tags$span(style = "color: green;", x))
                     }))
            )
          } else {
            ""
          },
          if(length(moku_red) > 0) {
            tags$div("Not overlapped by ex situ buffers:",
                     tags$ul(lapply(moku_red, function(x) {
                       tags$li(tags$span(style = "color: red;", x))
                     }))
            )
          } else {
            ""
          }
        ),
        style = "primary", collapsible = TRUE, open = FALSE
      )
      
      ahupuaa_panel <- bsCollapsePanel(
        "Ahupuaa Representation",
        tagList(
          if(length(ahupuaa_green) > 0) {
            tags$div("Overlapped by ex situ buffers:",
                     tags$ul(lapply(ahupuaa_green, function(x) {
                       tags$li(tags$span(style = "color: green;", x))
                     }))
            )
          } else {
            ""
          },
          if(length(ahupuaa_red) > 0) {
            tags$div("Not overlapped by ex situ buffers:",
                     tags$ul(lapply(ahupuaa_red, function(x) {
                       tags$li(tags$span(style = "color: red;", x))
                     }))
            )
          } else {
            ""
          }
        ),
        style = "primary", collapsible = TRUE, open = FALSE
      )
      
      locality_panel <- bsCollapsePanel(
        "Locality Representation",
        do.call(tagList, c(
          if (length(locality_green) > 0) {
            list(
              tags$div("Overlapped by ex situ buffers:",
                       tags$ul(lapply(locality_green, function(x) {
                         tags$li(tags$span(style = "color: green;", x))
                       }))
              )
            )
          } else {
            list()
          },
          if (length(locality_red) > 0) {
            list(
              tags$div("Not overlapped by ex situ buffers:",
                       tags$ul(lapply(locality_red, function(x) {
                         tags$li(tags$span(style = "color: red;", x))
                       }))
              )
            )
          } else {
            list()
          }
        )),
        style = "primary", collapsible = TRUE, open = FALSE
      )
      
      # Group panels into one collapsible UI element
      report_ui <- bsCollapse(
        gra_panel,
        watershed_panel,
        Ecoregion_panel,
        moku_panel,
        ahupuaa_panel,
        locality_panel,
        multiple = TRUE
      )
      
      incProgress(0.1, detail = "Finalizing Report")
      
      # Define the base report title
      selected_taxa <- unique(selected_data()$Taxon.Name)
      if (length(selected_taxa) == 1) {
        base_title <- selected_taxa[1]
      } else {
        first_taxon <- selected_taxa[1]
        first_space <- regexpr(" ", first_taxon)
        if (first_space > 0) {
          base_title <- paste0(substr(first_taxon, 1, first_space - 1), " spp.")
        } else {
          base_title <- paste0(first_taxon, " spp.")
        }
      }
      
      final_report_title <- paste0(input$bufferSize, "km Representativeness Report for ", base_title)
      
      output[[new_tab_id]] <- renderUI({
        tagList(
          h2(final_report_title),
          report_ui
        )
      })
      
      tagList(
        h2(final_report_title),
        report_ui
      )
    })
    
    # Append a new tab dynamically and switch to it
    appendTab(
      inputId = "tabs",
      tabPanel(
        title = final_report_title,
        value = new_tab_id,
        uiOutput(new_tab_id)
      )
    )
    updateTabsetPanel(session, "tabs", selected = new_tab_id)
    
    showNotification("New Representativeness Report generated in a new tab!", type = "message")
  })
  
}

shinyApp(ui, server)
