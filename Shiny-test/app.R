pacman::p_load(shiny, tidyverse, shinydashboard,dplyr,
               spatstat, spdep,
               lubridate, leaflet,
               plotly, DT, viridis,
               ggplot2, sf, tmap, readr, purrr)


ACLED_MMR <- read_csv("data/MMR.csv")

mmr_shp_mimu_1 <- sf::st_read(dsn = "data/geospatial3", layer = "mmr_polbnda2_adm1_250k_mimu_1")
mmr_shp_mimu_2 <- sf::st_read(dsn = "data/geospatial3", layer = "mmr_polbnda_adm2_250k_mimu")



#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Decoding Chaos: Armed Conflicts in Myanmar", titleWidth = 450)  


#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Exploratory", tabName = "Exploratory", icon = icon("globe")),
    menuItem("Spatial Point Pattern Analysis", tabName = "SpacialPoint", icon = icon("arrow-trend-up")),
    menuItem("Cluster and Outlier Analysis", tabName = "Cluster", icon = icon("magnifying-glass-chart")),
    menuItem("Hot & Cold Spot Analysis", tabName = "HotCold", icon = icon("fire",lib = "font-awesome")),
    menuItem("Confirmatory Analysis", tabName = "ConfirmatoryAnalysis", icon = icon("microscope")),
    menuItem("Visit the ACLED website", icon = icon("send",lib='glyphicon'), 
             href = "https://acleddata.com/")))


#Cluster and Outlier Analysis, 1st Tab
#####################################
Cluster1 <- fluidRow(
  box(title = "Location of Conflict Events",
      status = "danger",
      solidHeader = TRUE,
      collapsible = TRUE,
      align = "center",
      leafletOutput("Pointmap", height = "750px"),
      sliderInput("slider1", "Year:", 2010, 2024, 2021),
      selectInput("eventType1", "Event Type:",
                  choices = c("Battles" = "Battles",
                              "Violence against civilians" = "Violence against civilians",
                              "Protests" = "Protests",
                              "Riots" = "Riots",
                              "Explosions/Remote violence" = "Explosions/Remote violence",
                              "Strategic developments" = "Strategic developments"),
                  selected = "Battles")) # Default selection
  ,box(
    title = "Distribution of Conflict Events",
    status = "danger",
    solidHeader = TRUE,
    collapsible = TRUE,
    align = "center",
    plotOutput("choropleth", height = "600px"),
    sliderInput("slider2", "Year:", 2010, 2024, 2021),
    selectInput("eventType2", "Event Type:",
                choices = c("Battles" = "Battles",
                            "Violence against civilians" = "Violence against civilians",
                            "Protests" = "Protests",
                            "Riots" = "Riots",
                            "Explosions/Remote violence" = "Explosions/Remote violence",
                            "Strategic developments" = "Strategic developments"),
                selected = "Battles"),
    radioButtons("metricType", "Count Type:",
                 choices = c("Incidents" = "Incidents",
                             "Fatalities" = "Fatalities"),
                 selected = "Fatalities",
                 inline = TRUE),
    radioButtons("mapStyle", "Classification Type:",
                 choices = c("quantile", "equal", "jenks", "kmeans"),
                 selected = "quantile",
                 inline = TRUE))
)

#Cluster and Outlier Analysis, 2nd Tab
#####################################

Cluster2 <- fluidRow(
  box(
    title = "Local Moran's I Statistic",
    status = "danger",
    solidHeader = TRUE,
    collapsible = TRUE,
    align = "center",
    plotOutput("LocalMoranMap", height = "500px", width = "100%"),
    sliderInput("slider3", "Years:", 2010, 2024, 2021),
    selectInput("eventType3", "Event Type:",
                choices = c("Battles" = "Battles",
                            "Violence against civilians" = "Violence against civilians",
                            "Protests" = "Protests",
                            "Riots" = "Riots",
                            "Explosions/Remote violence" = "Explosions/Remote violence",
                            "Strategic developments" = "Strategic developments"),
                selected = "Battles"))
  ,box(
    title = "Local Moran's I P-values",
    status = "danger",
    solidHeader = TRUE,
    collapsible = TRUE,
    align = "center",
    plotOutput("LocalMoranPval", height = "650px", width = "100%"))
  ,box(
    title = "Local Moran's I Results",
    status = "danger",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = 12,
    align = "center",
    dataTableOutput("localMoranDataTable")
  )
)


#Cluster and Outlier Analysis, 3rd Tab
#####################################

Cluster3 <- fluidRow(
  box(title = "Moran Scatter Plot"
      ,status = "danger"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,align = "center"
      ,plotOutput("MoranScatter", height = "600px")  
      ,sliderInput("slider5", "Years:", 2010, 2024, 2021),
      selectInput("eventType4", "Event Type:",
                  choices = c("Battles" = "Battles",
                              "Violence against civilians" = "Violence against civilians",
                              "Protests" = "Protests",
                              "Riots" = "Riots",
                              "Explosions/Remote violence" = "Explosions/Remote violence",
                              "Strategic developments" = "Strategic developments"),
                  selected = "Battles"))
  ,box(
    title = "Local Indicator of Spacial Association (LISA)"
    ,status = "danger"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,align = "center"
    ,plotOutput("Lisa", height = "600px")
    ,sliderInput("slider6", "Years:", 2010, 2024, 2021),
    selectInput("eventType5", "Event Type:",
                choices = c("Battles" = "Battles",
                            "Violence against civilians" = "Violence against civilians",
                            "Protests" = "Protests",
                            "Riots" = "Riots",
                            "Explosions/Remote violence" = "Explosions/Remote violence",
                            "Strategic developments" = "Strategic developments"),
                selected = "Battles"))
)

#Hot and Cold Spot Analysis
#####################################

HotCold1 <- fluidRow(
  box(title = "Hot and Cold Spots"
      ,status = "danger"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,align = "center"
      ,plotOutput("Gimap", height = "600px")  
      ,sliderInput("slider7", "Years:", 2010, 2024, 2021),
      selectInput("eventType6", "Event Type:",
                  choices = c("Battles" = "Battles",
                              "Violence against civilians" = "Violence against civilians",
                              "Protests" = "Protests",
                              "Riots" = "Riots",
                              "Explosions/Remote violence" = "Explosions/Remote violence",
                              "Strategic developments" = "Strategic developments"),
                  selected = "Battles"))
  ,box(title = "GI Statistics",
       status = "danger",
       solidHeader = TRUE,
       collapsible = TRUE,
       width = 12,
       align = "center",
       dataTableOutput("GiStat")
  )
)



#define the no of sub tabs needed

ClusterSubTabs <- tabsetPanel(
  tabPanel("Proportional Symbol Map", 
           Cluster1),
  tabPanel("Local Moran Statistics", 
           Cluster2),
  tabPanel("LISA Map", 
           Cluster3)
)




body <- dashboardBody(
  tabItems(
    # 1st tab content
    tabItem(tabName = "Exploratory",
            #content
    ),
    # 2nd tan content
    tabItem(tabName = "SpacialPoint",
            #content
    ),
    # 3rd tab content
    tabItem(tabName = "Cluster",
            
            ClusterSubTabs # add the sub tabs which was defined above
    ),
    #4th tab content
    tabItem(tabName = "HotCold",
            HotCold1
            # content
    ),
    #5th tab content
    tabItem(tabName = "ConfirmatoryAnalysis"
            #content)
    )
  )  
)

#completing the ui part with dashboard Page
ui <- dashboardPage(title = 'Armed Conflict Dashboard', header, sidebar, body, skin='red')    



# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #Data subset for Choropleth Maps (Admin 2)
  ######################################################
  Data2 <- ACLED_MMR %>%
    group_by(year, admin2, event_type, sub_event_type) %>%
    summarise(Incidents = n(),
              Fatalities = sum(fatalities, na.rm = TRUE)) %>%
    
    ungroup()
  
  #Spacial join between shape file and attribute file (admin 2)
  ######################################################
  ACLED_MMR_admin2 <- left_join(mmr_shp_mimu_2, Data2,
                                by = c("DT" = "admin2"))
  
  # Convert conflict data to an sf object
  ###################################################
  conflict_sf <- sf::st_as_sf(ACLED_MMR, coords = c("longitude", "latitude"), crs = 4326)
  
  #Data subset for Local Morans statistics
  ######################################################
  
  Events2 <- ACLED_MMR %>%
    group_by(year, admin2, event_type) %>%
    summarise(Incidents = n(),
              Fatalities = sum(fatalities, na.rm = TRUE)) %>%
    
    ungroup()
  
  Events_admin2 <- left_join(mmr_shp_mimu_2, Events2,
                             by = c("DT" = "admin2"))
  
  
  #Calculating Spacial weights for Local Morans statistics
  ######################################################
  #KIV
  
  
  
  
  #creating the Plots 
  
  # Pointmap in Cluster 1 UI
  #################################################################  
  
  # Reactive expression to filter data based on input from UI
  PointMapYears <- reactive({
    filtered_data <- conflict_sf %>% 
      filter(event_type == input$eventType1, year %in% input$slider1)
    return(filtered_data)  # Ensure the filtered data is returned
  })
  
  # Create the map based on filtered data
  output$Pointmap <- renderLeaflet({
    data_years <- PointMapYears() # Use the reactive expression to get filtered data
    
    if(nrow(data_years) == 0){  # Check if data is empty
      return(NULL)  # Return NULL to avoid trying to plot an empty map
    }
    
    scaleFactor <- 2 # Define your scale factor
    
    # Now use data_years instead of Battles
    leaflet(data_years) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = mmr_shp_mimu_1, color = "#444444", weight = 1, fillOpacity = 0.5) %>%
      addCircleMarkers(popup = ~paste("Event:", event_type, "<br>State/Region:", admin1, 
                                      "<br>Actor1:", actor1, "<br>Actor2:", actor2,
                                      "<br>Year:", year, "<br>Fatalities:", fatalities),
                       radius = ~sqrt(fatalities) * scaleFactor,
                       fillColor = "red", fillOpacity = 0.4, color = "#FFFFFF", weight = 1) %>%
      setView(lng = 96.1603, lat = 19.745, zoom = 6)
  })
  
  
  # choropleth in Cluster 1 UI
  #################################################################  
  
  output$choropleth <- renderPlot({
    # Dynamically filter data based on user input
    data_filtered <- ACLED_MMR_admin2 %>%
      filter(year == input$slider2, event_type == input$eventType2)
    
    # Check if the filtered data is empty to avoid errors
    if(nrow(data_filtered) == 0) {
      return(NULL)  # Return NULL if there's no data to avoid plotting errors
    }
    
    # Determine the column to use for filling based on user selection
    fillColumn <- ifelse(input$metricType == "Fatalities", "Fatalities", "Incidents")
    
    # Generate the choropleth map with dynamic data and user-selected metric
    tm_map <- tm_shape(data_filtered) +
      tm_fill(fillColumn,
              n = 5,
              style = input$mapStyle,
              palette = "Reds") +
      tm_borders(alpha = 0.5)
    
    print(tm_map)  
  })
  
  # Local Morans's I Map in Cluster 2  
  ###############################################
  
  localMIResults <- reactive({
    # Filter the data based on the user's selection
    filteredData <- Events_admin2 %>%
      filter(year == input$slider3, event_type == input$eventType3)
    
    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data
    
    # Computing Contiguity Spatial Weights
    wm_q <- poly2nb(filteredData, queen = TRUE)
    rswm_q <- nb2listw(wm_q, style = "W", zero.policy = TRUE)
    
    # Computing Local Moran's I
    localMI <- localmoran(filteredData$Incidents, rswm_q, na.action = na.exclude)
    
    # Combine Local Moran's I results with the filtered data
    df_localMI <- cbind(filteredData, localMI)
    
    # Set the geometry column explicitly if needed
    st_geometry(df_localMI) <- st_geometry(filteredData)
    
    df_localMI
  })
  
  # Render the map of Local Moran's I values
  output$LocalMoranMap <- renderPlot({
    df <- localMIResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    # Map creation using tmap
    localMI_map <- tm_shape(df) +
      tm_fill(col = "Ii", style = "pretty", palette = "RdBu", title = "Local Moran's I") +
      tm_borders(alpha = 0.5)
    
    localMI_map
  })
  
  # Local Morans's I P-values Map in Cluster 2  
  ###############################################
  
  output$LocalMoranPval <- renderPlot({
    df <- localMIResults()
    
    # Exit if there's no data to plot
    if(is.null(df) || nrow(df) == 0) {
      return()
    }
    
    # Create the choropleth map for p-values
    pvalue_map <- tm_shape(df) +
      tm_fill(col = "Pr.z....E.Ii..", 
              breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
              palette = "-Blues", 
              title = "Local Moran's I p-values") +
      tm_borders(alpha = 0.5)
    
    pvalue_map
  })
  
  # Local Morans's I Data Table in Cluster 2  
  ###############################################
  
  # Render the data table for Local Moran's I results
  output$localMoranDataTable <- renderDataTable({
    df <- localMIResults()
    
    # Check if data is available
    if (!is.null(df)) {
      
      df
    }
  })
  
  
  # Moran Scatter plot in Cluster 3
  ####################################
  
  output$MoranScatter <- renderPlot({
    # Retrieve filtered data based on input selections for event type and year
    filteredData1 <- Events_admin2 %>%
      filter(year == input$slider5, event_type == input$eventType4)
    
    # Exit if no data is available for the selected criteria
    if(nrow(filteredData1) == 0) {
      return(NULL)
    }
    
    # Standardize the Incidents variable
    standardizedIncidents <- scale(filteredData1$Incidents) %>% 
      as.vector 
    
    # Computing Contiguity Spatial Weights
    wm_q <- poly2nb(filteredData1, queen = TRUE)
    rswm_q <- nb2listw(wm_q, style = "W", zero.policy = TRUE)
    
    # Compute Moran's I values
    moranValues <- localmoran(standardizedIncidents, rswm_q, na.action = na.exclude)
    
    plotTitle <- paste("Moran Scatterplot for", input$eventType4, "in", input$slider5)
    
    # Create the Moran scatterplot
    nci <- moran.plot(standardizedIncidents, rswm_q,
                      labels = as.character(filteredData1$DT),
                      xlab = "Standardized Incidents",
                      ylab = "Spatially Lagged Incidents",
                      main = plotTitle)
    
    
  })
  
  # LISA Map in Cluster 3
  ####################################
  
  
  lisaCalculation <- reactive({
    LisaData <- Events_admin2 %>%
      filter(year == input$slider6, event_type == input$eventType5)
    
    if(nrow(LisaData) == 0) return(NULL)
    
    # Computing spatial weights and local Moran's I (LISA)
    
    wm_q <- poly2nb(LisaData, queen = TRUE)
    rswm_q <- nb2listw(wm_q, style = "W", zero.policy = TRUE)
    
    
    lag_Incidents <- lag.listw(rswm_q, LisaData$Incidents)
    DV <- lag_Incidents - mean(lag_Incidents)
    localMI <- localmoran(LisaData$Incidents, rswm_q)
    
    # Classifying into quadrants
    quadrant <- rep(NA, nrow(localMI))
    signif <- 0.05
    quadrant[DV < 0 & localMI[, 1] > 0] <- 1
    quadrant[DV > 0 & localMI[, 1] < 0] <- 2
    quadrant[DV < 0 & localMI[, 1] < 0] <- 3  
    quadrant[DV > 0 & localMI[, 1] > 0] <- 4    
    quadrant[localMI[, 5] > signif] <- 0
    
    # Append the results to the data frame
    LisaData$quadrant <- factor(quadrant, levels = 0:4, 
                                labels = c("insignificant", "low-low", 
                                           "low-high", "high-low", 
                                           "high-high"))
    
    # Return the data frame with LISA results
    LisaData
  })
  
  # Render the LISA map plot
  output$Lisa <- renderPlot({
    df <- lisaCalculation()
    if(is.null(df)) return()
    
    colors <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
    
    # Generate the LISA map
    tm_shape(df) +
      tm_fill(col = "quadrant", 
              style = "cat", 
              palette = colors, 
              title = "LISA Clusters") +
      tm_borders(alpha=0.5)
  })
  
  
  # Hot cold Map in HotCold1 
  ####################################  
  
  HotColdData <- reactive({
    filtered_data2 <- Events_admin2 %>%
      filter(year == input$slider7, event_type == input$eventType6)
    
    # Calculate centroids for filtered data
    longitude <- map_dbl(filtered_data2$geometry, ~st_centroid(.x)[[1]])
    latitude <- map_dbl(filtered_data2$geometry, ~st_centroid(.x)[[2]])
    coords <- cbind(longitude, latitude)
    
    # Adaptive distance weight matrix
    knn <- knn2nb(knearneigh(coords, k=8))
    knn_lw <- nb2listw(knn, style = 'B')
    
    # Calculate Gi statistics for the filtered data
    gi.adaptive <- localG(filtered_data2$Incidents, knn_lw)
    
    # Convert the "localG" object to a matrix and bind it to filtered_data2
    gi_values <- as.matrix(gi.adaptive)
    
    # Check if the number of rows match before combining
    if(nrow(filtered_data2) == nrow(gi_values)) {
      # Add the Gi values as a new column to the filtered data
      filtered_data2 <- cbind(filtered_data2, gstat_adaptive = gi_values)
    } else {
      stop("The number of rows in filtered_data2 does not match the number of Gi values.")
    }
    
    filtered_data2
  })
  
  output$Gimap <- renderPlot({
    df <- HotColdData()
    
    # Exit if there's no data to plot
    if(is.null(df) || nrow(df) == 0) {
      return()
    }
    
    # Create the choropleth map for GI stats
    Gi_map <- tm_shape(df) +
      tm_fill(col = "gstat_adaptive", 
              style = "pretty", 
              palette = "-RdBu", 
              title = "Adaptive Distance\nlocal Gi") +
      tm_borders(alpha = 0.5)
    
    Gi_map
  })
  
  
  output$GiStat <- renderDataTable({
    data_with_gi <- HotColdData()  # Reactive function for data preparation
    if(is.null(data_with_gi)) {
      return(data.frame())  # Return an empty data frame if data is null
    }
    return(data_with_gi)
  })
  
}
# Run the app
shinyApp(ui = ui, server = server)
