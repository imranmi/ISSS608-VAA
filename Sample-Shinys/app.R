# load R packages
pacman::p_load(shiny, shinydashboard, shinycssloaders, 
               tidyverse, dplyr, leaflet, plotly, 
               ggthemes, fresh, sf, sfdep, tmap, tm, 
              DT,lubridate, ggplot2, readr, purrr)



Events_2 <- read_csv("data/df_complete.csv")
Space_2 <- read_csv("data/df1_complete.csv")


mmr_shp_mimu_1 <- sf::st_read(dsn = "data/geospatial3", layer = "mmr_polbnda2_adm1_250k_mimu_1")
mmr_shp_mimu_2 <- sf::st_read(dsn = "data/geospatial3", layer = "mmr_polbnda_adm2_250k_mimu")




###############################   
########### MAIN BODY - START
###############################

#==========================================================  
# main header ---
header <- dashboardHeader(title = "Decoding Chaos")


# main sidebar ---
sidebar <- dashboardSidebar(
  tags$style(HTML("
      .main-sidebar{
        width: 250px;
      }
      .main-header > .navbar {
        margin-left: 250px;      
      }
      
      
      .box.box-solid.box-info>.box-header {
      color:#000000;
      background:#F8F8F8
                    }
      .box.box-solid.box-info{
      border-bottom-color:#9A3E41;
      border-left-color:#F8F8F8;
      border-right-color:#F8F8F8;
      border-top-color:#9A3E41;
      }
      .box.box-info>.box-header {
      color:black; 
      background:#F8F8F8
      }
      .box.box-info{
      border-bottom-color:#9A3E41;
      border-left-color:#F8F8F8;
      border-right-color:#F8F8F8;
      border-top-color:#9A3E41;
      background: #F8F8F8;
      }
                  ")),
  
  
  sidebarMenu(
    width = 100,
    menuItem("Aspatial Analysis", tabName = "Exploratory", icon = icon("globe")),
    menuItem("Clustering & Outlier Analysis", tabName = "Cluster", icon = icon("circle-nodes")),
    menuItem("Emerging Hot Spot Analysis", tabName = "EHSA", icon = icon("magnifying-glass-chart")),
    menuItem("Confirmatory Analysis", tabName = "ConfirmatoryAnalysis", icon = icon("clipboard-check")),
    menuItem("Visit ACLED data", icon = icon("send",lib='glyphicon'), 
             href = "https://acleddata.com/data-export-tool/")))


# customise theme ---
mytheme <- create_theme(
  adminlte_color(
    red = "#9A3E41"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#F8F8F8",
    dark_hover_bg = "#9A3E41",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF", 
    info_box_bg = "#FFF"
  )
)


#==========================================================  
# Cluster and Outlier - START
#==========================================================  

#==========================================================  
##Cluster and Outlier Analysis, 2nd Tab
#==========================================================  



Cluster2 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Dataset"),
             selectInput("QtrMoransI", "Year-Quarter",
                         choices = c("2021-Q1" = "2021Q1",
                                     "2021-Q2" = "2021Q2",
                                     "2021-Q3" = "2021Q3",
                                     "2021-Q4" = "2021Q4",
                                     "2022-Q1" = "2022Q1",
                                     "2022-Q2" = "2022Q2",
                                     "2022-Q3" = "2022Q3",
                                     "2022-Q4" = "2022Q4",
                                     "2023-Q1" = "2023Q1",
                                     "2023-Q2" = "2023Q2",
                                     "2023-Q3" = "2023Q3",
                                     "2023-Q4" = "2023Q4"),
                         selected = "2021-Q1"),
             selectInput("eventType3", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Riots" = "Riots"),
                         selected = "Battles")
         ),
         box(title = "Options for computing Local Moran's I",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             selectInput("weightstyle1", "Spatial Weights Style",
                         choices = c("W: Row standardised" = "W",
                                     "B: Binary" = "B",
                                     "C: Globally standardised" = "C",
                                     "U: C / no of neighbours" = "U",
                                     "minmax" = "minmax",
                                     "S: Variance" = "S"),
                         selected = "W"),
             selectInput("numSims1", "Number of Simulations:",
                         choices = c(99, 199, 299, 399, 499),
                         selected = 99)
         )
  ),
  column(4,
         box(title = "Local Moran's I Statistic",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             tmapOutput("LocalMoranMap", height = "700px", width = "100%")
         )
  ),
  column(4,
         box(title = "Local Indicator of Spatial Association (p-values <0.05)",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             tmapOutput("Lisa", height = "700px", width = "100%")
         )
  ),
  column(2,
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("MoransItext")
         )
  )
)

# Add this outside the first fluidRow to make it full width and below everything else
Cluster2 <- tagList(Cluster2, 
                    fluidRow(
                      column(12,
                             box(title = "Local Moran's I Results - Data Table",
                                 status = "danger",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = NULL,
                                 align = "center",
                                 dataTableOutput("localMoranDataTable"),
                                 style = "height:600px; overflow-y: scroll; overflow-x: scroll;")
                      )
                    )
)

#==========================================================  
##Cluster and Outlier Analysis, 3rd Tab
#==========================================================  

HotCold1 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Dataset"),
             selectInput("QtrGI" , "Year-Quarter",
                         choices = c("2021-Q1" = "2021Q1",
                                     "2021-Q2" = "2021Q2",
                                     "2021-Q3" = "2021Q3",
                                     "2021-Q4" = "2021Q4",
                                     "2022-Q1" = "2022Q1",
                                     "2022-Q2" = "2022Q2",
                                     "2022-Q3" = "2022Q3",
                                     "2022-Q4" = "2022Q4",
                                     "2023-Q1" = "2023Q1",
                                     "2023-Q2" = "2023Q2",
                                     "2023-Q3" = "2023Q3",
                                     "2023-Q4" = "2023Q4"),
                         selected = "2021-Q1"),
             selectInput("eventType6", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Riots" = "Riots"),
                         selected = "Battles"),
             selectInput(inputId = "numSims2",
                         label = "Number of Simulations for Gi*:",
                         choices = c(99,199,299,399,499),
                         selected = 99)
         )
         
  ),
  column(4,
         box(title = "GI* Statistics"
             ,status = "danger"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,width = NULL
             ,align = "left"
             ,tmapOutput("Gistarmap", height = "700px", width = "100%")
         )
  ),
  column(4,
         box(
           title = "Significant Hot & Cold spot areas (p-values < 0.05)",
           status = "danger",
           solidHeader = TRUE,
           collapsible = TRUE,
           width = NULL,
           align = "left",
           tmapOutput("HotColdmap", height = "700px", width = "100%")
         )
  ),
  column(2,
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("HotColdText")
         )
  )  
  
  
)  

HotCold1 <- tagList(HotCold1,
                    fluidRow(
                      column(12,
                             box(
                               title = "GI* Statistics - Data Table",
                               status = "danger",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               width = NULL,
                               align = "center",
                               dataTableOutput("AdaptiveGiStat"),
                               style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                      )
                    )
                    
)

#==========================================================  
#Cluster and Outlier Analysis tab ----END
#==========================================================  

#==========================================================  
##Emerging Hot Spot Analysis tab --- START
#==========================================================  

EHSA1 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Dataset"),
             selectInput("eventType7", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Riots" = "Riots" ),
                         selected = "Battles"),
             selectizeInput(inputId = "Admin2",
                            label = "Select District",
                            choices = unique(Space_2$DT),
                            multiple = FALSE)
             
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("GITrendText")
         )
  ),
  column(10,
         box(title = "GI* Trends per district"
             ,status = "danger"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,width = NULL
             ,align = "left"
             ,plotlyOutput("Giplot", height = "600px")
         )
  ),       
  
  column(12,
         box(title = "Mann Kendall Test results"
             ,status = "danger"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,width = NULL
             ,align = "left"
             ,dataTableOutput("MKtest"),
             style = "height:600px; overflow-y: scroll;overflow-x: scroll;")
         
  )
)

EHSA2 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for ESHA map"),
             selectInput("eventType8", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Riots" = "Riots" ),
                         selected = "Battles"),
             selectInput(inputId = "numLags", 
                         label = "Number of Lags:", 
                         choices = c(1, 2, 3, 4, 5),
                         selected = 1),
             selectInput(inputId = "numSims", 
                         label = "Number of Simulations:", 
                         choices = c(99, 199, 299, 399, 499),
                         selected = 99)
             
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("EHSAText")
             
         )
  ),
  
  column(10,
         box(title = "Emerging Hot Spot Analysis map"
             ,status = "danger"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,width = NULL
             ,align = "left"
             ,tmapOutput("EHSAmap", height = "600px")
         )
  ),       
  
  column(12,
         box(title = "Distribution of EHSA classes"
             ,status = "danger"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,width = NULL
             ,align = "left"
             ,plotlyOutput("EHSAbar", height = "600px")
         )
         
  )
  
)

#==========================================================  
##Emerging Hot Spot Analysis tab ----END
#==========================================================  

#define the no of sub tabs needed

ClusterSubTabs <- tabsetPanel(
  #tabPanel("Distribution of Conflict Events", 
  #        Cluster1),
  tabPanel("Local Measures of Spatial Autocorrelation", 
           Cluster2),
  tabPanel("Hot & Cold Spot Analysis(HCSA)", 
           HotCold1)
)

ESHASubTabs <- tabsetPanel(
  tabPanel("Gi* trend and Mann Kendall test", 
           EHSA1),
  tabPanel("Emerging Hot Spot Map", 
           EHSA2)
  
)




body <- dashboardBody(
  
  # use theme
  use_theme(mytheme),
  
  
  # =============================
  # tabItems - All Pages
  # =============================
  
  tabItems(
    # 1st tab content
    tabItem(tabName = "Exploratory",
            #no content
    ),
    # 2nd tab content
    tabItem(tabName = "Cluster",
            
            ClusterSubTabs # add the sub tabs which was defined above
    ),
    #3rd tab content
    tabItem(tabName = "EHSA",
            
            ESHASubTabs
    ),
    #4th tab content
    tabItem(tabName = "ConfirmatoryAnalysis"
            #content)
    )
  )  
)

###############################   
########### MAIN BODY - END
###############################


# =============================    
########### UI - START
# =============================

# UI dashboard ---
ui <- dashboardPage(title = 'Armed Conflicts in Myanmar (2010 to 2023)', 
                    header, sidebar, body, skin='red')      

# =============================    
########### UI - END
# =============================


# create the server functions for the dashboard  
server <- function(input, output, session) { 
  
  # =============================    
  #DATA Wrangling
  # =============================  
  

  #Data subset for Local Moran's & Gi* statistics
  #====================================================
  
  Events_admin2 <- left_join(mmr_shp_mimu_2, Events_2,
                             by = c("DT" = "admin2"))
  
  Events_admin2 <- Events_admin2 %>%
    select(-OBJECTID, -ST, -ST_PCODE, 
           -DT_PCODE, -DT_MMR, -PCode_V) %>%
    rename("District" = "DT")
  
  
  
  
  #==========================================================
  # START of Cluster & Outlier Analysis Module
  #==========================================================   
  
  
  #creating the Plots- for Cluster & Outlier Analysis 
  
  
  
  #==========================================================
  # Local Morans's I Statistics and LISA map in Cluster 2
  #==========================================================   
  
  localMIResults <- reactive({
    # Filter the data based on the user's selection
    filteredData <- Events_admin2 %>%
      filter(quarter == input$QtrMoransI, event_type == input$eventType3) 
    
    
    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data
    
    # Computing Contiguity Spatial Weights
    wm_q <- filteredData %>%
      mutate(nb = st_contiguity(geometry),
             wt = st_weights(nb,
                             style = input$weightstyle1))
    
    
    
    # Computing Local Moran's I
    lisa <- wm_q %>%
      mutate(local_moran = local_moran(
        Incidents, nb, wt, nsim = as.numeric(input$numSims1)),
        .before = 5) %>%
      unnest(local_moran)
    
    return(lisa)       
    
    
  })
  
  
  # Render the map of Local Moran's I values
  output$LocalMoranMap <- renderTmap({
    df <- localMIResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    # Map creation using tmap
    localMI_map <- tm_shape(df) +
      tm_fill(col = "ii", style = "pretty", palette = "RdBu", title = "Local Moran's I") +
      tm_borders() 
    
    localMI_map + 
      tm_view(set.zoom.limits = c(5,7))
  })
  
  
  
  #==========================================================
  # LISA Map in Cluster 2 
  #==========================================================  
  
  output$Lisa <- renderTmap({
    df <- localMIResults()
    if(is.null(df)) return()
    
    lisa_sig <- df  %>%
      filter(p_ii < 0.05)
    
    
    lisamap <- tm_shape(df) +
      tm_polygons() +
      tm_borders() +
      
      tm_shape(lisa_sig) +
      tm_fill(col = "mean",  
              palette = "-RdBu",  
              title = "Significance") +
      tm_borders(alpha = 0.4)
    
    
    lisamap + 
      tm_view(set.zoom.limits = c(5,7))
    
    
  })
  
  
  #==========================================================
  # Local Morans's I Data Table in Cluster 2 
  #==========================================================  
  
  # Render the data table for Local Moran's I results
  output$localMoranDataTable <- renderDataTable({
    df <- localMIResults()
    
    # Check if data is available
    if (!is.null(df)) {
      
      df
    }
  })
  
  output$MoransItext <- renderText({ 
    "Local Moran's I, is a spatial statistic that detects and quantifies 
    spatial clustering or dispersion within a given geographic area.
    It assesses spatial patterns at a local level, determining if features form 
    significant clusters (high-high or low-low) or outliers (high-low or low-high) 
    compared to neighboring features.
    
    In our analysis, we're examining if certain areas (admin region 2) exhibit higher or 
    lower incident rates of a specific event type than expected by chance alone, 
    indicating deviations from a random spatial distribution." 
  })
  

  
  #==========================================================
  # Hot & Cold Spot Analysis - GI* statistics
  #==========================================================
  
  
  GiData <- reactive({
    filtered_data2 <- Events_admin2 %>%
      filter(quarter == input$QtrGI, event_type == input$eventType6)
    
    
    #Derive a spatial weight matrix by using sfdep functions and tidyverse approach.
    wm_idw <- filtered_data2 %>%
      mutate(nb = st_contiguity(geometry),
             wts = st_inverse_distance(nb, geometry,
                                       scale = 1,
                                       alpha = 1))
    
    #compute the local Gi* by using the code chunk below
    
    HCSA <- wm_idw %>% 
      mutate(local_Gi = local_gstar_perm(
        Incidents, nb, wt, nsim = as.numeric(input$numSims2)),
        .before = 5) %>%
      unnest(local_Gi)
    
    return(HCSA)
    
  })
  
  output$Gistarmap <- renderTmap({
    df <- GiData()
    
    # Exit if there's no data to plot
    if(is.null(df) || nrow(df) == 0) return() #Exit if no data
    
    
    # Create the choropleth map for GI stats
    Gi_map <- tm_shape(df) +
      tm_fill(col = "gi_star", 
              palette = "-RdBu", 
              title = "Local Gi") +
      tm_borders()
    
    Gi_map + 
      tm_view(set.zoom.limits = c(5,7))
  })
  
  
  output$HotColdmap <-  renderTmap({
    df <- GiData()
    
    if(is.null(df) || nrow(df) == 0) return() #Exit if no data
    
    
    HCSA_sig <- df  %>%
      filter(p_value < 0.05)
    
    # Create the choropleth map for HSCA Map
    HSCAmap <- tm_shape(df) +
      tm_polygons() +
      tm_borders() +
      
      tm_shape(HCSA_sig) +
      tm_fill(col = "gi_star",  
              palette = "-RdBu",  
              title = "gi_star") +
      tm_borders(alpha = 0.4)
    
    HSCAmap + 
      tm_view(set.zoom.limits = c(5,7))
  })
  
  
  
  
  
  output$AdaptiveGiStat <- renderDataTable({
    data_with_gi <- GiData()  # Reactive function for data preparation
    if(is.null(data_with_gi)) {
      return(data.frame())  # Return an empty data frame if data is null
    }
    return(data_with_gi)
  })
  
  output$HotColdText <- renderText({ 
    "HCSA uses spatial weights to identify locations of statistically significant 
    hot spots and cold spots in an spatially weighted attribute that are in proximity 
    to one another based on a calculated distance. 
    The analysis groups features when similar high (hot) or low (cold) values are found in a cluster.
    
    Here we are utilizing the Getis and Ord’s G statistics. 
    High positive G values indicate hot spots—areas where high values cluster together
    while low negative G values indicate cold spots—areas where low values cluster together." 
  })
  
  
  #==========================================================
  # END of Cluster & Outlier Analysis Module
  #==========================================================
  
  
  #==========================================================
  # START of Emerging Hot spot Analysis Module
  #==========================================================
  
  #==========================================================
  # GI* Trends per district & Mann Kendall test
  #==========================================================
  
  
  
  EHSAData <- reactive({
    space_data <- Space_2 %>%
      filter(event_type == input$eventType7)
    
    
    Filtered_space <- space_data %>%
      select(-event_type, -year, -Fatalities)
    
    Quarterly_spt <- spacetime(Filtered_space, mmr_shp_mimu_2,
                               .loc_col = "DT",
                               .time_col = "quarter")
    
    Quarterly_nb <- Quarterly_spt %>%
      activate("geometry") %>%
      mutate(nb = include_self(st_contiguity(geometry)),
             wt = st_inverse_distance(nb, geometry,
                                      scale = 1,
                                      alpha = 1),
             .before = 1) %>%
      set_nbs("nb") %>%
      set_wts("wt")
    
    gi_stars <- Quarterly_nb %>% 
      group_by(quarter) %>% 
      mutate(gi_star = local_gstar_perm(
        Incidents, nb, wt)) %>% 
      tidyr::unnest(gi_star)
    
    return(gi_stars)
    
  }) 
  
  
  output$Giplot <- renderPlotly({
    
    df <- EHSAData()
    
    # Exit if there's no data to plot
    if(is.null(df) || nrow(df) == 0) return() #Exit if no data
    
    
    filtered_df <- df %>%
      filter(DT == input$Admin2) %>%
      select(DT, quarter, gi_star)
    
    p1 <- ggplot(data = filtered_df, 
                 aes(x = quarter, 
                     y = gi_star)) +
      geom_line() +
      theme_light() +
      ggtitle(paste("GI* Trends for District:", input$Admin2))
    
    ggplotly(p1)
    
    
  })
  
  
  EHSADataMKTest <- reactive({
    df <- EHSAData() 
    
    ehsa3 <- df %>%
      group_by(DT) %>%
      summarise(mk = list(
        unclass(
          Kendall::MannKendall(gi_star)))) %>%
      tidyr::unnest_wider(mk)
    
    return(ehsa3)
  })
  
  
  output$MKtest <- renderDataTable({
    # Get the Mann-Kendall test results
    mkResults <- EHSADataMKTest()
    
    # Return the results to render them as a table
    mkResults
  })
  
  
  output$GITrendText <- renderText({ 
    "The GI* trend plot shows the changes in the Local Gi* statistics 
      per district, for each event type.
      The Mann-Kendall test is a non-parametric statistical test used to identify trends 
      in a series of data. Its primary purpose is to determine whether there is a 
      monotonic trend over time in the observed data. 
      To view significant emerging hot/cold spots, users can sort 
      the tau & sl variables in descending order " 
  })
  
  
  #==========================================================
  # Distribution of EHSA classes and EHSA Map
  #==========================================================
  
  EHSAData1 <- reactive({
    space_data <- Space_2 %>%
      filter(event_type == input$eventType8)
    
    
    Filtered_space <- space_data %>%
      select(-event_type, -year, -Fatalities)
    
    Quarterly_spt <- spacetime(Filtered_space, mmr_shp_mimu_2,
                               .loc_col = "DT",
                               .time_col = "quarter")
    
    ehsa3 <- emerging_hotspot_analysis(
      x = Quarterly_spt, 
      .var = "Incidents", 
      k = as.numeric(input$numLags), 
      nsim = as.numeric(input$numSims)
      
    )
    
    return(ehsa3)
    
  }) 
  
  output$EHSAbar <- renderPlotly({
    df <- EHSAData1() 
    
    EHSAbar1 <- ggplot(data = df, aes(x = classification)) +
      geom_bar() +
      theme_minimal()
    
    ggplotly(EHSAbar1) 
    
    
  })
  
  EHSAMapdata <- reactive({
    df <- EHSAData1() 
    
    mmr3_ehsa <- mmr_shp_mimu_2 %>%
      left_join(df,
                by = join_by(DT == location))
    
    mmr3_ehsa <- mmr3_ehsa %>%
      select(-OBJECTID, -ST, -ST_PCODE)
    
    return(mmr3_ehsa)
  })
  
  output$EHSAmap <- renderTmap({
    df <- EHSAMapdata()
    if(is.null(df)) return()
    
    ehsa_sig3 <- df  %>%
      filter(p_value < 0.05)
    
    
    ehsamap <- tm_shape(df) +
      tm_polygons() +
      tm_borders() +
      
      tm_shape(ehsa_sig3) +
      tm_fill("classification") +
      tm_borders(alpha = 0.4)
    
    
    ehsamap + 
      tm_view(set.zoom.limits = c(5,7))
    
    
  })
  
  
  
  
  
  output$EHSAText <- renderText({ 
    "Emerging Hot Spot Analysis identifies trends in spatial clustering 
      over a period of time. Emerging hot spot analysis combines the Getis-Ord Gi* statistic 
      with the Mann-Kendall 
      trend test to determine if there is a temporal trend associated with local clustering of hot and cold spots."
  })
  
  #==========================================================
  # END of Emerging Hot spot Analysis Module
  #==========================================================
  
  
  
  #==========================================================
  # START of Confirmatory Analysis Module
  #==========================================================
  
  # plot codes here
  
  
  #==========================================================
  # END of Confirmatory Analysis Module
  #==========================================================
  
  
}
# Run the app
shinyApp(ui = ui, server = server)