# load R packages
pacman::p_load(shiny, shinydashboard, shinycssloaders, 
               tidyverse, dplyr, leaflet, plotly, highcharter, 
               ggthemes, fresh, sf, spdep, tmap, tm, ggforce, 
               ggraph, igraph, wordcloud, tidytext, DT, spatstat,
               lubridate,viridis, ggplot2, readr, purrr)


ACLED_MMR <- read_csv("data/MMR.csv")

final <- readRDS("data/final.rds")
mapping_rates <- readRDS("data/mapping_rates.rds")

event_type <- unique(final$event_type)
admin1 <- unique(final$admin1)



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
    menuItem("Exploratory", tabName = "Exploratory", icon = icon("globe")),
    menuItem("Clustering & Outlier Analysis", tabName = "Cluster", icon = icon("circle-nodes")),
    menuItem("Hot/ Cold Zone Analysis", tabName = "HotCold", icon = icon("magnifying-glass-chart")),
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


# =============================    
########### EXPLORATORY - START
# =============================


#==========================================================  
# ExploreOverviewrow1 ---
#==========================================================  
ExploreOverviewrow1 <-  fluidRow(
  box(title = "Armed Conflict Incidents in Myanmar (2010 to 2023)",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      column(9,
             box(width = 12,  # Automatically adjust to full width
                 align = "center",
                 withSpinner(leafletOutput("emap_eo1", height = "650px", width = "100%"))  # Set map width to 100% of box
             )   
      ),
      column(3,
             box(title = "Desired Characteristics",
                 status = "info",
                 solidHeader = FALSE, 
                 width = NULL,
                 selectizeInput(inputId = "EventSelect_eo1",
                                label = "Select Event(s)",
                                choices = event_type,
                                multiple = TRUE,
                                options = list(maxItems = 6, placeholder = 'Enter event type',
                                               onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 hr(),
                 selectizeInput(inputId = "AdminSelect_eo1",
                                label = "Select Administrative Region(s)",
                                choices = admin1,
                                multiple = TRUE,
                                options = list(maxItems = 18, placeholder = 'Enter Admin Region',
                                               onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 hr(),
                 sliderInput(inputId = "YearSlider_eo1", 
                             label = "Years:", 
                             min = 2010, 
                             max = 2023,
                             value = c(2010, 2023)),
                 actionButton("resetButton", "Reset Selections")
             ),
             hr(),
             box(title = "About",
                 status = "danger",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 width = NULL,
                 align = "justify",
                 textOutput("abouttext") 
             )
      )
  )
  
)

#==========================================================  
# ExploreGeospatialrow1 ---
#==========================================================  
ExploreGeospatialrow1 <-  fluidRow(
  
  column(2,
         box(title = "Desired Characteristics",
             status = "info",
             solidHeader = FALSE, 
             width = NULL, 
             helpText("Filter options are applicable for statistical distribution"),
             radioButtons(inputId = "InciFata_eg1",
                          label = "Display",
                          choices = c("No. of Incidents" = "Total_Incidents", "No. of Fatalities" = "Total_Fatalities"),
                          selected = "Total_Incidents"),
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "justify",
             textOutput("statisticaltext") 
         )
         
  ),
  column(4,
         box(title = "Statistical Distribution",
             status = "danger",
             solidHeader = TRUE,
             width = NULL,  # Automatically adjust to full width
             align = "center",
             withSpinner(tmapOutput("emap_eg1", height = "700px", width = "100%"))
         )   
  ),
  column(4,
         box(title = "Spatial Distribution",
             status = "danger",
             solidHeader = TRUE,
             width = NULL,
             align = "center",
             withSpinner(tmapOutput("emap_eg2", height = "700px", width = "100%"))
         )   
  ),
  column(2,
         box(title = "Desired Characteristics",
             status = "info",
             solidHeader = FALSE, 
             width = NULL, 
             helpText("Filter options are applicable for spatial distribution"),
             selectInput(inputId = "YearSelect_eg2", 
                         label = "Year:", 
                         choices = unique(final$year), 
                         # selected = 2023,
             ),
             hr(),
             radioButtons(inputId = "InciFata_eg2",
                          label = "Display",
                          choices = c("Incident Rate" = "Total_Incidents", "Fatality Rate" = "Total_Fatalities"),
                          selected = "Incident Rate"),
             hr(),
             selectInput(inputId = "ClassificationSelect_eg2", 
                         label = "Classification Type:", 
                         choices = c("equal", "pretty", "quantile", "kmeans"),
                         selected = "kmeans"
             ),
             hr(),
             selectInput(inputId = "ClassSelect_eg2", 
                         label = "Number of Classes:", 
                         choices = c(2, 4, 6, 8, 10),
                         selected = 10
             )),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "justify",
             textOutput("spatialtext") 
         )
  )
  
)


#==========================================================  
# ExploreTrendrow1 ---
#==========================================================  
ExploreTrendrow1 <-  fluidRow(
  box(
    title = "Rising Armed Conflict Incidents in Myanmar (2010 to 2023)",
    status = "danger",
    solidHeader = TRUE, 
    collapsible = TRUE,
    width = 12,  # Use full width
    column(8,
           box(
             width = NULL,
             withSpinner(highchartOutput("line_et1", height = "400px"))  # Width is automatically 100% of the box
           )
    ),
    column(
      4,
      
      box(title = "Desired Characteristics",
          status = "info",
          solidHeader = FALSE, 
          width = NULL, 
          helpText("Filter options are applicable to line chart"),
          selectizeInput(inputId = "EventSelect_et1",
                         label = "Select Event(s)",
                         choices = unique(final$event_type),
                         multiple = TRUE,
                         options = list(maxItems = 6, placeholder = 'Enter event type',
                                        onInitialize = I('function() { this.setValue(""); }'))
          ),
          hr(),
          selectizeInput(inputId = "AdminSelect_et1",
                         label = "Select Administrative Region(s)",
                         choices = unique(final$admin1),
                         multiple = TRUE,
                         options = list(maxItems = 18, placeholder = 'Enter Admin Region',
                                        onInitialize = I('function() { this.setValue(""); }'))
          ),
          hr(),
          sliderInput(inputId = "YearSlider_et1", 
                      label = "Years:", 
                      min = 2010, 
                      max = 2023,
                      value = c(2010, 2023))
      )
    )
  )
)

#==========================================================  
# ExploreTrendrow2 ---
#==========================================================  
ExploreTrendrow2 <-  fluidRow(
  box(
    title = "Annual Calendar",
    status = "danger",
    solidHeader = TRUE, 
    collapsible = TRUE,
    width = 12,  # Use full width
    column(8,
           box(
             width = 12,  # Use full width
             withSpinner(plotlyOutput("calendar_et1", height = "400px"))
           )
    ),
    column(
      4,
      
      box(title = "Desired Characteristics",
          status = "info",
          solidHeader = FALSE, 
          width = 12,  # Use full width
          helpText("Filter options are applicable to calendar"),
          selectInput(inputId = "YearSelect_et2", 
                      label = "Year:", 
                      choices = unique(final$year), 
                      # selected = 2023,
          ),
          hr(),
          radioButtons(inputId = "InciFata_et2",
                       label = "Display",
                       choices = c("No. of Incidents" = "Total_Incidents", "No. of Fatalities" = "Total_Fatalities"),
                       selected = "No. of Fatalities")
      )
    )
  )
)


#==========================================================  
# ExploreDistributionrow1 ---
#==========================================================  
ExploreDistributionrow1 <-  fluidRow(
  column(4
  ),
  column(6,
         sliderInput(inputId = "YearSlider_ed1", 
                     label = "Years:", 
                     min = 2010, 
                     max = 2023,
                     value = c(2010, 2023))
  ),
  column(4
  )
)

#==========================================================  
# ExploreDistributionrow2 ---
#========================================================== 
ExploreDistributionrow2 <- fluidRow(
  column(6,
         box(
           title = "Distribution of armed conflicts in Myanmar based on Sub-national Administrative Region 1",
           status = "danger",
           solidHeader = TRUE, 
           width = 12,
           height = 700,
           withSpinner(plotlyOutput("box_ed1", height = "600px"))  
         )),
  column(6,
         box(
           title = "Distribution of armed conflicts in Myanmar based on Events",
           status = "danger",
           solidHeader = TRUE, 
           width = 12,
           height = 700,
           withSpinner(plotlyOutput("box_ed2", height = "600px"))  # Width is automatically 100% of the box
         ))
)

#==========================================================  
# ExploreDistributionrow3 ---
#========================================================== 
ExploreDistributionrow3 <- fluidRow(
  column(12,
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "justify",
             textOutput("distributiontext") 
         ))
)

#==========================================================  
# ExploreNetworkrow1 ---
#==========================================================  
ExploreNetworkrow1 <-  fluidRow(
  box(
    title = "Network Relationship Among Actors in Myanmar",
    status = "danger",
    solidHeader = TRUE, 
    width = 12,  # Use full width
    column(9,
           box(width = NULL,
               align = "center",
               withSpinner(plotOutput("network_en1", height = "600px", width = "100%"))  # Set map width to 100% of box
           )
    ),
    column(3,
           box(title = "Desired Characteristics",
               status = "info",
               solidHeader = FALSE, 
               width = NULL, 
               selectizeInput(inputId = "EventSelect_en1",
                              label = "Select Event(s)",
                              choices = unique(final$event_type),
                              multiple = TRUE,
                              options = list(maxItems = 6, placeholder = 'Enter event type',
                                             onInitialize = I('function() { this.setValue(""); }'))
               ),
               hr(),
               selectizeInput(inputId = "AdminSelect_en1",
                              label = "Select Administrative Region(s)",
                              choices = unique(final$admin1),
                              multiple = TRUE,
                              options = list(maxItems = 18, placeholder = 'Enter Admin Region',
                                             onInitialize = I('function() { this.setValue(""); }'))
               ),
               hr(),
               selectInput(inputId = "YearSelect_en1", 
                           label = "Year:", 
                           choices = unique(final$year), 
                           # selected = 2023,
               ),
           ),
           box(title = "Chart Interpretation",
               status = "danger",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = NULL,  # Automatically adjust to full width
               align = "justify",
               textOutput("networktext") 
           )
    )
  ) 
)



#==========================================================  
# ExploreSummaryrow1 ---
#========================================================== 
ExploreSummaryrow1 <-  fluidRow(
  column(4,
         box(
           title = "Word Cloud - Incident Summary",
           status = "danger",
           solidHeader = TRUE, 
           collapsible = TRUE,
           width = NULL,
           height = 800,
           box(title = "Desired Characteristics",
               status = "info",
               solidHeader = FALSE, 
               width = NULL,
               helpText("Filter options are applicable to word cloud"),
               selectizeInput(inputId = "EventSelect_es1",
                              label = "Select Event(s)",
                              choices = unique(final$event_type),
                              multiple = TRUE,
                              options = list(maxItems = 6, placeholder = 'Enter event type',
                                             onInitialize = I('function() { this.setValue(""); }'))
               ),
               hr(),
               selectizeInput(inputId = "AdminSelect_es1",
                              label = "Select Region(s)",
                              choices = unique(final$admin1),
                              multiple = TRUE,
                              options = list(maxItems = 18, placeholder = 'Enter Admin Region',
                                             onInitialize = I('function() { this.setValue(""); }'))
               ),
               hr(),
               selectInput(inputId = "YearSelect_es1", 
                           label = "Year:", 
                           choices = unique(final$year), 
                           # selected = 2023,
               )),
           withSpinner(plotOutput("cloud_es1", height = "300px", width = "100%"))  # Set map width to 100% of box
           
         )),
  
  column(8,
         box(
           title = "Data Table",
           status = "danger",
           width = NULL,
           solidHeader = TRUE, 
           collapsible = TRUE,
           withSpinner(DT::dataTableOutput(outputId = "ExploreSummaryTable")),
           style = "height:750px; overflow-y: scroll;overflow-x: scroll;" # Set scrollbars
         )
         
  )
  
)

#==========================================================  
# ExploreSubTabs
#==========================================================  
ExploreSubTabs <- tabsetPanel(
  tabPanel("Overview", 
           ExploreOverviewrow1
  ),
  tabPanel("Geospatial Exploration", 
           ExploreGeospatialrow1
  ),
  tabPanel("Trends", 
           ExploreTrendrow1,
           ExploreTrendrow2
  ),  
  tabPanel("Distributions", 
           ExploreDistributionrow1,
           ExploreDistributionrow2,
           ExploreDistributionrow3
  ),
  tabPanel("Network Relationships", 
           ExploreNetworkrow1
  ),
  tabPanel("Incident Summary", 
           ExploreSummaryrow1
  )
)

# =============================    
########### EXPLORATORY - END
# =============================

#==========================================================  
# Cluster and Outlier - START
#==========================================================  


#==========================================================  
##Cluster and Outlier Analysis, 1st Tab
#==========================================================  

Cluster1 <- fluidRow(
  column(2,
         box(title = "Desired Characteristic",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Proportional Symbol Map",
                      selectInput(inputId = "Yearpointmap",
                                  label = "Year:",
                                  choices = seq(2010,2023),
                                  selected = 2021),
             selectInput("eventType1", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Riots" = "Riots",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Strategic developments" = "Strategic developments"),
                         selected = "Battles"),
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             textOutput("PointMaptext"))
             
         )
         
  ),
  column(4,
         box(title = "Location of Conflict Events",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "center",
             leafletOutput("Pointmap", height = "700px", width = "100%"))
  ),
  column(4,
         box(
           title = "Distribution of Conflict Events",
           status = "danger",
           solidHeader = TRUE,
           collapsible = TRUE,
           width = NULL,
           align = "center",
           tmapOutput("choropleth", height = "700px", width = "100%"))
         
  ),   
  column(2,
         box(title = "Desired Characteristic",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Choropleth Map",
                      selectInput(inputId = "Yearchoromap",
                                  label = "Year:",
                                  choices = seq(2010,2023),
                      selected = 2021),
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
                          choices = c("quantile", "equal", "jenks", "kmeans", "pretty"),
                          selected = "quantile",
                          inline = TRUE)),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             textOutput("ChoroMaptext"))
         
  )
  
)

)

#==========================================================  
##Cluster and Outlier Analysis, 2nd Tab
#==========================================================  


Cluster2 <- fluidRow(
  column(2,
         box(title = "Desired Characteristic",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Choropleth Map"),
             selectInput(inputId = "YearMoransI",
                         label = "Year:",
                         choices = seq(2010,2023),
                         selected = 2021),
             selectInput("eventType3", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Riots" = "Riots",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Strategic developments" = "Strategic developments"),
                         selected = "Battles")
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("MoransItext")
         )
  ),
  
  column(4,
         box(title = "Local Moran's I Statistic",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "center",
             tmapOutput("LocalMoranMap", height = "700px", width = "100%"))
  ),
  
  column(4,
         box(
           title = "Local Moran's I P-values",
           status = "danger",
           solidHeader = TRUE,
           collapsible = TRUE,
           width = NULL,
           align = "center",
           tmapOutput("LocalMoranPval", height = "700px", width = "100%"))
  ),
  
  column(12,
         box(
           title = "Local Moran's I Results - Data Table",
           status = "danger",
           solidHeader = TRUE,
           collapsible = TRUE,
           width = NULL,
           align = "center",
           dataTableOutput("localMoranDataTable")
         )
  )
  
)

#==========================================================  
##Cluster and Outlier Analysis, 3rd Tab
#==========================================================  

Cluster3 <- fluidRow(
  column(2,
         box(title = "Desired Characteristic",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Moran Scatterplot"),
             selectInput(inputId = "YearMoranScat",
                         label = "Year:",
                         choices = seq(2010,2023),
                         selected = 2021),
             selectInput("eventType4", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Riots" = "Riots",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Strategic developments" = "Strategic developments"),
                         selected = "Battles")
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("MoranScatText")
         )
  ),
  
  column(4,
      box(title = "Moran Scatter Plot"
      ,status = "danger"
      ,solidHeader = TRUE 
      ,collapsible = TRUE
      ,width = NULL
      ,align = "center"
      ,plotOutput("MoranScatter", height = "700px", width = "100%")) 
  ),
  column(4,
         box(
           title = "Local Indicator of Spacial Association (LISA)",
           status = "danger",
           solidHeader = TRUE,
           collapsible = TRUE,
           width = NULL,
           align = "center",
           tmapOutput("Lisa", height = "700px", width = "100%"))
         
  ),   
  column(2,
         box(title = "Desired Characteristic",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for LISA Map",
                      selectInput(inputId = "YearLisamap",
                                  label = "Year:",
                                  choices = seq(2010,2023),
                                  selected = 2021),
             selectInput("eventType5", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Riots" = "Riots",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Strategic developments" = "Strategic developments"),
                         selected = "Battles")),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             textOutput("LisaMapText"))
      
         )
  )    
  
)

#==========================================================  
#Cluster and Outlier Analysis tab ----END
#==========================================================  


#==========================================================  
##Hot and Cold Spot Analysis tab --- START
#==========================================================  

HotCold1 <- fluidRow(
  column(2,
         box(title = "Desired Characteristic",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Hot & Cold spot map"),
             selectInput(inputId = "YearHotCold",
                         label = "Year:",
                         choices = seq(2010,2023),
                         selected = 2021),
             selectInput("eventType6", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Riots" = "Riots",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Strategic developments" = "Strategic developments"),
                         selected = "Battles"),
             radioButtons("mapStyle2", "Classification Type:",
                          choices = c("quantile", "equal", "jenks", "kmeans", "pretty"),
                          selected = "pretty",
                          inline = TRUE),
             sliderInput("numClusters", "Number of Clusters, Adaptive weight matrix:", 
                         min = 4, max = 10, value = 8)
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("HotColdText")
         )
  ),
  column(4,
         box(title = "Adaptive Distance - Hot & Cold spots"
             ,status = "danger"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,width = NULL
             ,align = "center"
             ,tmapOutput("AdaptiveGimap", height = "700px", width = "100%"))
  ),
  column(6,
      box(
        title = "GI Statistics (Adaptive Distance) - Data Table",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = NULL,
        align = "center",
        dataTableOutput("AdaptiveGiStat")
      )
  )

)

#==========================================================  
##Hot and Cold Spot Analysis tab ----END
#==========================================================  

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
  
  # use theme
  use_theme(mytheme),
  
  
  # =============================
  # tabItems - All Pages
  # =============================
  
  tabItems(
    # 1st tab content
    tabItem(tabName = "Exploratory",
            ExploreSubTabs
    ),
    # 2nd tab content
    tabItem(tabName = "Cluster",
            
            ClusterSubTabs # add the sub tabs which was defined above
    ),
    #3rd tab content
    tabItem(tabName = "HotCold",
            HotCold1
            # content
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
  
  
  ##Data subset for Choropleth Maps (Admin 2)
  #====================================================
  Data2 <- ACLED_MMR %>%
    group_by(year, admin2, event_type, sub_event_type) %>%
    summarise(Incidents = n(),
              Fatalities = sum(fatalities, na.rm = TRUE)) %>%
    
    ungroup()
  
  ##Spacial join between shape file and attribute file (admin 2)
  #=======================================================
  ACLED_MMR_admin2 <- left_join(mmr_shp_mimu_2, Data2,
                                by = c("DT" = "admin2"))
  
  ACLED_MMR_admin2 <- ACLED_MMR_admin2 %>%
                      select(-OBJECTID, -ST, -ST_PCODE)
  
  
  # Convert conflict data to an sf object
  #===================================================
  conflict_sf <- sf::st_as_sf(ACLED_MMR, coords = c("longitude", "latitude"), crs = 4326)
  
  #Data subset for Local Morans statistics
  #====================================================
  
  Events2 <- ACLED_MMR %>%
    group_by(year, admin2, event_type) %>%
    summarise(Incidents = n(),
              Fatalities = sum(fatalities, na.rm = TRUE)) %>%
    
    ungroup()
  
  Events_admin2 <- left_join(mmr_shp_mimu_2, Events2,
                             by = c("DT" = "admin2"))
  
  Events_admin2 <- Events_admin2 %>%
                  select(-OBJECTID, -ST, -ST_PCODE)
  
  
  
  # =============================    
  # START of EXPLORATORY Module
  # =============================
  
  #==========================================================
  # Explore Point Map ---
  #==========================================================    
  final_explorespatial <- reactive({
    
    req(input$YearSlider_eo1, input$EventSelect_eo1, input$AdminSelect_eo1)
    # Filter 'final' dataset based on user inputs
    filtered_data <- final %>%
      filter(event_type %in% input$EventSelect_eo1,
             admin1 %in% input$AdminSelect_eo1,
             year %in% range(input$YearSlider_eo1))
    return(filtered_data)
  })
  
  # Render Point Map --- 
  output$emap_eo1 <- renderLeaflet({
    # Use the filtered dataset
    incident_pts <- final_explorespatial() %>%
      filter(fatalities > 0)  
    
    cof <- colorFactor(c("#ff7e8a", "#394938", "#ffa500", 
                         "#0092ff", "#741b47", "#60dcb5"), 
                       domain=c("Battles", "Explosions/ Remote violence", 
                                "Protests", "Riots",
                                "Strategic developments", 
                                "Violence against civilians"))
    
    leaflet() %>% 
      addTiles('https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
      setView(100, 20, zoom = 5) %>%
      addCircles(data=incident_pts, lat= ~latitude, lng = ~longitude, 
                 color = ~cof(event_type), 
                 fillColor = ~cof(event_type),  # Use fillColor for consistency or adjust as necessary
                 popup = paste( "<strong>", incident_pts$event_date, "</strong>", 
                                "<br><strong>Country: </strong>",
                                incident_pts$country, 
                                "<br><strong>Sub-national Admin Region: </strong>",
                                incident_pts$admin1, "/", 
                                incident_pts$admin2, "/", 
                                incident_pts$admin3, 
                                "<br><strong>Event type: </strong>",
                                incident_pts$event_type, 
                                "<br><strong>Sub-event type: </strong>", 
                                incident_pts$sub_event_type, 
                                "<br><strong>Summary: </strong>", 
                                incident_pts$notes,
                                "<br><strong>Total Fatalities: </strong>",
                                incident_pts$fatalities))
  })
  
  observeEvent(input$resetButton, {
    updateSelectizeInput(session, "EventSelect_eo1", selected = character(0))
    updateSelectizeInput(session, "AdminSelect_eo1", selected = character(0))
    updateSliderInput(session, "YearSlider_eo1", value = c(2010, 2023))
  })
  
  output$abouttext <- renderText({ 
    "Armed conflicts due to political violence and coordinated attacks targeting innocent civilians, have been on the rise globally. 
      This threatens the public at both physical and psychological levels. 
      this visualisation dashboard helps: 
      (1) discover armed conflicts trends and 
      (2) conceptualise armed conflict spaces in Myanmar." 
  })
  
  #==========================================================
  # Explore Statistical Distribution Map ---
  #==========================================================   
  
  boxbreaks <- function(v, mult=1.5) {
    qv <- unname(quantile(v, na.rm = TRUE))
    iqr <- qv[4] - qv[2]
    upfence <- qv[4] + mult * iqr
    lofence <- qv[2] - mult * iqr
    bb <- vector(mode="numeric", length=7)
    if (lofence < qv[1]) { 
      bb[1] <- lofence
      bb[2] <- floor(qv[1])
    } else {
      bb[2] <- lofence
      bb[1] <- qv[1]
    }
    if (upfence > qv[5]) { 
      bb[7] <- upfence
      bb[6] <- ceiling(qv[5])
    } else {
      bb[6] <- upfence
      bb[7] <- qv[5]
    }
    bb[3:5] <- qv[2:4]
    return(bb)
  }
  
  # Function to fetch variable data
  get.var <- function(vname, df) {
    v <- df[[vname]]
    return(v)
  } 
  
  # Function to create a box map
  boxmap <- function(vnam, df, legtitle=NA, mtitle="Box Map", mult=1.5) {
    var <- get.var(vnam, df)
    bb <- boxbreaks(var, mult)
    tm <- tm_shape(df) +
      tm_polygons(col = vnam, title = legtitle, breaks = bb, palette = "Reds", 
                  labels = c("Lower Outlier", "< 25%", "25% - 50%", "50% - 75%", "> 75%", "Upper Outlier")) +
      tm_layout(main.title = mtitle, main.title.position = "center", legend.outside = TRUE)
    return(tm)
  }
  
  # Render the map based on user input
  output$emap_eg1 <- renderTmap({
    selected_variable <- input$InciFata_eg1
    map_title <- if(selected_variable == "Total_Incidents") {"No. of Incidents"} else {"No. of Fatalities"}
    
    boxmap(selected_variable, mapping_rates, legtitle = map_title, mtitle = paste0("Box Map of ", map_title))
  })
  
  
  
  ########################################
  output$statisticaltext <- renderText({ 
    "A boxmap is a spatial representation akin to boxplot and histogram, 
    which is useful to detect outliers and visualise distribution of variables across Myanmar's
    different geographical subnational administrative region 2." 
  })
  
  #==========================================================
  # Explore Spatial Distribution Map ---
  #==========================================================  
  mapping_rates_exploredist <- reactive({
    req(input$InciFata_eg2)
    req(input$YearSelect_eg2)
    req(input$ClassSelect_eg2)
    req(input$ClassificationSelect_eg2)
    
    
    filtered_emap_eg2 <- mapping_rates %>%
      filter(year == input$YearSelect_eg2) 
    
    # Check if the filtered data is empty to avoid errors
    if(nrow(filtered_emap_eg2) == 0) {
      return(NULL)  # Return NULL if there's no data to avoid plotting errors
    }
    
    # Determine the column to use for filling based on user selection
    fill_column <- ifelse(input$InciFata_eg2 == "Total_Fatalities", "Total_Fatalities", "Total_Incidents")
    
    
    return(list(filtered_data = filtered_emap_eg2, fill_column = fill_column))
  })
  
  
  # Render Spatial Distribution Map --- 
  output$emap_eg2 <- renderTmap({
    mapping_data <- mapping_rates_exploredist()
    if(is.null(mapping_data$filtered_data)) {
      return(NULL)  # Return NULL if there's no data to avoid plotting errors
    }
    tmap_obj2 <- tm_shape(mapping_data$filtered_data) +
      tm_fill(mapping_data$fill_column, 
              n = input$ClassSelect_eg2,
              style = input$ClassificationSelect_eg2,
              palette = "Reds", 
              legend.hist = TRUE, 
              legend.is.portrait = TRUE, 
              legend.hist.z = 0.1) +
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_layout(main.title = paste("Distribution of Armed Conflict Incidents in Myanmar", input$ClassificationSelect_eg2, "classification"),
                title = "",
                main.title.size = 1,
                legend.height = 0.60,
                legend.width = 5.0,
                legend.outside = FALSE,
                legend.position = c("left", "bottom"))
    return(tmap_obj2)
  })
  
  ########################################  
  output$spatialtext <- renderText({ 
    "A choropleth map visualises spatial pattern or distribution across Myanmar's
    different geographical subnational administrative region 2, which can be further customised 
    by adjusting the desired data classification type and number of classes (or data ranges)." 
  })
  
  #==========================================================  
  # Explore Trends ---
  #==========================================================  
  final_exploretime <- reactive({
    req(input$YearSlider_et1)
    req(input$EventSelect_et1)
    req(input$AdminSelect_et1)
    filter(final, event_type %in% input$EventSelect_et1) %>%
      filter(final$admin1 %in% input$AdminSelect_et1) %>%
      filter(final$year %in% input$YearSlider_et1)
    
  })
  
  calendar <- final %>%
    filter(fatalities > 0) %>%
    group_by(year, event_date, admin1) %>%
    mutate(
      wkday = wday(event_date),
      day = mday(event_date),
      month = factor(months(event_date), levels = rev(month.name)),
      week = isoweek(event_date),
      year_month = format(zoo::as.yearmon(event_date), "%y-%m")
    ) %>%
    ungroup()
  
  
  # Render Line Chart --- 
  output$line_et1 <- renderHighchart({
    
    year_fata <- calendar %>%
      filter(fatalities > 0) %>%
      group_by(year_month) %>%
      select(year, month, year_month, fatalities) %>%
      summarise(Total_Fatalities = sum(fatalities),
                Total_Incidents = n()) %>%
      ungroup()
    
    hc_plot1 <-  highchart() %>% 
      hc_add_series(year_fata, hcaes(x = year_month, y = Total_Fatalities), type = "line", 
                    name = "Total Fatalities", color = "lightcoral") %>%
      hc_add_series(year_fata, hcaes(x = year_month, y = Total_Incidents), type = "line", 
                    name = "Total Incidents", color = "black") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", 
                 backgroundColor = "#FCFFC5",
                 borderWidth = 5,
                 pointFormat = "<b>20{point.year_month}</b> 
                                 <br> Fatalities: <b>{point.Total_Fatalities}</b>
                                 <br> Incidents: <b>{point.Total_Incidents}</b>"
      ) %>%
      hc_title(text = "Armed Conflict Over The Years") %>% 
      hc_subtitle(text = "2010 to 2023") %>%
      hc_xAxis(title = list(text = "2010-2023"), labels = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Frequency"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "lightcoral", width = 1, dashStyle = "Dash",
                 value = mean(year_fata$Total_Fatalities),
                 label = list(text = paste("Average Monthly Fatalities:", round(mean(year_fata$Total_Fatalities))),
                              style = list(color = 'lightcoral', fontSize = 20))))) 
    hc_plot1
    
  })
  
  ########################################
  final_explorecalendar <- reactive({
    req(input$InciFata_et2)
    req(input$YearSelect_et2)
    filter(final, year == input$YearSelect_et2)
    
  })
  
  # Render calendar --- 
  # selected years
  # years <- c(2010:2023)
  years <- 2023
  
  cal_conflict <- calendar %>%
    group_by(year, day, month) %>%
    filter(year == years) %>%
    summarise(Total_Fatalities = sum(fatalities),
              Total_Incidents = n()) %>%
    ungroup()
  
  output$calendar_et1 <- renderPlotly({
    # tooltip
    tooltip_heat <- paste("<b>", cal_conflict$day, " ", cal_conflict$month, " ",
                          cal_conflict$year, "</b>", 
                          "\nFatalities : ", cal_conflict$Total_Fatalities,
                          "\nIncidents : ", cal_conflict$Total_Incidents)
    
    heat <- ggplot(cal_conflict, aes(x = day, y = month, fill = Total_Fatalities)) + 
      geom_tile(color = "white", size = 1, aes(text = tooltip_heat)) + 
      theme_tufte(base_family = "Helvetica") + 
      coord_equal() +
      scale_fill_gradient(name = "Total Fatalities", low = "#fff2f4", 
                          high = "lightcoral") +
      labs(x = "Days of Month", 
           y = "", 
           title = paste("Fatalities due to Armed Conflicts in Myanmar in ", years),
           caption = "Data Source: ACLED (2023)") +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 7),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6),
            legend.position = "top") +
      scale_x_continuous(breaks = seq(min(cal_conflict$day), max(cal_conflict$day), 
                                      by = 2),
                         labels = seq(min(cal_conflict$day), max(cal_conflict$day), 
                                      by = 2))
    heat_plotly <- ggplotly(heat, tooltip = "text")
    
    heat_plotly
  })
  
  ########################################
  output$trendtext <- renderText({ 
    "This space is saved for interpretation of charts." 
  })
  
  #==========================================================  
  # Explore Distributions ---
  #==========================================================  
  final_exploretime <- reactive({
    req(input$YearSlider_ed1)
    filter(final, year %in% input$YearSlider_ed1)
    
  })
  
  box_pts <- final %>%
    filter(fatalities >0)
  
  # Render Boxplot1 --- 
  
  output$box_ed1 <- renderPlotly({
    
    box1 <- ggplot(box_pts, aes(x = forcats::fct_infreq(admin1), y = event_date, 
                                color = factor(admin1))) +
      
      geom_boxplot(width = .2, color = "#000000", fill = NA, size = .5, 
                   outlier.shape = NA, position = position_nudge(.25)) +
      geom_sina(method = "density", alpha = .3) +
      coord_flip()+
      theme_minimal() +
      theme(legend.position = "none", 
            plot.title.position = "plot") +
      labs(title = "Frequency of Conflict Has Increased Over Time in the Largest Sub-national Administrative Region in Myanmar", subtitle = "Year 2010 to Year 2023") +
      labs(y = "Year (2010-2023)",
           x = "Adminstrative Region 1", 
           caption = "Data Source: ACLED (2023)")
    
    ggplotly(box1)
  })
  
  # Render Boxplot2 --- 
  output$box_ed2 <- renderPlotly({
    # tooltip_box <- paste("<b>", final$date, "</b>", "\nFatalities : ", final$fatalities)
    
    box2 <- ggplot(box_pts, aes(x = forcats::fct_infreq(event_type), y = event_date, 
                                color = factor(event_type), fill = factor(event_type))) +
      geom_sina(method = "density", alpha = .3) +
      geom_boxplot(width = .2, color = "#000000", fill = NA, size = .5, 
                   outlier.shape = NA, position = position_nudge(.25)) +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none", 
            plot.title.position = "plot") +
      labs(title = "Battles, Explosion & Violence against Civilian Have Been Happening in Myanmar Over Time\n With More Occurrence Happening From Year 2020 Onwards", subtitle = "Year 2010 to Year 2023") +
      labs(y = "Year (2010-2023)",
           x = "Event Types", 
           caption = "Data Source: ACLED (2023)")
    
    ggplotly(box2)
  })
  
  ########################################
  output$distributiontext <- renderText({ 
    "A jittered-cum-boxplot shows the distribution of the armed conflicts incidents in Myanamr and how it has changed over the years.
    It helps to determine how spread out the incidents were across the years, 
    how it started/ ended in certain years or high concentrations in certain years. 
    "  
  })
  
  #==========================================================
  # Explore Network Relationship ---
  #==========================================================    
  final_explorenetwork <- reactive({
    req(input$EventSelect_en1)
    req(input$AdminSelect_en1)
    req(input$YearSelect_en1)
    filter(final, event_type %in% input$EventSelect_en1) %>%
      filter(final$admin1 %in% input$AdminSelect_en1) %>%
      filter(final$year == input$YearSelect_en1)
    
  })
  
  # select year
  years <- 2023  # input$YearSelect_en1
  
  # calculate frequency
  conflict_count <- final %>% 
    group_by(actor1) %>% 
    filter (year == years) %>%
    summarise(frequency = n()) %>%
    arrange(desc(frequency)) 
  
  conflict_count2 <- final %>% 
    group_by(actor2) %>% 
    filter (year == years) %>%
    summarise(frequency = n()) %>%
    arrange(desc(frequency))
  
  colnames(conflict_count) = c("actor","Freq")
  colnames(conflict_count2) = c("actor","Freq")
  
  # combine both actor 1 & 2
  final_conflict_count = rbind(conflict_count,conflict_count2)
  
  final_conflict_count2 = final_conflict_count %>%
    group_by(actor) %>%
    summarise(FrequencyConflicts = sum(Freq)) %>%
    arrange(desc(FrequencyConflicts))
  
  # trim actor/ assoc_actor
  final$actor1 = trimws(str_replace(final$actor1, "[Õ]", ""))
  final$actor2  = trimws(str_replace(final$actor2, "[Õ]", ""))
  final$assoc_actor_1 = trimws(str_replace(final$assoc_actor_1, "[Õ]", ""))
  final$assoc_actor_2 = trimws(str_replace(final$assoc_actor_2, "[Õ]", ""))
  
  assoc1 = final %>%
    filter(!is.na(actor1)) %>%
    filter(!is.na(assoc_actor_1)) %>%
    filter(!(actor1 == "")) %>%
    filter(!(assoc_actor_1 == "")) %>%
    select(actor1,assoc_actor_1)
  
  assoc2 = final %>%
    filter(!is.na(actor2)) %>%
    filter(!is.na(assoc_actor_2)) %>%
    filter(!(actor2 == "")) %>%
    filter(!(assoc_actor_2 == "")) %>%
    select(actor2,assoc_actor_2)
  
  colnames(assoc1) = c("actor","assoc_actor") 
  colnames(assoc2) = c("actor","assoc_actor") 
  
  # combine both assoc 1 & 2
  combined1 = rbind(assoc1,assoc2)
  
  combined2 = combined1 %>%
    group_by(actor,assoc_actor) %>%
    tally(sort = TRUE) 
  
  
  final_conflict_count3 = trimws(final_conflict_count2$actor)
  
  combined3 = combined2 %>%
    filter(actor %in% final_conflict_count3)
  
  
  # network graph
  viz_actors_12 <- function(actors_12) {
    set.seed(1234)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    actors_12 %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "lightcoral") +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE,
                     point.padding = unit(0.5, "lines")) +
      theme_void()
  }
  
  # Render network --- 
  output$network_en1 <- renderPlot({
    combined3 %>%
      filter(n >= 300) %>%
      viz_actors_12
  })
  
  
  ########################################
  output$networktext <- renderText({ 
    "A network graph schematically depict the nodes and connections amongst the actors and their associations involved in the armed conflicts in Myanmar.
     The line thickness indicates the strength of connections (e.g. thicker line denotes stronger connection) among actors in the network." 
  })
  
  #==========================================================
  # END of Exploratory Module
  #==========================================================
  
  #==========================================================
  # START of Cluster & Outlier Analysis Module
  #==========================================================   
  
  
  #creating the Plots- for Cluster & Outlier Analysis 
  
  #==========================================================
  # Proportional Symbol map in Cluster 1 UI
  #==========================================================   
  
  # Reactive expression to filter data based on input from UI
  PointMapYears <- reactive({
    filtered_data <- conflict_sf %>% 
      filter(event_type == input$eventType1, year %in% input$Yearpointmap)
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
  
  output$PointMaptext <- renderText({ 
    "Proportional symbol maps are a class of maps that use the visual variable of size 
    to represent differences in the magnitude of a discrete, 
    abruptly changing phenomenon. Here, the number of fatalities are mapped to the sizes of the circles" 
  })
  
  #==========================================================
  # choropleth in Cluster 1 UI
  #==========================================================   
  
  
  #tmap_mode("plot")
  
  output$choropleth <- renderTmap({
    # Dynamically filter data based on user input
    data_filtered <- ACLED_MMR_admin2 %>%
      filter(year == input$Yearchoromap, event_type == input$eventType2) 
      
    
    # Check if the filtered data is empty to avoid errors
    if(nrow(data_filtered) == 0) {
      return(NULL)  # Return NULL if there's no data to avoid plotting errors
    }
    
    # Determine the column to use for filling based on user selection
    fillColumn <- ifelse(input$metricType == "Fatalities", "Fatalities", "Incidents")
    
    tm_map <- tm_shape(mmr_shp_mimu_2) +
      tm_borders() +  # Draws borders for all regions
      tm_fill(col = "white", alpha = 0.5, title = "Background") +
      
      tm_shape(data_filtered) +
      tm_fill(fillColumn,
              n = 5,
              style = input$mapStyle,
              palette = "Reds")+
      tm_borders(alpha = 0.5)
    
    tm_map
  })
  
  output$ChoroMaptext <- renderText({ 
    "A choropleth map visualises spatial pattern or distribution across Myanmar's
    different administrative region 2. This can be further customised 
    by adjusting the year, event type, count of Incidents or Fatalities and data classification type." 
  })
  
  
  #==========================================================
  # Local Morans's I Map in Cluster 2
  #==========================================================   
  
  
  
  localMIResults <- reactive({
    # Filter the data based on the user's selection
    filteredData <- Events_admin2 %>%
    filter(year == input$YearMoransI, event_type == input$eventType3) 
    
    
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
  output$LocalMoranMap <- renderTmap({
    df <- localMIResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    # Map creation using tmap
    localMI_map <- tm_shape(mmr_shp_mimu_2) +
      tm_borders() +  # Draws borders for all regions
      tm_fill(col = "white", alpha = 0.5, title = "Background") +
      
      tm_shape(df) +
      tm_fill(col = "Ii", style = "pretty", palette = "RdBu", title = "Local Moran's I") +
      tm_borders(alpha = 0.5)
    
    localMI_map
  })
  
  #==========================================================
  # Local Morans's I P-values Map in Cluster 2 
  #==========================================================  
  
  output$LocalMoranPval <- renderTmap({
    df <- localMIResults()
    
    # Exit if there's no data to plot
    if(is.null(df) || nrow(df) == 0) {
      return()
    }
    
    # Create the choropleth map for p-values
    pvalue_map <- tm_shape(mmr_shp_mimu_2) +
      tm_borders() +  # Draws borders for all regions
      tm_fill(col = "white", alpha = 0.5, title = "Background") +
      
      tm_shape(df) +
      tm_fill(col = "Pr.z....E.Ii..", 
              breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
              palette = "-Blues", 
              title = "Local Moran's I p-values") +
      tm_borders(alpha = 0.5)
    
    pvalue_map
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
    "Local Moran's I, a component of LISA, is a spatial statistic that detects and quantifies 
    spatial clustering or dispersion within a given geographic area.
    Unlike global measures like Moran's I, it assesses spatial patterns at a local level, 
    determining if features form significant clusters (high-high or low-low) or outliers (high-low or low-high) 
    compared to neighboring features.In our analysis, we're examining if certain areas (admin region 2) exhibit higher or lower incident rates of a specific 
    event type than expected by chance alone, indicating deviations from a random spatial distribution.
    Here, we are mapping Ii: the local Moran's I statistics, and the p-value of the Local Moran's I statistic" 
  })
  
  
  #==========================================================
  # Moran Scatter plot in Cluster 3
  #==========================================================  

  
  output$MoranScatter <- renderPlot({
    # Retrieve filtered data based on input selections for event type and year
    filteredData1 <- Events_admin2 %>%
      filter(year == input$YearMoranScat, event_type == input$eventType4)
    
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
    
    plotTitle <- paste("Moran Scatterplot for", input$eventType4, "in", input$YearMoranScat)
    
    # Create the Moran scatterplot
    nci <- moran.plot(standardizedIncidents, rswm_q,
                      labels = as.character(filteredData1$DT),
                      xlab = "Standardized Incidents",
                      ylab = "Spatially Lagged Incidents",
                      main = plotTitle)
    
    
  })
  
  output$MoranScatText <- renderText({ 
    "The Moran scatterplot is divided into four areas, with each quadrant corresponding 
    with one of four categories: (1) High-High (HH) in the top-right quadrant; (2) High-Low (HL) 
    in the bottom right quadrant; (3) Low-High (LH) in the top-left quadrant; 
    (4) Low- Low (LL) in the bottom left quadrant. The top right corner belongs to areas that have high incidents of events and are surrounded by other areas 
    that have higher than the average level/number of battles This is the high-high locations." 
  })
  
  
  #==========================================================
  # LISA Map in Cluster 3
  #==========================================================  

  
  lisaCalculation <- reactive({
    LisaData <- Events_admin2 %>%
      filter(year == input$YearLisamap, event_type == input$eventType5) 
    
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
  output$Lisa <- renderTmap({
    df <- lisaCalculation()
    if(is.null(df)) return()
    
    colors <- c("lightyellow", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
    
    # Generate the LISA map
    lisamap <- tm_shape(mmr_shp_mimu_2) +
      tm_borders() +  # Draws borders for all regions
      tm_fill(col = "white", alpha = 0.5, title = "Background") +
      
      tm_shape(df) +
      tm_fill(col = "quadrant", 
              style = "cat", 
              palette = colors, 
              title = "LISA Clusters") +
      tm_borders(alpha=0.5)
    
    lisamap
  })
  
  output$LisaMapText <- renderText({ 
    "In addition to the four categories described in the Moran Scatterplot, 
    the LISA Map includes an additional category: 
    (5) Insignificant: where there are no spatial autocorrelation or clusters where event types have occurred.
    The Lisa Cluster map shows the specific parts of the country which are categorized 
    as statistically significant or insignificant." 
  })
  
  
  
  #==========================================================
  # END of Cluster & Outlier Analysis Module
  #==========================================================
  
  
  #==========================================================
  # START of Hot & Cold spot Analysis Module
  #==========================================================
  
  
  #==========================================================
  # Adaptive GI Map and Table
  #==========================================================
  
  
  AdaptiveGiData <- reactive({
    filtered_data2 <- Events_admin2 %>%
      filter(year == input$YearHotCold, event_type == input$eventType6)
    
    # Calculate centroids for filtered data
    longitude <- map_dbl(filtered_data2$geometry, ~st_centroid(.x)[[1]])
    latitude <- map_dbl(filtered_data2$geometry, ~st_centroid(.x)[[2]])
    coords <- cbind(longitude, latitude)
    
    # Adaptive distance weight matrix
    knn <- knn2nb(knearneigh(coords, k=input$numClusters))
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
  
  output$AdaptiveGimap <- renderTmap({
    df <- AdaptiveGiData()
    
    # Exit if there's no data to plot
    if(is.null(df) || nrow(df) == 0) {
      return()
    }
    
    # Create the choropleth map for GI stats
    Gi_map <- tm_shape(mmr_shp_mimu_2) +
      tm_borders() +  # Draws borders for all regions
      tm_fill(col = "white", alpha = 0.5, title = "Background") +
      
      tm_shape(df) +
      tm_fill(col = "gstat_adaptive", 
              style = input$mapStyle2, 
              palette = "-RdBu", 
              title = "Adaptive Distance\nlocal Gi") +
      tm_borders(alpha = 0.5)
    
    Gi_map
  })
  
  
  output$AdaptiveGiStat <- renderDataTable({
    data_with_gi <- AdaptiveGiData()  # Reactive function for data preparation
    if(is.null(data_with_gi)) {
      return(data.frame())  # Return an empty data frame if data is null
    }
    return(data_with_gi)
  })
  
  output$HotColdText <- renderText({ 
    "Unlike the previous section utilizing the Local Moran’s I statistics, 
    here we will be utilizing the Getis and Ord’s G statistics. Getis-Ord's G focuses 
    on detecting hot spots (areas with high values) and cold spots (areas with low values) 
    within a defined proximity (distance).
    
    Getis-Ord's G assesses the statistical significance of clustering by considering both the values of features 
    and the values of their neighboring features. High positive G values indicate hot spots—areas where high values cluster together—
    while low negative G values indicate cold spots—areas where low values cluster together." 
  })
  
  
  #==========================================================
  # END of Hot & Cold spot Analysis Module
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
